library(RCurl)
library(parallel)
library(here)
library(dplyr)
library(igraph)
library(tidyr)
library(purrr)
library(geeM)
library(writexl)

options(mc.cores = detectCores() - 1)

# Declare location of current script
i_am("R/analyses.R")

# ------------------------------- REDCap data ------------------------------- #

# API
uri   <- "https://redcap.unisante.ch/api/"

# Token
token <- here("misc", "redcap_personal_data.token")
token <- readChar(token, file.info(token)$size - 1)

# Import personal data with the API (RCurl)
pdata <- read.csv(text = postForm(
  uri = uri,
  token = token,
  content = "record",
  format = "csv"
))
rm(uri, token)

# ------------------------------ Participants ------------------------------- #

# Select variables from personal data
obs <- pdata %>%
  filter(cat_e_participant_type %in% 1:2) %>%
  select(id = cat_ec_participant_sid, indexCase = cat_e_participant_type,
         fid = cat_s_familycode, armExp = cat_s_arm, positive = cat_l_result,
         sexM = cat_dp_sexe_v2, firstName = cat_dp_prenom_v2) %>%
  mutate(across(c(indexCase, armExp, positive, sexM), function(x) 2L - x),
         participate = 1L)

# # Sent SMS (by family)
# sms <- pdata %>%
#   select(id = cat_ec_participant_sid, sms = cat_r_sms_done) %>%
#   left_join(
#     pdata %>%
#       select(id = cat_ec_participant_sid, fid = cat_s_familycode) %>%
#       filter(!is.na(id) & id != "" & !is.na(fid) & fid != ""),
#    by = "id"
#   ) %>%
#   group_by(fid) %>%
#   summarise(n_sms = sum(sms, na.rm = TRUE))

# # Define SMS arm
# obs <- obs %>%
#   left_join(sms, by = "fid") %>%
#   mutate(armSMS = as.integer(n_sms > 0)) %>%
#   select(!n_sms)

# ---------------------- Graph data of family members ----------------------- #

files <- sapply(list.dirs(here("data-raw", "matrices_20230123"),
                          recursive = FALSE),
                list.files, full.names = TRUE)
gdata <- mclapply(files, function(f) {
  # Family ID
  fid <- sub("\\.csv$", "", basename(f))
  # Adjacency matrices
  adj_mat <- as.matrix(read.csv(f, row.names = 1, check.names = FALSE))
  adj_mat[is.na(adj_mat)] <- ""
  adj_mat <- trimws(adj_mat)
  # rownames(adj_mat) <- substr(rownames(adj_mat), 1, 10)
  # colnames(adj_mat) <- substr(colnames(adj_mat), 1, 10)
  # Nodes
  nodes <- obs %>%
    right_join(data.frame(id = unique(rownames(adj_mat))), by = "id") %>%
    mutate(across(c(indexCase, participate), replace_na, 0L))
  nodes[is.na(nodes$armExp), "armExp"] <-
    if (all(is.na(nodes$armExp))) NA else na.omit(nodes$armExp)[1]
  # Check fid
  fid_ok <- all(na.omit(nodes$fid) %in% fid)
  # Edges
  edges <- map_dfr(rownames(adj_mat), function(x) {
    z <- adj_mat[x, ][adj_mat[x, ] != ""]
    if (length(z) == 0) {
      data.frame(from = character(0), to = character(0), label = character(0))
    } else {
      data.frame(from = x, to = names(z), label = unname(z))
    }
  })
  # We only keep the edges which go away of the index case (desymmetrization)
  if (nrow(edges) > 0 & sum(nodes$indexCase) == 1) {
    g <- graph_from_edgelist(as.matrix(edges[, c("from", "to"), drop = FALSE]))
    d <- distances(g)[nodes[nodes$indexCase == 1, "id"], ]
    edges <- edges[d[edges[, "from"]] < d[edges[, "to"]], , drop = FALSE]
  }
  # Complete missing sex values
  sex <- edges %>%
    select(id = to, sexM.2 = label) %>%
    mutate(sexM.2 = recode(sexM.2, Father = 1L, Mother = 0L, Brother = 1L,
                           Sister = 0L, Son = 1L, Daughter = 0L))
  nodes <- nodes %>%
    left_join(sex, by = "id") %>%
    mutate(sexM = if_else(is.na(sexM), sexM.2, sexM)) %>%
    select(!sexM.2)
  # Not eligible relatives
  # * If one parent of a positive case is positive, then the other parent is
  #   not eligible
  # * Relatives of a negative case, who is not an index case, are not eligible
  nonEligibleParents <- edges %>%
    filter(label %in% c("Father", "Mother")) %>%
    rename(id = from, parentId = to) %>%
    group_by(id) %>%
    mutate(n_parents = n()) %>%
    left_join(select(nodes, id, positiveParent = positive),
              by = c("parentId" = "id")) %>%
    mutate(
      anyPositiveParent = any(positiveParent == 1, na.rm = TRUE),
      nonEligibleParent = anyPositiveParent & !(positiveParent %in% 1)
    ) %>%
    ungroup() %>%
    select(id = parentId, nonEligibleParent)
  nonEligibleRelatives <- edges %>%
    rename(id = from, relativeId = to) %>%
    left_join(select(nodes, id, indexCase, positive), by = "id") %>%
    mutate(nonEligibleRelative = indexCase == 0 & positive == 0) %>%
    select(id = relativeId, nonEligibleRelative) %>%
    full_join(nonEligibleParents, by = "id") %>%
    mutate(nonEligibleParent = replace_na(nonEligibleParent, FALSE),
           nonEligible = nonEligibleParent | nonEligibleRelative) %>%
    select(id, nonEligible)
  nodes <- nodes %>%
    left_join(nonEligibleRelatives, by = "id") %>%
    mutate(nonEligible = as.integer(replace_na(nonEligible, FALSE)))
  list(nodes = nodes, edges = edges, adj_mat = adj_mat, fid = fid,
       fid_ok = fid_ok)
})
names(gdata) <- sapply(gdata, function(z) z$fid)
rm(files)

# Check that fid are well defined
# if (!all(map_lgl(gdata, ~ .x$fid_ok))) stop("check fid")

# -------------------------------- Analyses --------------------------------- #

# Number of relatives, participants, results and positives per family
ndata <- map_dfr(gdata, ~ mutate(.x$nodes, fid = .x$fid)) %>%
  group_by(fid) %>%
  mutate(
    positiveIndexCase =
      if (sum(indexCase) == 1) positive[indexCase == 1] else NA_integer_,
    armExp = if (length(unique(na.omit(armExp))) == 1) na.omit(armExp)[1] else
      NA_integer_
  ) %>%
  filter(!(nonEligible %in% 1), indexCase == 0) %>%
  summarise(
    positiveIndexCase = positiveIndexCase[1],
    armExp = armExp[1],
    n_relatives = n(),
    n_participants = sum(participate),
    n_results = sum(!is.na(positive)),
    n_positive = sum(positive, na.rm = TRUE)
  )

# Participation and positivity rates (proportions)
tbls <- c(participation_rate.all_IC      = 1,
          participation_rate.negative_IC = 2,
          participation_rate.positive_IC = 3,
          positivity_rate                = 4)
tbls <- mclapply(tbls, function(j) {
  k <- if (j <= 3) 1 else 2 # k=1: participation, k=2: positivity
  dta <- if (j == 1) {
    ndata
  } else if (j == 2) {
    filter(ndata, positiveIndexCase == 0)
  } else {
    filter(ndata, positiveIndexCase == 1)
  }
  dta <- filter(dta, !is.na(armExp))
  dta <- filter(dta, if (k == 1) n_relatives > 0 else n_results > 0)
  dta <- map_dfr(1:nrow(dta), function(i) {
    with(dta[i, ], data.frame(
      fid = fid,
      arm = factor(armExp, 0:1, c("Control", "Experimental")),
      y = if (k == 1) {
        c(rep(0L, n_relatives - n_participants), rep(1L, n_participants))
      } else {
        c(rep(0L, n_results - n_positive), rep(1L, n_positive))
      }
    ))
  })
  tab <- map_dfr(1:2, function(l) {
    fml <- if (l == 1) y ~ 1 else y ~ arm
    fit <- geem(fml, id = fid, family = binomial, data = dta,
                corstr = "exchangeable")
    smy <- summary(fit)
    # odds ratio of the GEE model
    or <- smy$beta
    s <- qnorm(0.975) * smy$se.robust
    or <- exp(cbind(or = or, or_lwr = or - s, or_upr = or + s))
    or <- cbind(data.frame(coef = smy$coefnames), or,
                data.frame(pval = smy$p))
    # predictions of the gee model
    x <- if (length(fit$beta) == 1) list(1) else list(c(1, 0), c(1, 1))
    s <- qnorm(0.975) *
      sqrt(sapply(x, function(z) (z %*% fit$var %*% z)[1, 1]))
    p <- sapply(x, function(z) z %*% fit$beta)
    p <- 1 / (1 + exp(-cbind(prop = p, prop_lwr = p - s, prop_upr = p + s)))
    p <- cbind(data.frame(coef = smy$coefnames), p)
    # Table
    w <- data.frame(or = NA, or_lwr = NA, or_upr = NA, pval = NA)
    tab <- cbind(p, rbind(w, or[-1, -1]))
    if (nrow(tab) == 1) {
      tab$coef <- ""
    } else {
      tab$coef <- sub("\\(Intercept\\)", "Control", tab$coef)
      tab$coef <- sub("^arm", "", tab$coef)
    }
    names(tab)[names(tab) == "coef"] <- "arm"
    tab
  })
  Merge <- function(x, y) merge(x, y, by = "arm")
  n_fam <- aggregate(fid ~ arm, dta, function(z) length(unique(z)))
  names(n_fam)[2] <- "n_families"
  n0 <- data.frame(arm = "", n_families = length(unique(dta$fid)))
  n_fam <- rbind(n0, n_fam)
  n <- Reduce(Merge, lapply(c("length", "sum", "mean"), function(w) {
    z <- aggregate(y ~ arm, dta, get(w))
    names(z)[2] <- w
    z0 <- setNames(data.frame("", get(w)(dta$y)), c("arm", w))
    return(rbind(z0, z))
  }))
  names(n)[names(n) == "length"] <- c("n_relatives", "n_results")[k]
  names(n)[names(n) == "sum"] <- c("n_participants", "n_positive")[k]
  names(n)[names(n) == "mean"] <- "raw_prop"
  Merge(Merge(n_fam, n), tab)
})

# Detection rate
dta <- ndata %>%
  filter(positiveIndexCase == 1) %>%
  mutate(arm = factor(armExp, 0:1, c("Control", "Experimental"))) %>%
  select(n_positive, arm) %>%
  na.omit()
tab <- map_dfr(1:2, function(l) {
  fml <- if (l == 1) n_positive ~ 1 else n_positive ~ arm
  fit <- glm(fml, family = poisson, data = dta)
  # rate ratios of the Poisson model
  beta <- coef(fit)
  s <- qnorm(0.975) * sqrt(diag(vcov(fit)))
  rr <- exp(cbind(rr = beta, rr_lwr = beta - s, rr_upr = beta + s))
  rr <- cbind(data.frame(coef = names(beta)), rr,
              pval = coef(summary(fit))[, 4])
  # predictions of the Poisson model
  x <- if (length(beta) == 1) list(1) else list(c(1, 0), c(1, 1))
  s <- qnorm(0.975) *
    sqrt(sapply(x, function(z) (z %*% vcov(fit) %*% z)[1, 1]))
  r <- sapply(x, function(z) z %*% beta)
  r <- exp(cbind(rate = r, rate_lwr = r - s, rate_upr = r + s))
  r <- cbind(data.frame(coef = names(beta)), r)
  # Table
  w <- data.frame(rr = NA, rr_lwr = NA, rr_upr = NA, pval = NA)
  tab <- cbind(r, rbind(w, rr[-1, -1]))
  if (nrow(tab) == 1) {
    tab$coef <- ""
  } else {
    tab$coef <- sub("\\(Intercept\\)", "Control", tab$coef)
    tab$coef <- sub("^arm", "", tab$coef)
  }
  names(tab)[names(tab) == "coef"] <- "arm"
  names(tab) <- sub("^rr", "rate_ratio", names(tab))
  rownames(tab) <- NULL
  tab
})
n <- data.frame(
  arm = c("", "Control", "Experimental"),
  n_families = c(nrow(dta), sum(dta$arm == "Control"),
                 sum(dta$arm == "Experimental")),
  n_positive = c(sum(dta$n_positive),
                 sum(dta[dta$arm == "Control", "n_positive"]),
                 sum(dta[dta$arm == "Experimental", "n_positive"]))
)
tbls$detection_rate <- full_join(n, tab, by = "arm")
rm(dta, tab, n)

# Formatted tables
ftbls <- lapply(append(tbls, list(data = ndata)), function(tbl) {
  fmt <- "%.2f"
  if (all(c("prop", "or") %in% names(tbl))) {
    ftbl <- tbl %>%
      mutate(
        raw_prop = sprintf(fmt, prop),
        prop = paste0(sprintf(fmt, prop), " (", sprintf(fmt, prop_lwr),
                      ",", sprintf(fmt, prop_upr), ")"),
        or = if_else(!is.na(or),
          paste0(sprintf(fmt, or), " (", sprintf(fmt, or_lwr), ",",
                 sprintf(fmt, or_upr), ")"),
          as.character(NA))
      ) %>%
      select(!c(prop_lwr, prop_upr, or_lwr, or_upr))
  } else if (all(c("rate", "rate_ratio") %in% names(tbl))) {
    ftbl <- tbl %>%
      mutate(
        rate = paste0(sprintf(fmt, rate), " (", sprintf(fmt, rate_lwr),
                      ",", sprintf(fmt, rate_upr), ")"),
        rate_ratio = if_else(!is.na(rate_ratio),
          paste0(sprintf(fmt, rate_ratio), " (",
                 sprintf(fmt, rate_ratio_lwr), ",",
                 sprintf(fmt, rate_ratio_upr), ")"),
          as.character(NA))
      ) %>%
      select(!c(rate_lwr, rate_upr, rate_ratio_lwr, rate_ratio_upr))
  } else {
    ftbl <- tbl %>%
      rename(arm = armExp) %>%
      mutate(arm = factor(arm, 0:1, c("Control", "Experimental")))
  }
  if ("pval" %in% names(ftbl)) {
    ftbl <- mutate(ftbl, pval = if_else(pval < .001, "<.001", sprintf("%.3f", pval)))
  }
  names(ftbl) = recode(names(ftbl),
    arm               = "Arm",
    n_families        = "Index cases",
    n_relatives       = "Eligible relatives",
    n_participants    = "Tested relatives",
    n_results         = "Relatives with test restitution",
    n_positive        = "Positive relatives",
    raw_prop          = "Raw proportion",
    prop              = "Estimated proportion",
    or                = "Odds ratio",
    rate              = "Rate",
    rate_ratio        = "Rate ratio",
    pval              = "p-value",
    fid               = "Family ID",
    positiveIndexCase = "Positive Index Case"
  )
  ftbl
})

# Export tables
if (!dir.exists(here("results"))) dir.create(here("results"))
write_xlsx(ftbls, here("results", paste0("analyses_",
                                         format(Sys.Date(), "%Y%m%d"), ".xlsx")))
