library(RCurl)
library(XML)
library(catchr)
library(geeM)
library(parallel)
library(writexl)

options(mc.cores = detectCores() - 1)

# Working directory
setwd("~/Projects/Consultations/Nanchen David (CATCH)")

# ------------------------------- REDCap data ------------------------------- #

# API
uri   <- "https://redcap.unisante.ch/api/"

# Token
token <- "misc/redcap_personal_data.token"
token <- readChar(token, file.info(token)$size - 1)

# Import personal data with the API (RCurl)
tmp_file <- "/mnt/ramdisk/CATCH_pdta.rda"
if (file.exists(tmp_file)) {
  load(tmp_file)
} else {
  pdta <- read.csv(text = postForm(
    uri = uri,
    token = token,
    content = 'record',
    format = 'csv'
  ))
  save(pdta, file = tmp_file)
}
rm(tmp_file)

# Help funtion to check if values are missing
any.miss <- function(x) any(is.na(x) | x == "")

# Id variables
idv <- c("cat_ec_participant_sid", "cat_s_familycode")

# Extract parents from index case data
parents <- do.call(rbind, lapply(1:2, function(i) {
  v1 <- c("id", "alive", "country", "participate")
  v1 <- paste("cat_r", v1, c("father", "mother")[i], sep = "_")
  v2 <- paste("cat_r", c("father", "mother")[i], c("sms", "mail"), "done",
              sep = "_")
  v <- c(idv, v1, v2)
  b <- pdta$cat_e_participant_type %in% 1
  d <- cbind(pdta[b, v], cat_r_type = i)
  names(d) <- sub(paste0("_", c("father", "mother")[i]), "", names(d))
  return(d)
}))
if (any.miss(parents[idv])) stop("missing id")
nrow(parents)

# All index cases relatives
i <- !is.na(pdta[[idv[2]]]) & pdta[[idv[2]]] != "" &
  pdta$cat_e_participant_type %in% 1
ids <- unique(pdta[i, idv])
if (any.miss(ids)) stop("missing id")
if (any(duplicated(ids[[idv[1]]]))) stop("duplicated id")
v <- c("id", "type", "alive", "country", "participate")
v <- c(idv[1], paste0("cat_r_", v),
       paste0("cat_r_", c("sms", "mail"), "_done"))
i <- pdta$redcap_repeat_instrument == "family_history" &
  pdta[[idv[1]]] %in% ids[[idv[1]]]
relatives <- pdta[i, v]
if (any.miss(relatives[[idv[1]]])) stop("missing id")
relatives <- merge(relatives, ids, by = idv[1], all.x = TRUE)
if (any(is.na(relatives[[idv[2]]]))) stop("missing id")
nrow(relatives)
relatives <- rbind(parents, relatives)
relatives <- relatives[order(relatives[[idv[2]]], relatives[[idv[1]]]), ]
nrow(relatives)
rm(i, ids, v)

# # Add study variables to relatives table
# v <- c(idv[1], "cat_s_arm", "cat_l_result")
# relatives <- merge(relatives, pdta[pdta$cat_e_participant_type %in% 1, v],
#                    by = idv[1], all.x = TRUE)
# rm(v)

# Relatives who meet the conditions to be invited
# relatives$condition <-
#   apply(relatives[paste0("cat_r_", c("alive", "country", "participate"))],
#         1, function(z) all(!is.na(z) & (z == c(1, 1, 0) | z == c(3, 3, 0))))
relatives$condition <-
  apply(relatives[paste0("cat_r_", c("alive", "country"))],
        1, function(z) all(!is.na(z) & z %in% c(1, 3)))

# At most one parent could be invited
z <- aggregate(cat_r_type ~ cat_ec_participant_sid,
               subset(relatives, condition),
               function(z) all(1:2 %in% z))
z <- z[z[[2]], 1]
z <- relatives$cat_ec_participant_sid %in% z & relatives$cat_r_type %in% 2
relatives <- relatives[!z, ]
rm(z)

# # cat_r_id that are in the variable cat_ec_participant_sid
# relatives$rid_in_sid <-
#   ifelse(relatives$cat_r_id == "", NA,
#          relatives$cat_r_id %in% pdta$cat_ec_participant_sid)

# # Duplicated relatives ids (cat_r_id)
# rid <- relatives$cat_r_id[relatives$cat_r_id != ""]
# dup <- unique(rid[duplicated(rid)])
# dup <- relatives[relatives$cat_r_id %in% dup, ]
# dup[order(dup$cat_r_id), ]
# rm(dup, rid)

# # Relatives who came but who are not in cat_r_id
# sid <- pdta[pdta$cat_e_participant_type %in% 2, idv[1]]
# sid[!(sid %in% pdta$cat_r_id)]
# rm(sid)

# Index cases relatives who came
b <- pdta$cat_e_participant_type %in% 2 & pdta$cat_con_agree %in% 1
v <- c(idv, "cat_s_referent_id", "cat_l_result")
v <- c(idv, "cat_s_referent_id", "cat_s_referent_relation", "cat_l_result")
participants <- pdta[b, v]
if (any(duplicated(participants$cat_ec_participant_sid))) stop("duplicated id")
#if (any(participants$cat_s_referent_id == "")) stop("missing referent")
if (any(participants$cat_s_referent_id == "")) {
  warning("missing referent(s) - remove participant(s) - possible bias")
  write_xlsx(participants[participants$cat_s_referent_id == "", ],
             "~/missing_referent.xlsx")
  participants <- participants[participants$cat_s_referent_id != "", ]
}
participants <- merge(
  participants,
  pdta[pdta$cat_e_participant_type %in% 1:2,
       c(idv[1], "cat_e_participant_type")],
  by.x = "cat_s_referent_id",
  by.y = idv[1],
  all.x = TRUE
)
names(participants)[names(participants) == "cat_e_participant_type"] <-
  "referent_type"
participants <- participants[participants$referent_type == 1, ]
participants <- participants[order(participants$cat_s_familycode), ]
nrow(participants)
rm(b, v)

# Check that at most one of the parents participates. This is equivalent to
# checking that the referent is at most once the son or daughter of a
# participant.
b <- with(participants, table(cat_s_referent_id, cat_s_referent_relation))
b <- subset(as.data.frame(b), cat_s_referent_relation %in% 5:6)$Freq <= 1
if (any(!b)) stop("more than one parent participate")

# Check that relatives are all linked to one person (index case) per family
b <- aggregate(cat_ec_participant_sid ~ cat_s_familycode, relatives,
               function(x) length(unique(x)) == 1)[[2]]
if (any(!b)) stop("multiple IC per family")
rm(b)

# Check that participants are all linked to one referent (index case) per
# family
b <- aggregate(cat_s_referent_id ~ cat_s_familycode, participants,
               function(x) length(unique(x)) == 1)[[2]]
if (any(!b)) stop("multiple referent per family")
rm(b)

# Number of relatives, participants, results and positives per family
r <- list(aggregate(condition ~ cat_s_familycode, relatives, sum))
names(r[[1]])[2] <- "n_relatives"
p <- aggregate(cat_ec_participant_sid ~ cat_s_familycode, participants, length)
names(p)[2] <- "n_participants"
p_r <- aggregate(!is.na(cat_l_result) ~ cat_s_familycode, participants, sum)
names(p_r)[2] <- "n_results"
p_p <- aggregate(I(cat_l_result %in% 1) ~ cat_s_familycode, participants, sum)
names(p_p)[2] <- "n_positive"
n <- merge(r, p, by = "cat_s_familycode", all = TRUE)
n <- merge(n, p_r, by = "cat_s_familycode", all = TRUE)
n <- merge(n, p_p, by = "cat_s_familycode", all = TRUE)
if (any(is.na(n$n_relatives))) stop("check n_relatives")
n[is.na(n)] <- 0
if (any(n$n_relatives < n$n_participants)) stop("n_relatives < n_participants")
if (any(n$n_participants < n$n_results)) stop("n_participants < n_results")
if (any(n$n_results < n$n_positive)) stop("n_results < n_positive")
rm(r, p, p_r, p_p)

# Add study variables to table `n`
s <- unique(pdta[pdta$cat_e_participant_type %in% 1,
            c(idv, "cat_s_arm", "cat_l_result", "cat_s_date_rdv_two")])
if (any(duplicated(s$cat_ec_participant_sid))) stop("duplicated id")
if (any(duplicated(s$cat_s_familycode))) stop("duplicated id")
n <- merge(n, s, by = "cat_s_familycode", all.x = TRUE)
if (nrow(n) != sum(pdta$cat_e_participant_type %in% 1)) stop("check n")
n$cat_s_date_rdv_two <- as.Date(n$cat_s_date_rdv_two,
                                format = "%Y-%m-%d %H:%M")
rm(s)

# Analyse - Generate data from n
adta <- list()
adta$participation_rate <- do.call(rbind, lapply(1:nrow(n), function(i) {
  if (n[i, "n_relatives"] == 0) return(NULL)
  with(n[i, ], data.frame(
    participate = c(rep(0, n_relatives - n_participants),
                    rep(1, n_participants)),
    familycode = cat_s_familycode,
    arm = factor(cat_s_arm, 2:1, c("Control", "Intervention")),
    index_case_result = factor(cat_l_result, 2:1, c("Negative", "Positive")),
    date_rdv2 = cat_s_date_rdv_two
  ))
}))
adta$positivity_rate <- do.call(rbind, lapply(1:nrow(n), function(i) {
  if (n[i, "n_results"] == 0) return(NULL)
  with(n[i, ], data.frame(
    positive = c(rep(0, n_results - n_positive),
                 rep(1, n_positive)),
    familycode = cat_s_familycode,
    arm = factor(cat_s_arm, 2:1, c("Control", "Intervention")),
    index_case_result = factor(cat_l_result, 2:1, c("Negative", "Positive")),
    date_rdv2 = cat_s_date_rdv_two
  ))
}))

# Participation and positivity rates (proportions)
S <- c("all", "negative", "positive")
S <- c(S, paste0(S, "_6m"))
S <- setNames(S, S)
tbls <- mclapply(S, function(s) {
  sdta <- lapply(adta, function(z) {
    sdta <- subset(z, !is.na(arm))
    if (grepl("^negative", s)) {
      sdta <- subset(sdta, index_case_result %in% "Negative")
    } else if (grepl("^positive", s)) {
      sdta <- subset(sdta, index_case_result %in% "Positive")
    }
    if (grepl("_6m$", s)) {
      six_months_ago <- seq(Sys.Date(), length = 2, by = "-6 months")[2]
      sdta <- subset(sdta, !is.na(date_rdv2) & date_rdv2 <= six_months_ago)
    }
    return(sdta)
  })
  fits <- lapply(setNames(names(sdta), names(sdta)), function(u) {
    fml <- if (u == "participation_rate") participate ~ 1 else positive ~ 1
    fmls <- list(fml, update(fml, . ~ arm))
    lapply(fmls, function(fml) {
      if (u == "positivity_rate" & !grepl("^positive", s)) return(NULL)
      catch_expr(
        geem(fml, id = familycode, corstr = "exch",
             family = binomial, data = sdta[[u]]),
        warning = c(collect, muffle)
      )
    })
  })
  wrns <- unlist(lapply(fits, lapply, function(fit) {
    if (is.null(fit)) character(0) else do.call(paste, fit$warning)
  }), recursive = FALSE)
  fits <- lapply(fits, lapply, function(fit) {
    if (is.null(fit)) return(NULL)
    fit$value
  })
  tbls <- lapply(fits, lapply, function(fit) {
    if (is.null(fit)) return(NULL)
    smy <- summary(fit)
    # odds ratio of the GEE model
    or <- smy$beta
    s <- qnorm(0.975) * smy$se.robust
    or <- exp(cbind(or = or, or_lwr = or - s, or_upr = or + s))
    or <- cbind(data.frame(coef = smy$coefnames), or, data.frame(pval = smy$p))
    # predictions of the gee model
    x <- if (length(fit$beta) == 1) list(1) else list(c(1, 0), c(1, 1))
    s <- qnorm(0.975) * 
      sqrt(sapply(x, function(z) (z %*% fit$var %*% z)[1, 1]))
    p <- sapply(x, function(z) z %*% fit$beta)
    p <- 1 / (1 + exp(-cbind(prop = p, prop_lwr = p - s, prop_upr = p + s)))
    p <- cbind(data.frame(coef = smy$coefnames), p)
    # Table
    w <- data.frame(or = NA, or_lwr = NA, or_upr = NA, pval = NA)
    tbl <- cbind(p, rbind(w, or[-1, -1]))
    if (nrow(tbl) == 1) {
      tbl$coef <- ""
    } else {
      tbl$coef <- sub("\\(Intercept\\)", "Control", tbl$coef)
      tbl$coef <- sub("^arm", "", tbl$coef)
    }
    names(tbl)[names(tbl) == "coef"] <- "arm"
    tbl
  })
  tbls <- lapply(tbls, function(z) do.call(rbind, z))
  tbls <- tbls[!sapply(tbls, is.null)]
  # Number of families, relatives and participants per arm
  Merge <- function(x, y) merge(x, y, by = "arm")
  n <- lapply(setNames(names(sdta), names(sdta)), function(u) {
    if (u == "positivity_rate" & !grepl("^positive", s)) return(NULL)
    d <- sdta[[u]]
    n_fam <- aggregate(familycode ~ arm, d, function(z) length(unique(z)))
    names(n_fam)[2] <- "n_families"
    n0 <- data.frame(arm = "", n_families = length(unique(d$familycode)))
    n_fam <- rbind(n0, n_fam)
    if (u == "participation_rate") {
      fcts <- c(n_relatives = "length", n_participants = "sum",
                raw_prop = "mean")
      y <- "participate"
    } else {
      fcts <- c(n_results = "length", n_positive = "sum", raw_prop = "mean")
      y <- "positive"
    }
    fml <- as.formula(paste(y, "~ arm"))
    n <- Reduce(Merge, lapply(names(fcts), function(w) {
      z <- aggregate(fml, d, get(fcts[w]))
      names(z)[2] <- w
      z0 <- setNames(data.frame("", get(fcts[w])(d[[y]])), c("arm", w))
      return(rbind(z0, z))
    }))
    Merge(n_fam, n)
  })
  n <- n[!sapply(n, is.null)]
  tbl <- lapply(setNames(names(tbls), names(tbls)), function(u) {
    Merge(n[[u]], tbls[[u]])
  })
  attr(tbl, "warnings") <- wrns
  return(tbl)
})
rm(S)

# Detection rate
tbls2 <- lapply(c(positive = 1, positive_6m = 3), function(k) {
  sdta <- subset(n, cat_l_result %in% 1)
  if (k == 2) {
    six_months_ago <- seq(Sys.Date(), length = 2, by = "-6 months")[2]
    sdta <- subset(sdta, !is.na(cat_s_date_rdv_two) &
                           cat_s_date_rdv_two <= six_months_ago)
  }
  sdta <- na.omit(sdta[c("n_positive", "cat_s_arm")])
  sdta$arm <- factor(sdta$cat_s_arm, 2:1, c("Control", "Intervention"))
  fmls <- list(n_positive ~ 1, n_positive ~ arm)
  tbl <- do.call(rbind, lapply(fmls, function(fml) {
    fit <- glm(fml, family = poisson, data = sdta)
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
    tbl <- cbind(r, rbind(w, rr[-1, -1]))
    if (nrow(tbl) == 1) {
      tbl$coef <- ""
    } else {
      tbl$coef <- sub("\\(Intercept\\)", "Control", tbl$coef)
      tbl$coef <- sub("^arm", "", tbl$coef)
    }
    names(tbl)[names(tbl) == "coef"] <- "arm"
    names(tbl) <- sub("^rr", "rate_ratio", names(tbl))
    rownames(tbl) <- NULL
    tbl
  }))
  n1 <- data.frame(
    arm = c("", "Control", "Intervention"),
    n_families = c(nrow(sdta), sum(sdta$arm == "Control"),
                   sum(sdta$arm == "Intervention")),
    n_positive = c(sum(sdta$n_positive),
                   sum(sdta[sdta$arm == "Control", "n_positive"]),
                   sum(sdta[sdta$arm == "Intervention", "n_positive"]))
  )
  merge(n1, tbl, by = "arm", sort = FALSE)
})
for (s in names(tbls2)) tbls[[s]]$detection_rate <- tbls2[[s]]
rm(s, tbls2)

# Warnings
wrns <- unlist(lapply(tbls, function(tbl) attr(tbl, "warnings")))

# Export table
z <- append(unlist(tbls, recursive = FALSE), list(N = n))
z <- z[!sapply(z, is.null)]
f <- paste0("results/IC_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
write_xlsx(z, f)
rm(z, f)
