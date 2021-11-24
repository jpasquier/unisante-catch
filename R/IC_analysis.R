library(geeM)
library(parallel)
library(RCurl)
library(writexl)
library(XML)

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
pdta <- read.csv(text = postForm(
  uri = uri,
  token = token,
  content = 'record',
  format = 'csv'
))

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
relatives$condition <-
  apply(relatives[paste0("cat_r_", c("alive", "country", "participate"))],
        1, function(z) all(!is.na(z) & (z == c(1, 1, 0) | z == c(3, 3, 0))))

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
v <- c(idv, "cat_s_referent_id")
participants <- pdta[b, v]
if (any(duplicated(participants$cat_ec_participant_sid))) stop("duplicated id")
###############################################################################
#if (any(participants$cat_s_referent_id == "")) stop("missing referent")
if (any(participants$cat_s_referent_id == "")) warning("missing referent")
###############################################################################
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

# Number of relatives and participants per family
r <- aggregate(condition ~ cat_s_familycode, relatives, sum)
names(r)[2] <- "n_relatives"
p <- aggregate(cat_ec_participant_sid ~ cat_s_familycode, participants, length)
names(p)[2] <- "n_participants"
n <- merge(r, p, by = "cat_s_familycode", all = TRUE)
if (any(is.na(n$n_relatives))) stop("check n_relatives")
n[is.na(n)] <- 0
if (any(n$n_relatives < n$n_participants)) stop("n_relatives < n_participants")
rm(r, p)

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

# Generate data to analyse from n
adta <- do.call(rbind, lapply(1:nrow(n), function(i) {
  if (n[i, "n_relatives"] == 0) return(NULL)
  with(n[i, ], data.frame(
    participate = c(rep(0, n_relatives - n_participants),
                    rep(1, n_participants)),
    familycode = cat_s_familycode,
    arm = factor(cat_s_arm, 2:1, c("Control", "Intervention")),
    result = factor(cat_l_result, 2:1, c("Negative", "Positive")),
    date_rdv2 = cat_s_date_rdv_two
  ))
}))

# Analyses
S <- c("all", "negative", "positive")
S <- c(S, paste0(S, "_6m"))
S <- setNames(S, S)
fits <- mclapply(S, function(s) {
  sdta <- subset(adta, !is.na(arm))
  if (grepl("^negative", s)) {
    sdta <- subset(sdta, !is.na(result) & result == "Negative")
  } else if (grepl("^positive", s)) {
    sdta <- subset(sdta, !is.na(result) & result == "Positive")
  }
  if (grepl("_6m$", s)) {
    six_months_ago <- seq(Sys.Date(), length = 2, by = "-6 months")[2]
    sdta <- subset(sdta, !is.na(date_rdv2) & date_rdv2 <= six_months_ago)
  }
  fit <- geem(participate ~ arm, id = familycode, corstr = "exch",
              family = binomial, data = sdta)
  smy <- summary(fit)
  # odds ratio of the GEE model
  or <- smy$beta
  s <- qnorm(0.975) * smy$se.robust
  or <- exp(cbind(or = or, or_lwr = or - s, or_upr = or + s))
  or <- cbind(data.frame(coef = smy$coefnames), or, data.frame(pval = smy$p))
  # predictions of the gee model
  x <- list(c(1, 0), c(1, 1))
  s <- qnorm(0.975) * sqrt(sapply(x, function(z) (z %*% fit$var %*% z)[1, 1]))
  p <- sapply(x, function(z) z %*% fit$beta)
  p <- 1 / (1 + exp(-cbind(prop = p, prop_lwr = p - s, prop_upr = p + s)))
  p <- cbind(data.frame(coef = smy$coefnames), p)
  # Table
  tbl <- cbind(p, rbind(NA, or[-1, -1]))
  tbl$coef[tbl$coef == "(Intercept)"] <- levels(sdta$arm)[1]
  tbl$coef <- sub("^arm", "", tbl$coef)
  names(tbl)[names(tbl) == "coef"] <- "arm"
  # Number of families, relatives and participants per arm
  n_fam <- aggregate(familycode ~ arm, sdta, function(z) length(unique(z)))
  names(n_fam)[2] <- "n_families"
  Merge <- function(x, y) merge(x, y, by = "arm")
  fcts <- c(n_relatives = "length", n_participants = "sum", raw_prop = "mean")
  n <- Reduce(Merge, lapply(names(fcts), function(u) {
    z <- aggregate(participate ~ arm, sdta, get(fcts[u]))
    names(z)[2] <- u
    return(z)
  }))
  n <- Merge(n_fam, n)
  tbl <- Merge(n, tbl)
  # Results
  list(fit = fit, tbl = tbl)
})
rm(S)

# Export table
tbls <- append(lapply(fits, function(z) z$tbl), list(N = n))
f <- paste0("results/IC_analysis_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
write_xlsx(tbls, f)
rm(f)
