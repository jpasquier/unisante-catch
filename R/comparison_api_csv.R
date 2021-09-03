library(RCurl)
library(XML)
library(parallel)
library(writexl)

options(mc.cores = detectCores() - 1)

# Working directory
setwd("~/Projects/Consultations/Nanchen David (CATCH)")

# Test similarity
`%~=%` <- function(x, y) !is.na(x) & !is.na(y) & x == y | is.na(x) & is.na(y)

# ------------------------------- REDCap data ------------------------------- #

# API
uri   <- "https://redcap.unisante.ch/api/"

# Tokens
tokens <- c("personal_data", "research_data")
tokens <- lapply(setNames(tokens, tokens), function(z) {
  z <- paste0("misc/redcap_", z, ".token")
  readChar(z, file.info(z)$size - 1)
})

# Import data with the API (RCurl)
catch_data_api <- lapply(tokens, function(token) {
  read.csv(text = postForm(
    uri = uri,
    token = token,
    content = 'record',
    format = 'csv'
  ))
})
catch_metadata <- lapply(tokens, function(token) {
  xmlToDataFrame(postForm(
    uri = uri,
    token = token,
    content = 'metadata',
    format = 'xml'
  ))
})

# Import data from CSV files
csv_files = list(
  personal_data = "CATCHPersonalData_DATA_2021-09-03_1045.csv",
  research_data = "CATCHResearchData_DATA_2021-09-03_1046.csv"
)
catch_data_csv <- lapply(csv_files, function(csv) {
  read.csv(file.path("data-raw", csv))
})

# ------------------------------- Comparisons ------------------------------- #

# Compare datasets
U <- c("personal_data", "research_data")
U <- setNames(U, U)
cmp <- mclapply(U, function(u) {
  dta <- list(
    API = catch_data_api[[u]],
    CSV = catch_data_csv[[u]]
  )
  mdta <- catch_metadata[[u]]
  # Unique id
  dta <- lapply(dta, function(d) {
    if (u == "personal_data") {
      d$id <- with(d, {
        A <- cat_ec_participant_sid
        B <- redcap_repeat_instrument
        B[is.na(B)] <- ""
        C <- redcap_repeat_instance
        C[is.na(C)] <- ""
        interaction(A, B, C)
      })
    } else {
      d$id <- with(d, {
        A <- cat_pi_participant_hid
        B <- redcap_event_name
        B[is.na(B)] <- ""
        interaction(A, B)
      })
    }
    if (any(is.na(d$id))) stop("undefined id")
    if (any(duplicated(d$id))) stop("duplicated id")
    return(d)
  })
  # List of variables
  V <- data.frame(variable = mdta$field_name, metadata = 1)
  for (x in names(dta)) {
    V1 <- data.frame(names(dta[[x]]), 1)
    names(V1) <- c("variable", x)
    V <- merge(V, V1, by = "variable", all = TRUE, sort = FALSE)
  }
  V[is.na(V)] <- 0
  rm(x, V1)
  # Compare ids between the two dataset
  b <- all(dta[[1]]$id %in% dta[[2]]$id) & all(dta[[2]]$id %in% dta[[1]]$id)
  if (!b) stop("different ids")
  # Compare the two datasets on the common variables
  v <- V$variable[apply(V[c("API", "CSV")] == 1, 1, all)]
  cmp_list <- lapply(setNames(v, v), function(z) {
   x1 <- trimws(dta[[1]][[z]])
   x1[x1 == ""] <- NA
   x2 <- trimws(dta[[2]][match(dta[[1]]$id, dta[[2]]$id), z])
   x2[x2 == ""] <- NA
   i <- !(x1 %~=% x2)
   if (any(i)) {
     r <- data.frame(id = dta[[1]][i, "id"], API = x1[i],
                     CSV = x2[i])
   } else {
     r <- NULL
   }
   return(r)
  })
  cmp_list <- cmp_list[!sapply(cmp_list, is.null)]
  list(variables = V, comparison = cmp_list)
})

# Export
tbls <- list(personal_data = cmp$personal_data$variables,
             research_data = cmp$research_data$variables)
write_xlsx(tbls, "results/comparison_api_csv_20210903.xlsx")
