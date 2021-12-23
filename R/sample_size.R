#!/usr/bin/env Rscript

library(geeM)
library(parallel)
library(pbapply)

options(mc.cores = detectCores() - 1)

# Arguments
args = commandArgs(trailingOnly=TRUE)

# Input and output files
if (FALSE) { # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (length(args)==0) {
  stop("At least one argument must be supplied (input file)")
} else {
  input_file <- args[1]
  if (length(args) > 1) {
    output_file <- args[2]
  } else {
    output_file <- input_file
  }
}
if (!dir.exists(dirname(output_file))) {
  dir.create(dirname(output_file), recursive = TRUE)
}
} # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
input_file <- "ss.csv"
ss <- read.csv(input_file)

# Initialisation du générateur de nombres aléatoires
set.seed(333)

# Fonctions logit et expit
logit <- function(p) log(p /(1 - p))
expit <- function(x) 1 / (1 + exp(-x))

# Fonction transformant un intervale autour d'une probabilité en intervalle
# symétrique sur l'échelle logit
conv_int <- function(p, r) {
  d <- uniroot(function(d) expit(logit(p) + d) - expit(logit(p) - d) - r,
                           c(-1, 1) * 10^4)$root
  list(sd.logit = d / 2, p.ci = c(expit(logit(p) - d), expit(logit(p) + d)))
}

# Boucle sur les différentes conditions
ss$sample.size <- NA
ss$nsim <- NA
ss$rejection.rate.0 <- NA
ss$rejection.rate.1 <- NA
ss$no_conv_prop.0 <- NA
ss$no_conv_prop.1 <- NA
for (i in 1:nrow(ss)) {
  alpha <- ss[i, "alpha"]
  pwr <- ss[i, "power"]
  p <- unname(unlist(ss[i, c("pi1", "pi2")]))
  r <- unname(unlist(ss[i, c("range1", "range2")]))
  m <- unname(unlist(ss[i, c("size1", "size2")]))
  # Calcul de la taille d'échantillon
  ssize <- function(alpha = .05, power = .9, m, p, r) {
    if (length(m) == 1) m <- c(m, m)
    if (length(p) == 1) p <- c(p, p)
    if (length(r) == 1) r <- c(r, r)
    coef_z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
    nvar <- 1 / m[1] * p[1] * (1 - p[1]) + (m[1] - 1) / m[1] * (r[1] / 4)^2 +
      1 / m[2] * p[2] * (1 - p[2]) + (m[2] - 1) / m[2] * (r[2] / 4)^2
    coef_z * nvar / (p[2] - p[1])^2
  }
  n <- ceiling(ssize(alpha = alpha, power = pwr, m = m, p = p, r = r))
  # Intervalles des proportions
  p.ci.str <- lapply(1:2, function(j) {
    if (length(r) == 1) r <- c(r, r)
    ci <- conv_int(p[j], r[j])$p.ci
    paste0("[", paste(round(ci, 4), collapse = ","), "]")
  })
  # Simulation de données
  # n: nombre de grappes par groupe (nombre de famille par groupe)
  # m: nombre moyen d'observations par grappe (nombre de proches contactés par 
  #    famille)
  # u: écart type du nombre d'observations par grappe
  # p: proportions moyennes dans les groupes contrôle et expérimental
  # r: longueurs d'intervalle pour les proportions
  # nsim: nombre de simulations
  sim <- function(n, m, u, p, r, nsim = 10^3) {
    if (length(n) == 1) n <- c(n, n)
    if (length(m) == 1) m <- c(m, m)
    if (length(u) == 1) u <- c(u, u)
    if (length(p) == 1) p <- c(p, p)
    if (length(r) == 1) r <- c(r, r)
    s <- sapply(1:2, function(k) conv_int(p[k], r[k])$sd.logit)
    d <- 10^ceiling(log(n + 1, 10))
    sim_list <- mclapply(1:nsim, function(k) {
      do.call(rbind, lapply(1:2, function(grp) {
        do.call(rbind, lapply(1:n[grp], function(i) {
          J <- round(min(2 * m[grp] + 1, max(1, rnorm(1, m[grp], u[grp]))))
          cbind(
            grp = grp,
            cluster = grp * d[grp] + i,
            y = rbinom(J, 1, expit(logit(p[grp]) + rnorm(1, 0, s[grp])))
          )
        }))
      }))
    })
    sim_list <- lapply(sim_list, function(z) {
      z <- as.data.frame(z)
      z$grp <- factor(z$grp)
      return(z)
    })
    return(sim_list)
  }
  # Simulations sous les hypothèses nulle et alternative
  u <- unname(unlist(ss[i, c("sd.size1", "sd.size2")]))
  ns <- 10^3
  sim0_list <- sim(n = n, m = m, u = u, p = p[1], r = r, nsim = ns)
  sim1_list <- sim(n = n, m = m, u = u, p = p, r = r, nsim = ns)
  # Taux de rejet sous les hypothèses nulle et alternative
  rr <- function(sim_list) {
    v <- do.call(base::c, mclapply(sim_list, function(z) {
      fit <- tryCatch(geem(y ~ grp, id = cluster, family = binomial, data = z),
                      warning = function(w) w, error = function(e) e)
      if (any(class(fit) == "geem")) {
        o <- summary(fit)$p[2] <= alpha
      } else if (any(class(fit) == "warning")) {
        if (fit$message == "Did not converge") {
          o <- NA
        } else {
          stop("Other warning in geem")
        }
      } else {
        stop("Other problem in geem")
      }
      o
    }))
    no_conv_prop <- sum(is.na(v)) / length(v)
    rate <- sum(v, na.rm = TRUE) / sum(!is.na(v))
    list(rate = rate, no_conv_prop = no_conv_prop)
  }
  rr0 <- rr(sim0_list)
  rr1 <- rr(sim1_list)
  # Results
  ss[i, "sample.size"] <- n
  ss[i, "nsim"] <- ns
  ss[i, "rejection.rate.0"] <- rr0$rate
  ss[i, "rejection.rate.1"] <- rr1$rate
  ss[i, "no_conv_prop.0"] <- rr0$no_conv_prop
  ss[i, "no_conv_prop.1"] <- rr1$no_conv_prop
}
rm(i, alpha, pwr, p, r, m, ssize, n, p.ci.str, sim, u, ns, sim0_list,
   sim1_list, rr, rr0, rr1)
