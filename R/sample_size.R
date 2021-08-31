# Bibliothèques
library(parallel)
library(pbapply)
library(lme4)
library(lmerTest)

# Répertoire de travail
if(Sys.info()["nodename"] == "hos45528") {
  setwd("~/Projects/Consultations/Nanchen David (CATCH)")
}

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

# Fichier contenant les résultats
if(!file.exists("results")) dir.create("results")
output_file <- "results/sample_size_20191203.txt"
file.create(output_file)

# Boucle sur les différentes conditions
for (k in 1:2) {

  if (k == 1) {
    p <- c(0.5, 0.7)
    r <- 0.4
    m <- 4
  } else {
    p <- c(0.2, 0.4)
    r <- c(0.1, 0.3)
    m <- 4
  }

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
  n <- ssize(m = m, p = p, r = r)

  # Intervalles des proportions
  p.ci.str <- lapply(1:2, function(j) {
    if (length(r) == 1) r <- c(r, r)
    ci <- conv_int(p[j], r[j])$p.ci
    paste0("[", paste(round(ci, 4), collapse = ","), "]")
  })

  # Output
  sink(output_file, append = TRUE)
  cat("------------------- Taille d'échantillon -------------------\n\n")
  cat(paste("Proportions moyennes dans les groupes contrôles et",
            "expérimental :", paste(p, collapse = ", "), "\n"))
  cat(paste("Longueur(s) d'intervalle pour les proportions :",
            paste(r, collapse = ", "), "\n"))
  cat(paste("Intervalles pour les proportions :",
            paste(p.ci.str, collapse = ", "), "\n"))
  cat(paste("Taille(s) moyenne des familles :",
            paste(m, collapse = ", "), "\n"))
  cat(paste("Nombre de famille nécessaire par groupe",
            "(alpha = .05, power = .9) :", round(n, 2), "\n"))
  cat("\n\n")
  sink()

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
    sim_list <- lapply(1:nsim, function(k) {
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
  if (k == 1) {
    n2 <- 36
  } else {
    n2 <- 28
  }
  u <- .5
  ns <- 10^3
  sim0_list <- sim(n = n2, m = m, u = u, p = p[1], r = r, nsim = ns)
  sim1_list <- sim(n = n2, m = m, u = u, p = p, r = r, nsim = ns)

  # Initialisation du calcul parallèle
  cl <- makeCluster(detectCores())
  invisible(clusterEvalQ(cl, library(lme4)))

  # Modèle de régression logistique pour chaque simulation
  fit <- function(z) {
    suppressMessages(glmer(
      y ~ grp + (1 | cluster), family = binomial, data = z,
      control = glmerControl(optimizer = "bobyqa",
                             optCtrl = list(maxfun = 10^5))
    ))
  }
  m0_list <- pblapply(sim0_list, fit, cl = cl)
  m1_list <- pblapply(sim1_list, fit, cl = cl)

  # Proportion de fit singuliers
  pS0 <- sum(sapply(m0_list, isSingular)) / length(m0_list)
  pS1 <- sum(sapply(m1_list, isSingular)) / length(m1_list)

  # Taux de rejet sous les hypothèses nulle et alternative
  rr <- function(m_list, alpha = .05) {
    v <- pbsapply(m_list, function(m) {
      coef(summary(m))["grp2", "Pr(>|z|)"] <= alpha
    })
    sum(v) / length(v)
  }
  rr0 <- rr(m0_list)
  rr1 <- rr(m1_list)

  # Fin du calcul parallèle
  stopCluster(cl)

  # Output
  sink(output_file, append = TRUE)
  cat("----------------------- Simulations ------------------------\n\n")
  cat(paste("Nombre de simulations:", ns, "(par hypothèse)\n"))
  cat(paste("Nombre de familles par groupe :", n2, "\n"))
  cat(paste("Proportions moyennes dans les groupes contrôles et",
            "expérimental :", paste(p, collapse = ", "), "\n"))
  cat(paste("Longueur(s) d'intervalle pour les proportions :",
            paste(r, collapse = ", "), "\n"))
  cat(paste("Intervalles pour les proportions :",
            paste(p.ci.str, collapse = ", "), "\n"))
  cat(paste("Taille(s) moyenne des familles :",
            paste(m, collapse = ", "), "\n"))
  cat(paste("Proportion de fit singulier (par hypothèse):", pS0, pS1, "\n"))
  cat(paste("Taux de rejet (niv=.05) sous l'hypothèse nulle:", rr0, "\n"))
  cat(paste("Taux de rejet (niv=.05) sous l'alternative:", rr1, "\n"))
  cat("\n\n")
  sink()

}

# Session info
sink(output_file, append = TRUE)
cat("---------------------- Infos session -----------------------\n\n")
print(sessionInfo(), locale = FALSE)
sink()


