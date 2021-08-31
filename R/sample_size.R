# Bibliothèques
library(parallel)
library(pbapply)
library(lme4)
library(lmerTest)

# Répertoire de travail
if(Sys.info()["nodename"] == "hos45528") {
  setwd("~/Projects/Consultations/Nanchen David (CATCH)")
}

# Seed
set.seed(333)

# Fonctions
logit <- function(p) log(p /(1 - p))
expit <- function(x) 1 / (1 + exp(-x))

# Fichier contenant les résultats
if(!file.exists("results")) dir.create("results")
output_file <- "results/sample_size_20191127.txt"

# Calcul de la taille d'échantillon
ssize <- function(alpha = .05, power = .8, m, p, r) {
  if (length(m) == 1) m <- c(m, m)
  if (length(p) == 1) p <- c(p, p)
  if (length(r) == 1) r <- c(r, r)
  coef_z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  nvar <- 1 / m[1] * p[1] * (1 - p[1]) + (m[1] - 1) / m[1] * (r[1] / 4)^2 +
    1 / m[2] * p[2] * (1 - p[2]) + (m[2] - 1) / m[2] * (r[2] / 4)^2
  coef_z * nvar / (p[2] - p[1])^2
}
p0 <- 0.5
p1 <- 0.7
r <- 0.4
m <- 4
n <- ssize(m = m, p = c(p0, p1), r = r)

# Output
sink(output_file)
cat("------------------- Taille d'échantillon -------------------\n\n")
cat(paste("Proportion moyenne groupe contrôle :", p0, "\n"))
cat(paste("Proportion moyenne groupe expérimental :", p1, "\n"))
cat(paste("Longeur d'intervalle pour les proportions :", r, "\n"))
cat(paste("Taille moyenne des familles :", m, "\n"))
cat(paste("Nombre de famille nécessaire (alpha = .05, power = .8) :", n, "\n"))
cat("\n\n")
sink()

# On transpose l'intervalle autour de 0.5 à une autre probabilité en conservant
# la même longueur d'intervalle sur l'échelle logit
r <- 0.4
p1 <- 0.7
expit(logit(p1) + logit(0.5 + c(-1, 1) * r / 2))

# Simulation de données
# n: nombre de grappes par groupe (nombre de famille par groupe)
# m: nombre moyen d'observations par grappe (nombre de proches contactés par 
#    famille)
# u: écart type du nombre d'observations par grappe
# p: proportions moyennes dans les groupes contrôle et expérimental
# s: écarts type des proportions sur l'échelle logit
# nsim: nombre de simulations
sim <- function(n, m, u, p, s, nsim = 10^3) {
  if (length(n) == 1) n <- c(n, n)
  if (length(m) == 1) m <- c(m, m)
  if (length(u) == 1) u <- c(u, u)
  if (length(p) == 1) p <- c(p, p)
  if (length(s) == 1) s <- c(s, s)
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
sim0_list <- sim(n = 27, m = 4, u = 0.5, p = 0.5, s = logit(0.7) / 2)
sim1_list <- sim(n = 27, m = 4, u = 0.5, p = c(0.5, 0.7), s = logit(0.7) / 2)

# Initialistation du calcul parallèle
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(lme4)))

# Modèle de régression logistique pour chaque simulation
fit <- function(z) {
  suppressMessages(glmer(
    y ~ grp + (1 | cluster), family = binomial, data = z,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10^5))
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
r0 <- rr(m0_list)
r1 <- rr(m1_list)

# Fin du calcul parallèle
stopCluster(cl)

# Output
sink(output_file, append = TRUE)
cat("----------------------- Simulations ------------------------\n\n")
cat("Nombre de simultations: 1'000 (par hypothèse)\n")
cat("Paramètres : n=27, m=4, p0=0.5, p1=0.7, r=0.4\n")
cat(paste("Proportion de fit singulier (par hypothèse):", pS0, pS1, "\n"))
cat(paste("Taux de rejet (niv=.05) sous l'hypothèse nulle:", r0, "\n"))
cat(paste("Taux de rejet (niv=.05) sous l'alternative:", r1, "\n"))
cat("\n\n")
sink()

# Session info
sink(output_file, append = TRUE)
cat("---------------------- Infos session -----------------------\n\n")
print(sessionInfo(), locale = FALSE)
sink()

# Sauvegarde des objets
sapply(ls(), function(x) format(object.size(get(x)), units = "auto"))
save.image("results/sample_size_20191127.RData", compress = "xz")

