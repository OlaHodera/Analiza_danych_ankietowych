library(binom)

logit_confint_delta <- function(successes, trials, alpha) {
  p_hat <- successes / trials
  logit_p_hat <- log(p_hat / (1 - p_hat))
  
  # Granice dla przekształcenia logitowego
  q <- qnorm(1 - (alpha/2))
  
  part_logit = sqrt(1/(trials*p_hat*(1 - p_hat)))
  
  logit_lcl <- logit_p_hat - q * part_logit
  logit_ucl <- logit_p_hat + q * part_logit
  
  
  # Odwrócenie przekształcenia logitowego do p
  lcl <- exp(logit_lcl) / (1 + exp(logit_lcl))
  ucl <- exp(logit_ucl) / (1 + exp(logit_ucl))
  
  return(c(lower = lcl, upper = ucl))
}


alpha <- 0.05
conf_level <- 1 - alpha

trials <- 1000
sample_trial <- sample(c(1, 0), trials, replace = TRUE)
successes <- sum(sample_trial)

delta_confint <- logit_confint_delta(successes, trials, alpha)
binom_confint <- binom.confint(successes, trials, conf_level, methods = c("exact", "asymptotic"))

cat("Metoda delta (logit):\n")
print(delta_confint)
cat("\nFunkcja binom.confint:\n")
print(binom_confint)


