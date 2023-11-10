# Fit base model with cat cov
# ------------------------------------------------------------------------------
fitbasewcatcov.func <- function(.dv, .time,.df, .cov){
  baseCat <- .df %>%
    select_at(.cov) %>%
    map(~ glm(.df[, .dv] ~ offset(log(.df[, .time])) + .x, data = .df, family = poisson, na.action = na.exclude)) %>%
    map(summary)
  print(baseCat)
  assign(x = "baseCat", value = baseCat, envir = globalenv())
} # fitbasewcatcov.func
