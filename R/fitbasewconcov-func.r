# Fit base model with con cov
# ------------------------------------------------------------------------------
fitbasewconcov.func <- function(.dv, .time, .df, .cov){
  baseCon <- .df %>%
    select_at(.cov) %>%
    map(~ glm(.df[, .dv] ~ offset(log(.df[,.time])) + .x, data = .df, family = poisson, na.action = na.exclude)) %>%
    map(summary)
  print(baseCon)
  assign(x = "baseCon", value = baseCon, envir = globalenv())
} # fitbasewconcov.func
