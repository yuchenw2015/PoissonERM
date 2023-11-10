# Fit base model with exposure
# ------------------------------------------------------------------------------
fitbasewexp.func <- function(.dv, .time, .df, .expcov){
  baseEXPO <- .df %>%
    select_at(.expcov) %>%
    map(~ glm(.df[, .dv] ~ offset(log(.df[, .time])) + .x, data = .df, family = poisson, na.action = na.exclude)) %>%
    map(summary)
  print(baseEXPO)
  assign(x = "baseEXPO", value = baseEXPO, envir = globalenv())
} # fitbasewexp.func
