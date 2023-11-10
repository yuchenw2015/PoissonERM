# Fit base model without exposure
# ------------------------------------------------------------------------------
fitbasewoexp.func <- function(.dv, .time ,.df){
  baseNULL <- glm(.df[, .dv] ~ offset(log(.df[,.time])), data = .df, family = poisson, na.action = na.exclude)
  summary(baseNULL)
  print(baseNULL)
  assign(x = "baseNULL", value = baseNULL, envir = globalenv())
} # fitbasewoexp.func
