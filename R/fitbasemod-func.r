# Fit base model with most significant exposure metric
# ------------------------------------------------------------------------------
fitbasemod.func <- function(.exp){
  # Formula
  base.formula <- paste(dv, paste(c(paste0("offset(log(",EVDUR,"))"), .exp), collapse = " + "), sep = " ~ ")
  base.formula <- as.formula(base.formula)
  print(base.formula)
  # Fit model
  fit.base <- glm(base.formula, family = poisson, data = mod.df)
  summary(fit.base)
  assign(x = "base.mod", value = fit.base, envir = globalenv())
} # fitbasemod.func
