# Estimate AIC, Delta-Deviance, and p-value
aic_ddev_p.func <- function(modelSummary){
  s <- c(modelSummary$coef[2,1], 
         modelSummary$deviance,
         modelSummary$null.deviance,
         modelSummary$deviance-modelSummary$null.deviance, 
         modelSummary$aic, 
         modelSummary$coef[2,4])
  names(s) <- c("Estimate", "Deviance", "nullDeviance", "Delta-D", "AIC", "pvalue")
  return(s)
} # aic_ddev_p.func
