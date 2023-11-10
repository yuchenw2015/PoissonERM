# Calculate missing (N(%))
# ------------------------------------------------------------------------------
missing_pop.func <- function(x){
  tot <- length(x)
  miss <- sum(is.na(x))
  perc <- round(miss/tot*100, 1)
  return(paste0(miss, " (", perc, ")"))
} # missing_pop.func
