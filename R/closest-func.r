# Find value within vector closest to specified value
# ------------------------------------------------------------------------------
closest.func <- function(xv, sv){
  xv2 <- as.numeric(x = xv)
  c.slope <- xv2[which(x = abs(x = xv2 - sv) == min(abs(x = xv2 - sv)))]
  choice <- names(x = xv)[unname(obj = xv) == c.slope]
  return(choice[1])
} # closest.func
