# Geometric CV
# ------------------------------------------------------------------------------
geom_cv.func <- function(x, na.rm = F){
  sqrt(exp(stats::sd(log(x), na.rm = na.rm)^2)-1)*100
} # geom_cv.func
