# Modified function for calculating geometric mean (CV)
# ------------------------------------------------------------------------------
geomeancv_pop.func <- function(x, digit = NULL){
  # If digit is not provided, use the maximum decimal place +1 in the data
  if(is.null(digit)){
    if(any(is.na(x)) == T){
      decimal = 0
    } # if
    decimal <- sapply(na.omit(x), count_dec.func)
    digit <- max(decimal) + 1
  } # if
  if(all(is.na(x)) == T){
    return("-")
  } else{ # if
    x.geo <- round(geom_mean.func(x, na.rm = T), digit)
    x.cv <- round(geom_cv.func(x, na.rm = T), 0)
    fmt <- paste0("%.", digit, "f")
    x.geo.fmt <- sprintf(fmt = fmt, x.geo)
    return(paste0(x.geo.fmt," (", x.cv, ")"))
  } # else
} # geomeancv_pop.func
