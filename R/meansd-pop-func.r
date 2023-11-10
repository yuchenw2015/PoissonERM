# Modified function for calculating mean (SD)
# ------------------------------------------------------------------------------
meansd_pop.func <- function(x, digit.mean = NULL, digit.sd = NULL){
  # If digit is not provided, use the maximum decimal place + 1 for mean, maximum decimal place + 2 for sd.
  if(is.null(digit.mean)){
    decimal <- sapply(na.omit(x), count_dec.func)
    if(any(is.na(x)) == T){
      decimal = 0
    } # if
    digit.mean <- max(decimal) + 1
  } # if
  if(is.null(digit.sd)){
    decimal <- sapply(na.omit(x), count_dec.func)
    if(any(is.na(x)) == T){
      decimal = 0
    } # if
    digit.sd <- max(decimal) + 1
  } # if
  if(all(is.na(x)) == T){
    return("-")
  } else{ # if
    x.mean <- round(mean(x, na.rm = T), digit.mean)
    x.sd <- round(sd(x, na.rm = T), digit.sd)
    fmt.mean <- paste0("%.", digit.mean, "f")
    fmt.sd <- paste0("%.", digit.sd, "f")
    x.mean.fmt <- sprintf(fmt = fmt.mean, x.mean)
    x.sd.fmt <- sprintf(fmt = fmt.sd, x.sd)
    return(paste0(x.mean.fmt," (", x.sd.fmt, ")"))
  } # else
} # meansd_pop.func
