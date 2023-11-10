# Modified function for calculating median
# ------------------------------------------------------------------------------
median_pop.func <- function(x, digit = NULL){
  # If digit is not provided, use the maximum decimal place + 1 in the data
  if(is.null(digit)){
    decimal <- sapply(na.omit(x), count_dec.func)
    if(any(is.na(x)) == T){
      decimal = 0
      } # if
    digit <- max(decimal) + 1
    } # if
  if(all(is.na(x)) == T){
    return("-")
  } else{ # if
    f <- round(x = median(x, na.rm = T), digits = digit)
    fmt <- paste0("%.", digit, "f")
    return(sprintf(fmt = fmt, f))
  } # else
} # median_pop.func
