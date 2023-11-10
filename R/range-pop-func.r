# Modified function for calculating range
# ------------------------------------------------------------------------------
range_pop.func <- function(x, digit = NULL){
  # if digit is not provided, use the maximum decimal place in the data
  if(is.null(digit)){
    decimal <- sapply(na.omit(x), count_dec.func)
    if(any(is.na(x)) == T){
      decimal = 0
    } # if
    digit <- max(decimal)
  } # if
  if(all(is.na(x)) == T){
    return("-")
  } else{ # if
    x1 <- round(min(x, na.rm = T), digit)
    x2 <- round(max(x, na.rm = T), digit)
    fmt <- paste0("%.", digit, "f")
    x1.fmt <- sprintf(fmt = fmt, x1)
    x2.fmt <- sprintf(fmt = fmt, x2)
    return(paste0("(", x1.fmt, ", ", x2.fmt, ")"))
  } # else
} # range_pop.func
