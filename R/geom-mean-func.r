# Calculate geometric mean
# ------------------------------------------------------------------------------
geom_mean.func <- function(x, na.rm = F){
  if(na.rm == T){
    x <- stats::na.omit(x)
  } # if
  if(any(is.na(x))){
    as.numeric(NA)
  } else{ # if
    if(any(x == 0)){ # if
      0
    } else{ # if
      if(any(x < 0)){ # if
        # Protect from overflows by using the logarithm
        prod(sign(x))*exp(sum(log(abs(x)))/length(x))
      } else{
        exp(sum(log(x))/length(x))
      } # else
    } # else
  } # else
} # geom_mean.func
