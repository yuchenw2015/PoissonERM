# Create modified sequences
# ------------------------------------------------------------------------------
mod_seq.func <- function(.from, .to, .by){
  vec <- seq(from = .from, to = .to, by = .by)
  if(tail(x = vec, 1) != .to){
    return(c(vec, .to))
  } else{ # if
    return(vec)
  } # else
} # seq.mod
