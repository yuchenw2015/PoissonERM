# General function to count the number of decimal places
# ------------------------------------------------------------------------------
count_dec.func <- function(x){
  x <- as.character(x)
  float <- grepl(pattern = "\\.", x = x)
  if(T %in% float){
    n <- nchar(strsplit(x, "\\.")[[1]][2])
  } else{ # if
    n <- 0
  } # else
  return(n)
} # count_dec.func
