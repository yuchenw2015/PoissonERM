# Distribution of Exposure (original and log)
# ------------------------------------------------------------------------------
get_exp_dist.func <- function(df){
    exp.names <- names(x = orig.exposureCov)
    exp.vals <- unname(obj = orig.exposureCov)
  if(log_exp == "Yes"&exists("log.exposureCov")){
    exp.names <- c(exp.names, names(x = log.exposureCov))
    exp.vals <- c(exp.vals, unname(obj = log.exposureCov))
  } 
  if(sqrt_exp == "Yes"&exists("sqrt.exposureCov")){
    exp.names <- c(exp.names, names(x = sqrt.exposureCov))
    exp.vals <- c(exp.vals, unname(obj = sqrt.exposureCov))
  } 
  pdist_expo <- map2(.x = exp.names, 
                     .y = exp.vals, function(x, y){
  val <- df[[x]]
  val <- val[!is.na(val)]
  bw <- 2 * IQR(val) / length(val)^(1/3)
  ggplot(data = df[!is.na(x = df[, x]), ]) +
    geom_histogram(mapping = aes(x = val), fill = "#0095FF", binwidth = bw, alpha = 0.5) +
    xlab(parse(text = y))
  } # function(x, y)
 ) # map2
  #arrange smartly
  arr.ncol <- ifelse(max(nchar(exp.vals))>35, 2,
                  ifelse(max(nchar(exp.vals))>25, 3, 4))
  arr.nrow <- ceiling(length(exp.vals)/arr.ncol)
  m2 <- arrangeGrob(grobs = pdist_expo, ncol = arr.ncol, nrow = arr.nrow)
  return(list(m2,arr.nrow/arr.ncol))
} # get_exp_dist.func
