# Find significant exposure covariates by p_val (user input)
# ------------------------------------------------------------------------------
find_sig_expcov.func <- function(.sum){
  # ascending order by default
  if(useDeltaD == FALSE){
    .sum <- .sum[order(x = .sum$pvalue), ]
    sigpreds <- vector()
    for(row in 1:nrow(.sum)){
      if(.sum[row, "pvalue"] < p_val){
        sigpreds <- append(sigpreds, .sum[row, "Parameters"])
      } # if
    } # for
  } else{ # if
    .sum <- .sum[order(x = .sum$`Delta-D`), ]
    sigpreds <- .sum[1,"Parameters"]
  } # else
  print(sigpreds)
  sigpreds <- sigpreds[1]
  sigpreds <- names(x = exposureCov.mod)[
    convert_express_latex.func(text_to_convert = unname(obj = exposureCov.mod)) %in% sigpreds
  ] # names(x = exposureCov.mod)
  assign(x = "sigpreds", value = sigpreds, envir = globalenv())
} # find_sig_expcov.func
