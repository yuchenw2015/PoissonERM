# Summarize base model result for each exposure parameter
basemodsum_exp.func <- function(.mod){
  baseEXPO_sum <- .mod %>%
    map_dfr(.f = aic_ddev_p.func) %>%
    as.data.frame %>%
    rownames_to_column
  exposure.labels.latex <- lapply(X = unname(obj = exposureCov.mod), FUN = function(.){
    convert_express_latex.func(text_to_convert = .)
  } # function(.)
  ) # lapply
  baseEXPO_sum$rowname <- exposure.labels.latex
  names(x = baseEXPO_sum)[names(x = baseEXPO_sum) == "rowname"] <- "Parameters"
  assign(x = "baseEXPO_sum_pvals", value = baseEXPO_sum, envir = globalenv())
  baseEXPO_sum$Estimate <- sapply(X = baseEXPO_sum$Estimate, FUN = function(.){
    if(abs(x = .) < 0.001){
      formatC(x = ., digits = sigdigit, format = "e")
    } else{ # if
      formatC(x = signif(x = ., digits = sigdigit), format = "f")
    } # else
  } # function(.)
  ) # sapply
  baseEXPO_sum$pvalue <- formatC(x = baseEXPO_sum$pvalue, digits = 4, format = "f")
  baseEXPO_sum$pvalue[baseEXPO_sum$pvalue == "0.0000"] <- "<0.0001"
  other.cols <- colnames(x = baseEXPO_sum)[colnames(x = baseEXPO_sum) != "Parameters" &
                                             colnames(x = baseEXPO_sum) != "Estimate" &
                                             colnames(x = baseEXPO_sum) != "pvalue"]
  baseEXPO_sum[other.cols] <- formatC(x = unlist(x = baseEXPO_sum[other.cols]), digits = sigdigit, format = "f")
  baseEXPO_sum$`Delta-D`[baseEXPO_sum$`Delta-D` == "-0.00" | baseEXPO_sum$`Delta-D` == "0.00"] <- "|<0.01|"
  colnames(baseEXPO_sum) <- c("Parameters", "Estimate", "Deviance", "Null Deviance",
                              "$\\Delta$D", "AIC", "pvalue")
  baseEXPO_sum[colnames(x = baseEXPO_sum)] <- sapply(X = baseEXPO_sum[colnames(x = baseEXPO_sum)], FUN = as.character)
  print(baseEXPO_sum)
  if(LaTex.table){
    write.table(baseEXPO_sum, file = "ExposureFit.tex", sep = " & ",
                eol = "\\\\\n", quote = F, row.names = F)
  }else{
    write.table(baseEXPO_sum, file = "ExposureFit.tsv", sep = "\t",
                quote = F, row.names = F)
  }

  assign(x = "baseEXPO_sum", value = baseEXPO_sum, envir = globalenv())
} # basemodsum_exp.func
