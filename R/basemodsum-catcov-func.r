# Summarize base model result for each cat cov
basemodsum_catcov.func <- function(.mod){
  baseCat_sum <- .mod %>%
    map_dfr(.f = aic_ddev_p.func) %>%
    as.data.frame %>%
    rownames_to_column
  baseCat_sum$rowname <- ind.cat.covs
  baseCat_sum_cat_pvals <- baseCat_sum
  colnames(baseCat_sum_cat_pvals) <- c("Parameters", "Estimate", "Deviance", "Null Deviance",
                                       "$\\Delta$D", "AIC", "pvalue")
  assign(x = "baseCat_sum_cat_pvals", value = baseCat_sum_cat_pvals, envir = globalenv())
  names(x = baseCat_sum)[names(x = baseCat_sum) == "rowname"] <- "Parameters"
  assign(x = "baseCat_sum_pvals", value = baseCat_sum, envir = globalenv())
  baseCat_sum$Estimate <- sapply(X = baseCat_sum$Estimate, FUN = function(.){
    if(abs(x = .) < 0.001){
      formatC(x = ., digits = 2, format = "e")
    } else{ # if
      formatC(x = signif(x = ., digits = 2), format = "f")
    } # else
  } # function(.)
  ) # sapply
  baseCat_sum$pvalue <- formatC(x = baseCat_sum$pvalue, digits = 4, format = "f")
  baseCat_sum$pvalue[baseCat_sum$pvalue == "0.0000"] <- "<0.0001"
  other.cols <- colnames(x = baseCat_sum)[colnames(x = baseCat_sum) != "Parameters" &
                                            colnames(x = baseCat_sum) != "Estimate" &
                                            colnames(x = baseCat_sum) != "pvalue"]
  baseCat_sum[other.cols] <- formatC(x = unlist(x = baseCat_sum[other.cols]), digits = 2, format = "f")
  baseCat_sum$`Delta-D`[baseCat_sum$`Delta-D` == "-0.00" | baseCat_sum$`Delta-D` == "0.00"] <- "|<0.01|"
  baseCat_sum$Parameters <- convert_express_latex.func(text_to_convert = baseCat_sum$Parameters)
  colnames(baseCat_sum) <- c("Parameters", "Estimate", "Deviance", "Null Deviance",
                             "$\\Delta$D", "AIC", "pvalue")
  baseCat_sum[colnames(x = baseCat_sum)] <- sapply(X = baseCat_sum[colnames(x = baseCat_sum)], FUN = as.character)
  print(baseCat_sum)
  write.table(x = baseCat_sum, file = "IndCorrCatCovsFit.tex", sep = " & ",
              eol = "\\\\\n", quote = F, row.names = F)
} # basemodsum_catcov.func
