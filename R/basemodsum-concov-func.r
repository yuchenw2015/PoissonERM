# Summarize base model result for each con cov
basemodsum_concov.func <- function(.mod){
  baseCon_sum <- .mod %>%
    map_dfr(.f = aic_ddev_p.func) %>%
    as.data.frame %>%
    rownames_to_column
  baseCon_sum_con_pvals <- baseCon_sum
  baseCon_sum_con_pvals$rowname <- ind.con.covs
  colnames(baseCon_sum_con_pvals) <- c("Parameters", "Estimate", "Deviance", "Null Deviance",
                                       "$\\Delta$D", "AIC", "pvalue")
  assign(x = "baseCon_sum_con_pvals", value = baseCon_sum_con_pvals, envir = globalenv())
  exposure.labels.latex <- lapply(X = unname(full.con.t[names(full.con.t) %in% ind.con.covs]), FUN = function(.){
    convert_express_latex.func(text_to_convert = .)
  } # function(.)
  ) # lapply
  baseCon_sum$rowname <- exposure.labels.latex
  names(x = baseCon_sum)[names(x = baseCon_sum) == "rowname"] <- "Parameters"
  assign(x = "baseCon_sum_pvals", value = baseCon_sum, envir = globalenv())
  baseCon_sum$Estimate <- sapply(X = baseCon_sum$Estimate, FUN = function(.){
    if(abs(x = .) < 0.001){
      formatC(x = ., digits = 2, format = "e")
    } else{ # if
      formatC(x = signif(x = ., digits = 2), format = "f")
    } # else
  } # function(.)
  ) # sapply
  baseCon_sum$pvalue <- formatC(x = baseCon_sum$pvalue, digits = 4, format = "f")
  baseCon_sum$pvalue[baseCon_sum$pvalue == "0.0000"] <- "<0.0001"
  other.cols <- colnames(x = baseCon_sum)[colnames(x = baseCon_sum) != "Parameters" &
                                            colnames(x = baseCon_sum) != "Estimate" &
                                            colnames(x = baseCon_sum) != "pvalue"]
  baseCon_sum[other.cols] <- formatC(x = unlist(x = baseCon_sum[other.cols]), digits = 2, format = "f")
  baseCon_sum$`Delta-D`[baseCon_sum$`Delta-D` == "-0.00" | baseCon_sum$`Delta-D` == "0.00"] <- "|<0.01|"
  colnames(baseCon_sum) <- c("Parameters", "Estimate", "Deviance", "Null Deviance",
                             "$\\Delta$D", "AIC", "pvalue")
  baseCon_sum[colnames(x = baseCon_sum)] <- sapply(X = baseCon_sum[colnames(x = baseCon_sum)], FUN = as.character)
  print(baseCon_sum)
  write.table(x = baseCon_sum, file = "IndCorrConCovsFit.tex", sep = " & ",
              eol = "\\\\\n", quote = F, row.names = F)
} # basemodsum_concov.func
