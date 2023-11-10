# Function to summarize logistic regression
# ------------------------------------------------------------------------------
model_summary.func <- function(model){
  # Get summary table for estimate
  if(deparse(substitute(model)) == "fit.final"){
    file.names <- c("FinalModelSummary.tex", "FinalParamSummary.tex", "FinalModelParamSummary.tex")
  } else{ # if
    if(deparse(substitute(model)) == "fit.full"){
      file.names <- c("FullModelSummary.tex", "FullParamSummary.tex", "FullModelParamSummary.tex")
    } else{ # if
      file.names <- c("BaseModelSummary.tex", "BaseParamSummary.tex", "BaseModelParamSummary.tex")
    } # else
  } # else
  if(!LaTex.table) file.names <- gsub(".tex",".tsv",file.names)
  reg_tbl <- tidy(model, conf.int = TRUE, conf.method = "profile") %>%
    mutate(estimate = signif(estimate, sigdigit),
           statistic = signif(statistic, sigdigit),
           conf.low = signif(conf.low, sigdigit),
           conf.high = signif(conf.high, sigdigit),
           p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.4f", p.value)),
           ci = paste0("(", conf.low, ", ", conf.high,")")) %>%
    select_at(c("term","estimate","ci","statistic","p.value"))
  # Get summary table for odds ratio
  odds <- tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.method = "profile") %>%
    mutate(estimate = signif(estimate, sigdigit),
           conf.low = signif(conf.low, sigdigit),
           conf.high = signif(conf.high, sigdigit),
           ci = paste0("(", conf.low, ", ", conf.high,")"))
  odds.df <- data.frame(term = odds$term, or = odds$estimate, or.ci = odds$ci) %>%
    mutate_all(as.character)
  if(nrow(x = odds.df) > 1){
    odds.df <- odds.df %>%
      slice(-1)
  } # if
  # Combine esitmate and odds ratio
  comb <- left_join(reg_tbl, odds.df, by = "term")
  comb[is.na(comb)] <- "-"
  names(comb) <- c("Effects", "Estimate", "95\\% CI", "z value",
                   "Pr(>|z|)", "Odds Ratio", "95\\% CI of Odds Ratio")
  comb$Effects <- var_name_tidy.func(comb$Effects, tex = T)
  if(any(comb$Effects != "(Intercept)")){
    mod.terms <- comb$Effects[comb$Effects != "(Intercept)"]
    if(analyze_covs == "Yes"){
      con.covs <- names(x = attr(x = model$terms, which = "dataClasses"))[
        names(x = attr(x = model$terms, which = "dataClasses")) %in% var_name_tidy.func(names(x = full.con.t.es))] # con.covs
      cat.covs <- names(x = attr(x = model$terms, which = "dataClasses"))[
        names(x = attr(x = model$terms, which = "dataClasses")) %in% var_name_tidy.func(unname(obj = full.cat.lab.es))] # cat.covs
      con.plot.labs <- var_name_tidy.func(unlist(unname(obj = full.con.t.es)[names(x = full.con.t.es) %in% var_name_tidy.func(con.covs, tex = T)]), tex = T)
      cat.plot.labs <- sapply(X = cat.covs, simplify = F, USE.NAMES = F, FUN = function(.){
        paste(., levels(x = mod.df[, .])[2:length(x = levels(x = mod.df[, .]))], sep = " ") %>% var_name_tidy.func(tex = T)
      } # function(.)
      ) # sapply
    } # if
    comb$Effects[comb$Effects %in% mod.terms] <- sapply(X = comb$Effects[comb$Effects %in% mod.terms], FUN = function(.){
      if(analyze_covs == "Yes"){
        if(. %in% var_name_tidy.func(con.covs, tex = T)){
          if(con.model.ref=="Yes"){
            if(.%in% names(con.ref$ref)){
            convert_express_latex.func(
              text_to_convert = paste0(as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == .]),
                                       " (ref. to ", unlist(con.ref$ref[.]),")"))
            }else{
              convert_express_latex.func(
              text_to_convert = as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == .]))
            }# convert_express_latex.func
          }else{
            convert_express_latex.func(
            text_to_convert = as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == .]))
          }
          }else{
          if(length(which(gsub(var_name_tidy.func(unlist(cat.plot.labs), tex = T),
                               pattern = " ", replacement = "") == gsub(., pattern = " ", replacement = ""))) > 0){
            convert_express_latex.func(
              unlist(cat.plot.labs)[gsub(unlist(cat.plot.labs), pattern = " ", replacement = "") == gsub(., pattern = " ", replacement = "")]
            ) # convert_express_latex.func
          } else{ # if
            if(. == exp.met){
              convert_express_latex.func(
                as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == exp.met])
              ) # convert_express_latex.func
            } # if
          } # else
        }
    }else{ # if
      if(. %in% exp.met){
        convert_express_latex.func(
          as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == exp.met])
        ) # convert_express_latex.func
      } # if
    } # else # function(.)
    }
    ) # sapply
    comb[colnames(x = comb)] <- sapply(X = comb[colnames(x = comb)], FUN = as.character)
  }# if
  # Summary of model
  ms <- glance(model) %>%
    mutate(delta = null.deviance - deviance,
           df = df.null - df.residual,
           logLik = sprintf("%.2f", logLik),
           p.value = 1-pchisq(delta, df)) %>%
    mutate(delta = sprintf("%.2f", delta),
           p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.4f", p.value)),
           AIC = sprintf("%.2f", AIC)) %>%
    select_at(c("delta","df","p.value","logLik","AIC"))
  ms <- cbind(nobs(model), nrow(model$data), ms)
  names(ms) <- c("\\hline n", "N","$\\Delta$D", "df", "Pr(>Chisq)", "LogLik", "AIC \\hline")
  if(LaTex.table){
    write.table(x = ms, file = file.names[1], sep = " & ",
                eol = "\\\\\n", quote = F, row.names = F)
    write.table(x = comb, file = file.names[2], sep = " & ",
                eol = "\\\\\n", quote = F, row.names = F)
  }else{
    write.table(x = ms, file = file.names[1], sep = "\t",
                quote = F, row.names = F)
    write.table(x = comb, file = file.names[2], sep = "\t",
                quote = F, row.names = F)
  }

  ms2 <- as_tibble(x = ms) %>%
    rbind(colnames(ms), .)
  colnames(ms2) <- c(1:ncol(x = ms2))
  comb2 <- rbind(colnames(comb), comb)
  colnames(comb2) <- c(1:ncol(x = comb2))
  comb2 <- rbind(comb2, ms2)
  colnames(x = comb2) <- as.character(x = comb2[1, ])
  comb2 <- comb2[-1, ]
  if(LaTex.table){
    write.table(x = comb2, file = file.names[3], sep = " & ",
                eol = "\\\\\n", quote = F, row.names = F)
  }else{
    write.table(x = comb2, file = file.names[3], sep = "\t",
                quote = F, row.names = F)
  }

  return(list(modelsummary = ms, paramsummary = comb, parammodelsummary = comb2))
} # logit_summary.func
