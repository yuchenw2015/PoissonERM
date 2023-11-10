Method <- function(title){
  folder.location = paste(glob.wd, "ReportComponents", sep = "/")
  imputation_approach <- paste0("- Missing data within covariates was imputed provided the percentage of missing values was $\\le$ ",p.icon,"\\% for continuous covariates and $\\le$ ",p.icat,"\\% for categorical covariates.",
                                " For continuous covariates, the median value was used for the imputed value. For categorical covariates, the mode was used for the imputed value. If the percentage of missing values was larger than the stated threshold, the covariates were excluded from consideration for the endpoint.")

  if(p.yes.low < .1){
    missing_threshold <- paste0("- An E-R analysis was performed for all endpoints even if the number of events was very low.")
  } else{
    missing_threshold <- paste0("- If the data does not allow for a full analysis due to low incidence of an endpoint (incidence <",min(p.yes.low,p.yes.up),"\\%), the E-R analysis for that endpoint was not conducted.")
  }

  if(useDeltaD){
    model_selection_methods <- paste0("- The exposure metric with the largest change in deviance $\\Delta D$ was selected for the base model for each endpoint analyzed, regardless of statistical significance.")
  }else{
    if(p_val > 0.9){
      model_selection_methods <- paste0("- The exposure metric with the smallest p-value was selected for the base model for each endpoint analyzed, regardless of statistical significance.")
    }else{
      model_selection_methods <- paste0("- An $\\alpha$ of ",p_val,", which equates to a change in deviance greater than ",sprintf("%.2f",round(qchisq(1-p_val,1),2)), " ($\\chi^2_{",1-p_val,"}$), was used to identify an exposure metric for each endpoint analyzed.")
      exposure_select <- c("If no exposure metric met the $D$ criteria, the base, full, and final models did not include an exposure metric. If more than one exposure metric met the $D$ criteria, the exposure metric with the smallest p-value was selected. This is based on the rationale that smaller p-values indicate a better fit.")
      model_selection_methods <- paste(model_selection_methods,exposure_select)
    }
  }
  covariate_selection_text <- paste(c("- The $\\chi^2$ test for the log-likelihood difference in deviance between models was used to judge whether one model had a better fit over another during the backward elimination using an $\\alpha$ of ", p_val_b,". When the removal of any of the remaining covariates results in a $\\Delta D$ equivalent to p-value less than ",p_val_b," the elimination process was stopped and the model was considered final."),collapse="")
  covariate_selection_text <- c(covariate_selection_text,
                                "Categorical covariate would be dropped in full model development if all events occurred in only one of its categories. If any of the covariates were highly correlated (e.g. AST and ALT) with a correlation coefficient $\\ge$ 0.6, then only one of the correlated covariates was tested further based on univariate Deviance.")
  method.text <- c("  ","# METHODS", "  ", "  ", missing_threshold, model_selection_methods)

  if(analyze_covs == "Yes") method.text <- c(method.text, imputation_approach, covariate_selection_text)
  method.text <- c(method.text, "  ", "  ", "For the full method chapter, please see https://yuchenw2015.github.io/PoissonERM")
  writeLines(method.text,con=paste(folder.location,"Method.txt",sep="/"))
  if(!is.null(file)) write(method.text,file=title,append=TRUE)
}
