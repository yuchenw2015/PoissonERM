#########################################
# Text parsing and interpreting from the results of the
# modeling. Need to pull in the selected exposure, model estimates, covariate effects,
# methodology criteria (eg p-value thresholds)

EndpointNameLatex <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
  return(endpoint_text_name)
}

# This prints out a sentence with the selected exposure metric
# uses the base model for estimates.
ExposureSelection <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  #  endpoint_name <- gsub(" ","-",endpoint_name)
  load(determine_file_path(endpoint_name,"Models","fit.final"))
  # taking the names in the exposure list and comparing to the coefficients in the base model
  exposure_choice <- intersect(names(coef(fit.final)),names(exposureCov))
  # if no significant exposure metrics return
  if(length(exposure_choice)==0){
    p.value <- 1
  }else{
    p.value <- summary(fit.final)$coefficients[exposure_choice,"Pr(>|z|)" ]
  }
  if(length(exposure_choice)==0|p.value > 0.05) out.text = c("There was not a statistically significant exposure response relationship.")
  else {
    exposure_text <- convert_express_latex.func(exposureCov[[exposure_choice]])
    p.value <- ifelse(p.value>0.0001,paste0("=",sprintf("%.4f",round(p.value,4))),"<0.0001")
    estimate.exposure <- summary(fit.final)$coefficients[exposure_choice,"Estimate"]
    out.text.vector <- c("The selected best exposure metric was ",exposure_text," (p",p.value,").")
    out.text <- paste0(out.text.vector,collapse="")
  }
  return(out.text)
}

# figureing out the table levels
CovariateLevels <- function(full.cat){
  if(is.null(full.cat)) return()
  cov.test.levels = NULL
  for(kk in 1:length(full.cat)){
    levs.cat <- rev(full.cat[[kk]])
    cov.test.levels <- c(cov.test.levels,paste0(levs.cat[1],levs.cat[-1]))
  }
  return(cov.test.levels)
}

CovariateTableMatchup <- function(full.cat){
  if(is.null(full.cat)) return()
  cov.test.levels.df = NULL
  for(kk in 1:length(full.cat)){
    levs.cat <- rev(full.cat[[kk]])
    temp.df <- data.frame(
      Name = rep(levs.cat[1],length(levs.cat)-1),
      levels = levs.cat[-1],
      modelname = paste0(levs.cat[1],levs.cat[-1]))
    cov.test.levels.df <- rbind(cov.test.levels.df,temp.df)
  }
  return(cov.test.levels.df)
}

covariateReferenceLevel <- function(fit.final,full.cat){
  if(is.null(full.cat)) return()
  covs_all <- CovariateTableMatchup(full.cat)
  fin.cat.cov <- intersect(var_name_tidy.func(names(coef(fit.final)),tex=T),covs_all$modelname)
  covs_final_mod <- covs_all[covs_all$modelname %in% fin.cat.cov,]
  covs_all_levels <- covs_all[covs_all$Name %in% covs_final_mod$Name,]
  ref_levels <- setdiff(covs_all_levels$modelname,covs_final_mod$modelname)
  ref_df_levels <- covs_all_levels[covs_all_levels$modelname %in% ref_levels ,]
  ref_df_levels <- ref_df_levels %>% subset(levels != "Missing")
  colnames(ref_df_levels)[which(colnames(ref_df_levels)=="modelname")] <- c("model.reference.level")
  colnames(ref_df_levels)[which(colnames(ref_df_levels)=="levels")] <- c("RefLevel")
  ref_df_comb <- merge(covs_all_levels,ref_df_levels,by="Name")
  ref_df_comb <- ref_df_comb %>% subset(levels!="Missing")
  return(ref_df_comb)
}



InterpretCovariates <- function(endpoint_name = NULL){
  if(is.null(full.cat) & is.null(full.con)) return()
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  load(determine_file_path(endpoint_name,"Models","fit.final"))
  # taking the names in the exposure list and comparing to the coefficients in the base model
  covariates_cat_final <- intersect(var_name_tidy.func(names(coef(fit.final)),tex=T),CovariateLevels(full.cat))
  ref.val.cat <-  covariateReferenceLevel(fit.final,full.cat)
  final.covs.names <- c(as.character(unique(ref.val.cat$Name)))
  covariates_con_final <- intersect(var_name_tidy.func(names(coef(fit.final)),tex=T),names(full.con))
  n.cat.covs <- length(unique(ref.val.cat$Name))
  n.con.covs <- length(covariates_con_final)
  final.cont.names <- unlist(lapply(covariates_con_final,function(x)full.con[[x]]))
  final.cont.names <- convert_express_latex.func(final.cont.names)
  all.cov.names <- c(final.covs.names,final.cont.names)
  if(length(all.cov.names)>1){
    out.text <- c("There were ",length(all.cov.names)," covariates in the final model: ",paste(all.cov.names,collapse=", "),".")
  }
  if(length(all.cov.names)==1){
    out.text <- c("There was one covariate in the final model: ",paste(all.cov.names,collapse=", "),".")
  }
  if(length(all.cov.names)==0){
    # if exposure in the model
    if(length(coef(fit.final))>1){
      out.text <- c("There were no additional covariate effects identified.")
    }
    # if only an intercept
    if(length(coef(fit.final))==1){
      out.text <- c("There were no covariate effects identified.")
    }
  }
  return(paste(out.text,collapse=""))
}


count_events <- function(data=NULL,endpoint_name=NULL){
  if(is.null(data)) return()
  if(length(endpoint_name)==0) return("")
  # calculating the counts of the events
  count_events_df <- obsdf %>%
    # only performing the calculation for a given endpoint
    subset(Endpoint %in% endpoint_name) %>%
    subset(!duplicated(UID2)) %>%   # making sure the IDs are not counted twice.
    summarise(Total = n_distinct(UID2, na.rm = F),
              Events = n_distinct(UID2[across(dvf) == "Yes"], na.rm = T),
              NoEvents = n_distinct(UID2[across(dvf) == "No"], na.rm = T),
              Missing = n_distinct(UID2[is.na(across(dvf))]))
  return(count_events_df)
}

count_events_pct <- function(data=NULL,endpoint_name=NULL){
  # if the dataset is null or the endpoint is not correct then return()
  if(is.null(data)) return()
  if(length(endpoint_name)==0) return("")
  # count the number of events and divide by the total to get percentages
  # count_events outputs counts for total, yes, no, and missing
  count_events_df_pct <- count_events(data=data,endpoint_name) %>%
    mutate(Events = Events/Total*100,
           NoEvents = NoEvents/Total*100,
           Missing = Missing/Total*100)
  # return the percentages
  return(count_events_df_pct)
}

EventNumberInterpretation <- function(data=NULL,endpoint_name = NULL,include_crossref=TRUE){
  if(is.null(data)) return()
  if(length(endpoint_name)==0) return("")
  endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
  #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  event_sum <- count_events(obsdf,endpoint_name)
  event_sum_pct <- count_events_pct(obsdf,endpoint_name)
  out_text <- paste0("For the model of ",endpoint_text_name,
                     ", the analysis dataset included ",event_sum$Total, " study participants. ",
                     event_sum$Events," of them reported events and ",
                     event_sum$NoEvents," of them reported non-events. ")
  # if the crossreference ot the summary table is to be included
  # default is TRUE. reason for the argument is to suppress the crossrefernce
  # from the conclusions

  if(event_sum_pct$Events <= 2 | event_sum$Events < 20){
    out_text_warn <- c("Caution: The number of events used to build this model is very small. The results of this model should be interpreted with caution.")
    out_text <- paste(out_text,"",out_text_warn,"")
  }
  return(out_text)
}

InterpretEvents_execsum <- function(data=NULL,endpoint_name = NULL){
  if(is.null(data)) return()
  if(length(endpoint_name)==0) return("")
  endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
  #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  event_sum <- count_events(data=obsdf,endpoint_name)
  event_sum_pct <- count_events_pct(data=obsdf,endpoint_name)
  out_text <- paste0("The analysis dataset included a total of ",
                     paste0(event_sum$Events," events (",sprintf("%.2f",round(event_sum_pct$Events,2)),"\\% of study participants)."))
  return(out_text)
}

InterpretModel <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  # taking the given name for the subsections for each endpoint.
  endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
  # dont want bold text
  #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  out.text <- c(paste(c("A model was developed for ",
                        endpoint_text_name,"."),collapse=""),
                InterpretEvents_execsum(data=obsdf,endpoint_name),
                ExposureSelection(endpoint_name),
                InterpretCovariates(endpoint_name))
  return(out.text)
}


# getting the odds ratio for categorical covariates and printing a
# summary statement.

Covariate_OR_Interpret <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  endpoint_name <- gsub(" ","-",endpoint_name)
  # load final model results
  if(file.exists(paste(gsub(" ","-",endpoint_name),"Models",sep="/"))){
    load(determine_file_path(endpoint_name,"Models","fit.final"))
  }
  # load odds ratios
  if(file.exists(paste(gsub(" ","-",endpoint_name),"OR",sep="/"))){
    if(length(list.files(paste(gsub(" ","-",endpoint_name),"OR",sep="/")))>0){
      load(determine_file_path(endpoint_name,"OR","OR-results"))
      covlevels <- covariateReferenceLevel(fit.final,full.cat)
      if(is.null(covlevels)) return()
      if(nrow(covlevels)==0) return()
      odds_results <- oddsdf %>% subset(term %in% intersect(oddsdf$term,var_name_tidy.func(covlevels$modelname))) %>%
        dplyr::select(term,estimate,ci)  %>%
        dplyr::mutate(modelname=var_name_tidy.func(term,tex=T)) %>%
        left_join(covlevels,by="modelname")
      return(odds_results)
    }else{
      return()
    }
  }else{
    return()
  }

}

Continuous_Covariate_Interpret <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  endpoint_name <- gsub(" ","-",endpoint_name)
  # load final model results
  load(determine_file_path(endpoint_name,"Models","fit.final"))
  covariates_con_final <- intersect(var_name_tidy.func(names(coef(fit.final)),tex=T),names(full.con))
  if(is.null(covariates_con_final)) return()
  if(length(covariates_con_final)==0) return()
  final.cont.names <- unlist(lapply(covariates_con_final,function(x)full.con[[x]]))
  final.cont.names <- convert_express_latex.func(final.cont.names)
  name_variable <- data.frame(
    output.names = final.cont.names,
    Name = covariates_con_final
  )
  cont.results <- summary(fit.final)$coef
  # this coefficient table is class matrix, changing to dataframe
  cn <- colnames(cont.results)
  cont.results <- as.data.frame(cont.results)
  colnames(cont.results) <- cn
  cont.results$Name = var_name_tidy.func(rownames(cont.results),tex = T)

  cont.results <- cont.results %>% subset(Name %in% covariates_con_final)
  cont.results <- merge(cont.results,name_variable,by="Name")
  return(cont.results)
}



# if the logarithm transformed variable was in the model, then I want the name of the original
# variable for interpretations (eg log(age) in the model, but want to say that "age has...." rather than log(age))
find_nonLog <- function(covariate_label){
  if(is.null(covariate_label)) return()
  cov.model.name <- unlist(lapply(covariate_label,function(x)full.con[[x]]))
  if(substr(cov.model.name,1,4)=="Log("){
    cov_orig_var = substr(covariate_label,2,nchar(covariate_label))
    out_name <- unlist(lapply(cov_orig_var,function(x)full.con[[x]]))
    out_name <- convert_express_latex.func(out_name)
  }
  else{
    out_name = convert_express_latex.func(cov.model.name)
  }
  return(out_name)
}


ConclusionsInterpretCovariates <- function(endpoint_name=NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  endpoint_name <- gsub(" ","-",endpoint_name)
  cont_out <-  Continuous_Covariate_Interpret(endpoint_name)
  cov_OR_out <- Covariate_OR_Interpret(endpoint_name)
  cont_covs_concl <- cont_out$Name
  cat_covs_concl <- var_name_tidy.func(as.character(unique(cov_OR_out$Name)),tex=F)
  all_covs_concl <- c(cont_covs_concl,cat_covs_concl)
  if(length(all_covs_concl)==0){
    #    return("There were no statistically significant covariate effects.")
    return(NULL)
  }
  else{
    conc_text <- NULL
    for(cc in all_covs_concl){
      if(cc %in% cont_covs_concl){
        conc_estimate_process <- cont_out %>% subset(Name==cc)
        eff_dir <- ifelse(conc_estimate_process$Estimate>0,"positive","negative")
        eff_dir_interp <- ifelse(conc_estimate_process$Estimate>0,"increase","decrease")
        ind_cov_concl1 <- paste(c("The covariate ",as.character(conc_estimate_process$output.names)," was found to have a ",eff_dir," estimate "),collapse="")
        ind_cov_concl2 <- paste(paste(" where an increase in ", find_nonLog(cc)," is associated with a",eff_dir_interp,"in the probability of experiencing an event."),collapse="")
        ind_cov_concl <- paste(c(ind_cov_concl1,ind_cov_concl2),collapse = "")
        conc_text <- c(conc_text,ind_cov_concl)
      }
      if(cc %in% cat_covs_concl){
        iter.cat.res <- cov_OR_out %>% subset(Name == var_name_tidy.func(cc,tex=T))
        ref_level_text <- paste(iter.cat.res$Name[1],iter.cat.res$RefLevel[1]) # only need first row if multiple levels
        final_est_text <- paste(format(iter.cat.res$estimate,digits=3),iter.cat.res$ci)
        iter.text <- c("The covariate ",cc,", with the reference level of ",ref_level_text,", resulted in a statistically significant improvement in the model fit. The odds ratios and 95\\% confidence intervals for ", comma_and_syntax(paste(iter.cat.res$Name,iter.cat.res$levels))," are ",comma_and_syntax(final_est_text),", respectively.")
        conc_text <- c(conc_text,paste(gsub("~"," ", var_name_tidy.func(iter.text,tex=T)),collapse=""))
      }
    }
    return(conc_text)
  }
}

# Conclusions_endpoint <- function(endpoint_name=NULL){
# endpoint_concl_text <- c("","\\begin{itemize}")
# expselect <- paste("\\item",ExposureSelection(endpoint_name))
# intcov <- paste("\\item",InterpretCovariates(endpoint_name))
# concl_cov_int <- ConclusionsInterpretCovariates(endpoint_name)
#
# all_covs_concl <- count_covariates(endpoint_name)
#
# if(all_covs_concl==0) concl_cov_int = NULL
# if(all_covs_concl>0){
#   concl_cov_int <- c("\\begin{itemize}",paste("\\item",concl_cov_int),"\\end{itemize}")
# }
#
# endpoint_concl_text <- c(endpoint_concl_text,expselect,intcov,concl_cov_int,"\\end{itemize}","")
# return(endpoint_concl_text)
# }

Conclusions_endpoint <- function(endpoint_name=NULL){
  endpoint_concl_text <- c("")
  expselect <- paste("    - ",ExposureSelection(endpoint_name))
  counts_sum_statement <- paste("    - ",
                                EventNumberInterpretation(data=obsdf,endpoint_name,include_crossref = FALSE)
  )
  intcov <- InterpretCovariates(endpoint_name)
  if(!is.null(intcov)){
    intcov <- paste("    - ",InterpretCovariates(endpoint_name))
    intcov <- gsub("~", " ", intcov)
  }

  concl_cov_int <- ConclusionsInterpretCovariates(endpoint_name)
  if(length(concl_cov_int)>0){
    concl_cov_int <- c(paste("    - ",concl_cov_int))
  }
  if(length(concl_cov_int)==0){
    concl_cov_int <- NULL
  }
  endpoint_concl_text <- c(endpoint_concl_text,
                           expselect,
                           counts_sum_statement,
                           intcov,
                           concl_cov_int,"")
  return(endpoint_concl_text)
}




count_covariates <- function(endpoint_name=NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  endpoint_name <- gsub(" ","-",endpoint_name)
  # load final model results
  load(determine_file_path(endpoint_name,"Models","fit.final"))
  # load odds ratios
  load(determine_file_path(endpoint_name,"OR","OR-results"))
  covlevels <- covariateReferenceLevel(fit.final,full.cat)
  n.covs <- nrow(covlevels)
  return(n.covs)
}


determine_file_path <- function(endpoint_name = NULL, folder_name=NULL, desired_object = NULL){
  files_in_folder = list.files(paste(glob.wd,gsub(" ","-",endpoint_name),folder_name,sep="/"))
  if(desired_object == "fit.final"){
    look_str = "\\_fit\\.final\\.RData"
    model_file_name = files_in_folder[grep(look_str,files_in_folder)]
  }
  if(desired_object == "fit.full"){
    look_str = "\\_fit\\.full\\.RData"
    model_file_name = files_in_folder[grep(look_str,files_in_folder)]
  }
  if(desired_object == "base.mod"){
    look_str = "\\_base\\.mod\\.RData"
    model_file_name = files_in_folder[grep(look_str,files_in_folder)]
  }
  if(desired_object == "OR-results"){
    look_str = "\\_OR\\_forest\\_plot\\_results\\.RData"
    model_file_name = files_in_folder[grep(look_str,files_in_folder)]
  }

  model_file_name = paste(glob.wd,gsub(" ","-",endpoint_name),folder_name,model_file_name,sep="/")
  return(model_file_name)
}


