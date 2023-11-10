##################################
# Function to generating all the results for a given
# endpoint. This will be run for each endpoint
# will return a text file for the Results section and
# a file for the appendices.

build.endpoint.results.body <- function(endpoint_name = NULL){
  # ending function immediately if endpoint is NULL
  if(length(endpoint_name)==0) return("")
  # taking the given name for the subsections for each endpoint.
  endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
  endpoint_code <- endpName[names(endpName) %in% endpoint_name][[1]][1]
  # dont want bold text
  #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  endpoint_text_name_section <- endpoint_text_name
  sectionlabel <- gsub(" ","",endpoint_name)
  sectionlabel <- gsub("-","",sectionlabel)
  out_text_body <- c(
    "",
    paste0("## ",paste("Analysis of",endpoint_text_name_section)),
    paste0("\\label{",paste0("sec:",sectionlabel),"}"),
    "",
    EventNumberInterpretation(data=obsdf,endpoint_name),
    ""
  )
  endpoint_name <- gsub(" ","-",endpoint_name)
  out_text_body <- c(out_text_body," ",
                     make_pmx_table_chunk(table_path=paste("Demog-Sum",
                                                           paste0("Model_EventsByProtocol_endpoints_",
                                                                  endpoint_code,
                                                                  ifelse(LaTex.table, ".tex", ".tsv")),
                                                           sep = "/"),
                                          table_title=paste("Number of", endpoint_text_name, "(First Event)"),
                                          table_caption = "Percentage values refer to the number of patients.",
                                          LaTex = LaTex.table))
  out_text_body <- c(out_text_body," ",base_model_results_body(endpoint_name,endpoint_text_name))
  out_text_body <- c(out_text_body," ",base_model_prediction_body(endpoint_name,endpoint_text_name))
  out_text_body <- c(out_text_body," ",final_model_results_body(endpoint_name,endpoint_text_name))
  if(OR_fig == "Yes") out_text_body <- c(out_text_body," ",OR.model.body.fig(endpoint_name,endpoint_text_name))
  if(OR_tab == "Yes") out_text_body <- c(out_text_body," ",OR.model.body.tab(endpoint_name,endpoint_text_name))

  return(out_text_body)
}


# contains the model estimates for the final model
base_model_results_body <- function(endpoint_name,endpoint_text_name=NULL){
  path_to_output = paste(gsub(" ","-",endpoint_name),"Models",sep="/")
  # Exposure Summary
  model_title <- paste("Assessment of Exposure Metrics for",endpoint_text_name)
  if(!useDeltaD){
    model_caption <- paste("Each was assessed using a base model. The user specified the significance level to be p =",p_val,"for selection.")
  }else{
    model_caption <- paste("Each was assessed using a base model. The user specified to use $\\Delta D$ for selection.")
  }
  Exposure_estimates_output_body_table <- make_pmx_table_chunk(
    table_path=paste(path_to_output,ifelse(LaTex.table, "ExposureFit.tex", "ExposureFit.tsv"), sep="/"),
    table_title=model_title,
    table_caption=model_caption,
    LaTex = LaTex.table) # double width first column
  # removing objects to reduce clutter and incorrect usage
  rm(model_title,model_caption)

  model_title <- paste("Estimates from the Base Model for",endpoint_text_name)
  model_caption <- paste("Confidence Intervals calculated based the log-likelihood function (profile confidence interval). n = Number
of participants with event outcomes recorded. N = Total number of participants in the study.")
  Base_estimates_output_body_table <- make_pmx_table_chunk(
    table_path=paste(path_to_output,ifelse(LaTex.table, "BaseModelParamSummary.tex", "BaseModelParamSummary.tsv"),sep="/"),
    table_title=model_title,
    table_caption=model_caption,
    LaTex = LaTex.table)
  out_model_text <- c(Exposure_estimates_output_body_table, Base_estimates_output_body_table)
  return(out_model_text)
}

final_model_results_body <- function(endpoint_name,endpoint_text_name=NULL){
  path_to_output = paste(gsub(" ","-",endpoint_name),"Models",sep="/")
  # Final Model Estimates
  model_title <- paste("Estimates from the Final Model for",endpoint_text_name)
  model_caption <- "Confidence Intervals calculated based on the log-likelihood function (profile confidence interval). n = Number
of participants with event outcomes recorded. N = Total number of participants in the study."
  Final_estimates_output_body_table <- make_pmx_table_chunk(
    table_path=paste(path_to_output,ifelse(LaTex.table, "FinalModelParamSummary.tex", "FinalModelParamSummary.tsv"),sep="/"),
    table_title=model_title,
    table_caption=model_caption,
    LaTex = LaTex.table)
  # removing objects to reduce clutter and incorrect usage
  rm(model_title,model_caption)
  out_model_text <- c(Final_estimates_output_body_table)
  return(out_model_text)
}

base_model_prediction_body <- function(endpoint_name,endpoint_text_name=NULL){
  path_to_prediction = paste(gsub(" ","-",endpoint_name),"Prediction",sep="/")
  out_model_text <- ""
  if(file.exists(path_to_prediction)){
    if(file.exists(paste(path_to_prediction,ifelse(LaTex.table, "mod_pred.tex", "mod_pred.tsv"),sep="/"))){
      model_title <- paste("Incidence of", endpoint_text_name, "per 100 Patient Years")
      model_caption <- expo_pred_tab_caption
      Expo_pred_output_body_table <- make_pmx_table_chunk(
        table_path=paste(path_to_prediction,ifelse(LaTex.table, "mod_pred.tex", "mod_pred.tsv"),sep="/"),
        table_title=model_title,
        table_caption=model_caption,
        LaTex = LaTex.table)
      # removing objects to reduce clutter and incorrect usage
      rm(model_title,model_caption)
      out_model_text <- c(Expo_pred_output_body_table)
    }
    if(file.exists(paste(path_to_prediction, "model_prediction.png", sep="/"))){
      model_title <- paste("Incidence of", endpoint_text_name,
                           "per 100 Patient Years")
      model_caption <- ifelse(exists("Expo_pred_output_body_table"),
                              paste("Top: Gray circles and error bars are the observed mean and 95\\% CI, respectively, of the incidence of",
                                    endpoint_text_name, "(all types) per 100 patient years for each bin of the shown exposure metric",
                                    "in the analysis population", paste0("(n of bins = ", bin_n_obs, ")."),
                                    "The blue line, (and blue shaded area) are the model-predicted mean (and 95\\% CI) incidence per 100 patient years for the range of observed",
                                    "exposure metric in the analysis population. Red circles and error bars are the model-predicted mean and 95\\% CI, respectively, incidence of",
                                    endpoint_text_name, "per 100 patients years.",
                                    "Bottom:",expo_pred_tab_caption),
                              paste("Top: Gray circles and error bars are the observed mean and 95\\% CI, respectively, of the incidence of",
                                    endpoint_text_name, "(all types) per 100 patient years for each bin of the shown exposure metric",
                                    "in the analysis population", paste0("(n of bins = ", bin_n_obs, ")."),
                                    "Bottom:",expo_pred_tab_caption))
      Expo_pred_output_body_figure <- make_pmx_figure_chunk(
        figure_path=paste(path_to_prediction,"model_prediction.png",sep="/"),
        figure_title=model_title,
        figure_caption=model_caption)
      # removing objects to reduce clutter and incorrect usage
      rm(model_title,model_caption)
      out_model_text <- c(out_model_text, Expo_pred_output_body_figure)
    }

  }
  return(out_model_text)
}

#  incorporates the forest plot for the odds ratios
OR.model.body.fig <- function(endpoint_name,endpoint_text_name=NULL){
  if(is.null(full.con)&is.null(full.cat)) return()
  if(is.null(endpoint_text_name)){
    endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
    # dont want bold text
    #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  }
  path_to_output = paste(gsub(" ","-",endpoint_name),"OR",sep="/")
  fig_title <- paste("Odds Ratio for ",endpoint_text_name)
  fig_caption <- "95\\% confidence intervals are presented with the estimate."
  if(file.exists(paste(path_to_output,paste(endpoint_name,"odds.png",sep="-"),sep="/"))){
  OR_output_body_figure <- make_pmx_figure_chunk(
    figure_path=paste(path_to_output,paste(endpoint_name,"odds.png",sep="-"),sep="/"),
    figure_title=fig_title,
    figure_caption=fig_caption)
  }else{
    OR_output_body_figure <- ""
  }
  rm(fig_title,fig_caption)
  OR_output_body_figure
}

OR.model.body.tab <- function(endpoint_name,endpoint_text_name=NULL){
  if(is.null(full.con)&is.null(full.cat)) return()
  if(is.null(endpoint_text_name)){
    endpoint_text_name <- endpName[names(endpName) %in% endpoint_name][[1]][2]
    # dont want bold text
    #endpoint_text_name <- gsub("\\\\textbf","",endpoint_text_name)
  }
  path_to_output = paste(gsub(" ","-",endpoint_name),"OR",sep="/")
  tab_title <- paste("Risk Ratio for Covariates in First Incidence of",endpoint_text_name)
  tab_caption <- "Risk Ratio of scenarios based on final model. 95\\% confidence intervals are presented with the estimate."
  if(OR_con == "Yes"){
    OR_con_perc_text <- paste0(100*OR_con_perc, "th")
    tab_caption <- if(length(OR_con_perc_text) == 1){
      paste0(tab_caption,
             " The value in each continuous covariate is ",
             OR_con_perc_text, " percentile.")
    }else if(length(OR_con_perc) == 2){
      paste0(tab_caption,
             " The values in each continuous covariate are ",
             paste(OR_con_perc_text, collapse = " and "), " percentile.")
    }else{
      paste0(tab_caption,
             " The values in each continuous covariate are ",
             paste(paste(OR_con_perc_text[-length(OR_con_perc_text)],collapse = ", "),
                   OR_con_perc_text[length(OR_con_perc_text)],
                   sep = " and "), "percentile.")
    }
  }
  if(file.exists(paste(path_to_output,ifelse(LaTex.table, "OR_table.tex", "OR_table.tsv"),sep="/"))){
  	OR_output_body_tab <- make_pmx_table_chunk(
    table_path=paste(path_to_output,ifelse(LaTex.table, "OR_table.tex", "OR_table.tsv"),sep="/"),
    table_title=tab_title,
    table_caption=tab_caption,
    LaTex = LaTex.table)
  }else{
  	OR_output_body_tab <- ""
  }
  rm(tab_title,tab_caption)
  OR_output_body_tab
}

