##############################################
#  Naming function for observed data figures/tables
#
# title_string <- "Cov-EDA/Weight-Gain-exp.pdf"
smart_title_detect <- function(title_string=NULL){
  title_string <- gsub("^(.*[\\\\/])*","",title_string)
  title_string <- gsub(".pdf","",title_string)
  title_string <- gsub(".png","",title_string)
  title_string <- gsub(".tex","",title_string)
  title_string <- gsub(".tsv","",title_string)
  # exposure distributions
  if(length(grep("exposuredist",title_string))>0){
    title_string <- str_split(title_string,"-")[[1]]
    title_string <- paste(title_string,collapse=" ")
    title_string <- gsub("exposuredist","Distribution of Exposures in Study",title_string)
  }
  # correlations
  if(length(grep("correlation",title_string))>0){
    title_string <- gsub("correlation-cat","Correlation of Categorical Covariates",title_string)
    title_string <- gsub("correlation-cont","Correlation of Continuous Covariates",title_string)
  }
  if(length(grep("-exp",title_string))>0){
    title_string <- gsub("-exp"," Exposures",title_string)
  }
  # correlations
  if(length(grep("Demographics",title_string))>0){
    title_string <- gsub("Demographics-categorical","Demographic Summaries of Categorical Covariates",title_string)
    title_string <- gsub("Demographics-continuous","Demographic Summaries of Continuous Covariates",title_string)
  }
  # hard coded
  title_string <- gsub("EventsByProtocol","Summary of Events By Endpoint",title_string)
  title_string <- gsub("GradesByProtocol","Summary of Adverse Event Grades By Endpoint",title_string)
  title_string <- gsub("ExposureByProtocol","Summary of Exposures",title_string)
  title_string <- gsub("IndCorrConCovsFit","Highly Correlated Continuous Covariates",title_string)
  title_string <- gsub("IndCorrCatCovsFit","Highly Correlated Categorical Covariates",title_string)
  title_string <- gsub("AETime","Time Until the Adserve Event",title_string)
  # any remaining hyphens
  title_string <- gsub("-"," ",title_string)
  return(title_string)
}

#smart_title_detect(title_string)
