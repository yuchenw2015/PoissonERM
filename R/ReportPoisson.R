#'Predict the Incidence Rate using New Exposure Data set
#'
#' With the provided control script prediction-user-input-sim.r, simulated exposure data set .csv, and the ERModPoisson result .Rdata from ERModPoisson() (must be located under the same folder),
#' PredictionPoisson() generates incidence rate prediction for new exposures using strudtual model (covariates are ignored).
#' The prediction curve is only generated when there is a significant relationship between the exposure and the endpoint,
#' though the summaries of observed incidence rate and the simulated exposure distribution are always generated.
#'
#' @param pathRunType A directory path to the working folder. Default value is getwd().
#' @param model.RData The saved modeling result from ERModPoisson() or from PredictionPoisson(). Default value is "myEnvironment.RData".
#' @param file.name The name for the generated .Rmd file. Default file name is NULL which will save the .Rmd file as "Poisson-Regression-Date&Time.Rmd".
#' @export
#' @returns The function does not return any specific object but several objects will be saved as global values during the run.
#' A .Rmd file will be saved and it can compile an analysis report (.html) via knitr.
#' @examples
#' \dontrun{
#' # Include prediction of simulated exposures in the report
#' folder.dir <- getwd()
#' ModelPoisson(pathRunType = folder.dir,
#'              user.input = "user-input.r")
#'
#' PredictionPoisson(pathRunType = folder.dir,
#'                   prediction.input = "prediction_user_input_sim.R",
#'                   model.RData = "myEnvironment.RData")
#'
#' ReportPoisson(model.RData = "myEnvironment.RData")
#'
#' # No prediction of simulated exposures in the report
#' #' folder.dir <- getwd()
#' ModelPoisson(pathRunType = folder.dir,
#'              user.input = "user-input.r")
#' ReportPoisson(model.RData = "myEnvironment.RData",
#'               file.name = "Report_no_pred.Rmd")
#'
#' }
#'
ReportPoisson <- function(pathRunType = getwd(), model.RData = "myEnvironment.RData", file.name = NULL){
  require(tidyverse)
  require(glue)
  if(is.null(file.name)){
    title <- paste0("Poisson-Regression-",
                    gsub(" ","-",gsub(":","-",date())),".Rmd")
  } else if(!grep(".Rmd", file.name)){
    title <- paste0("Poisson-Regression-",
                    gsub(" ","-",gsub(":","-",date())),".Rmd")
  } else{
    title <- file.name
  }
  setwd(pathRunType)
  load(file = model.RData, envir = .GlobalEnv)
  if(!exists("full.con")) full.con = NULL
  if(!exists("full.cat")) full.cat = NULL
  if(!exists("OR_tab")) OR_tab <- "Yes"
  if(!exists("OR_fig")) OR_fig <- "Yes"
  dir.create(paste(glob.wd, "ReportComponents", sep = "/"))
  report.endpoints <- names(lapply(endpName,names))
  report.start <- "Poisson regression on "
  for(jj in report.endpoints){
    report.start <- c(report.start,paste0(EndpointNameLatex(jj),", "))
  }
  report.start[length(report.start)] <- paste("and",report.start[length(report.start)] )
  report.start[length(report.start)] <- gsub(", ","",report.start[length(report.start)])
  report.start <- c(report.start, " using ", input.data.name)
  report.start <- paste0(report.start, collapse = "")
  Rmd_begin <- c("---",
                 paste0("title: \"", report.start,"\""),
                 "output:",
                 "  bookdown::html_document2:",
                 "  df_print: paged",
                 "html_document:",
                 "  df_print: paged",
                 "---",
                 "<style type=\"text/css\">",
                 "  body{ /* Normal  */",
                 "      font-size: 18px;",
                 "  }",
                 "td {  /* Table  */",
                 "    font-size: 15px;",
                 "}",
                 "h1.title {",
                 "  font-size: 38px;",
                 "  color: DarkRed;",
                 "}",
                 "h1 { /* Header 1 */",
                 "    font-size: 30px;",
                 "  color: DarkBlue;",
                 "}",
                 "h2 { /* Header 2 */",
                 "    font-size: 25px;",
                 "  color: DarkBlue;",
                 "}",
                 "h3 { /* Header 3 */",
                 "    font-size: 20px;",
                 "  font-family: \"Times New Roman\", Times, serif;",
                 "  color: DarkBlue;",
                 "}",
                 "code.r{ /* Code block */",
                 "    font-size: 18px;",
                 "}",
                 "pre { /* Code block - determines code spacing between lines */",
                 "    font-size: 18px;",
                 "}",
                 "</style>",
                 "```{r setup, include=FALSE}",
                 #knitr::opts_chunk$set()
                 "knitr::opts_chunk$set(fig.pos = \"H\", out.extra = \"\",echo = F)",
                 "library(tidyverse)",
                 "library(stringr)",
                 "library(kableExtra)",
                 "```",
                 " "
  )
  writeLines(Rmd_begin,con=paste(title,sep="/"))
  Introduction(title)
  Method(title)
  ObservedData(title)
  Result(title)
  Conclusion(title)
  claim <- c("  ","# Reference","This analysis was conducted using PoissonERM package.", "  ",
             "See more details here: https://yuchenw2015.github.io/PoissonERM-QuickStart.")
  write(claim,file=paste(title,sep="/"), append = T)
}
