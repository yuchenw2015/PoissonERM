#'Automate Logistic Regression Analysis with provided control script and data set
#'
#' With the provided control script user-input.r and the data set .csv (must be located under the same folder),
#' ModelPoisson() automate logistic regression analysis on multiple endpoints
#' including data cleaning, variable selection, model selection and result summary.
#'
#' @param pathRunType A directory path to the working folder. Default value is getwd().
#' @param user.input File path of the control script to source. Default value is NULL.
#' @param clean If clean is TRUE, all folders under the pathRunType directory will be removed. Default value is TRUE.
#' @param save.name The name for the saved R object of the modeling results. Default value is "myEnvironment.RData".
#' @export
#' @returns The function does not return any specific object but several objects will be saved as global values during the run.
#' The model result will be saved as save.name in the directory of pathRunType.
#' @examples
#' \dontrun{
#' #after run user-input.r manually
#' ModelPoisson()
#'
#' #after setting the path to the working folder
#' #user-input.r and the data set should be under the working folder already
#' ModelPoisson(user.input = "user-input.r")
#'
#' #user-input.r and the data set should be under the working folder already
#' folder.dir <- getwd()
#' ModelPoisson(pathRunType = folder.dir,
#'              user.input = "user-input.r",
#'              clean = TRUE,
#'              save.name = "model1.RData")
#' }
#'
ModelPoisson <- function(pathRunType = getwd(), user.input = NULL, clean = TRUE, save.name = "myEnvironment.RData"){
  if(substr(pathRunType,nchar(pathRunType),nchar(pathRunType))!="/") pathRunType <- paste0(pathRunType, "/")
  pathRunType <<- pathRunType
  setwd(pathRunType)
  if(!is.null(user.input)) source(user.input)
  #source(paste(pathRunType, "process-user-input.r", sep = ""))
  process.user.input.func()
  # remove the folders
  folders.output <<- list.dirs(pathRunType, full.names = T, recursive = T)
  if(clean&length(folders.output) > 1){ #the first dir is the step itself
    for(i in 2:length(folders.output)){
      unlink(folders.output[i],recursive = TRUE)
    }
    message("Output folders are removed before re-modeling.")
  }

  #source(paste(pathRunType, "plot-objects.r", sep = ""))
  plot.objects.func()
  #source(paste(pathRunType, "read-dataset.r", sep = ""))
  read.dataset.func()
  #source(paste(pathRunType, "cov-labels.r", sep = ""))
  cov.labels.func()
  #source(paste(pathRunType, "cov-refs.r", sep = ""))
  cov.refs.func()
  if(analyze_covs == "Yes"&(log_covs=="Yes")&(!is.null(orig.con))){
    #source(paste(pathRunType, "con-cov-form.r", sep = ""))
    con.cov.form.func()
  } # if
  #source(paste(pathRunType, "demog-sum.r", sep = ""))
  demog.sum.func()
  if(analyze_covs == "Yes"){
    #source(paste(glob.wd, "impute-con.r", sep = "/"))
    impute.con.func()
    #source(paste(glob.wd, "impute-cat.r", sep = "/"))
    impute.cat.func()
  } # if
  #source(paste(glob.wd, "dist-corr.r", sep = "/"))
  dist.corr.func()
  if(analyze_covs == "Yes"){
    corr.6.func()
  } # if

  #source(paste(glob.wd, "final-endpoints.r", sep = "/"))
  final.endpoints()
  if(length(x = endpName) > 0){
    #source(paste(glob.wd, "dir-setup.r", sep = "/"))
    dir.setup.func()
    # These scripts need to be looped over each endpoint
    for(zi in endpoints){
      z <<- zi
      end.dir.name <<- gsub(pattern = " ",
                           replacement = "-",
                           x = names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))])
      setwd(dir = end.path[grep(pattern = end.dir.name, x = end.path)])
      try(expr = {
        #source(paste(glob.wd, "subset-df.r", sep = "/"))
        subset.df.func()
        #source(paste(glob.wd, "base-mod.r", sep = "/"))
        base.mod.func()
        #source(paste(glob.wd, "full-model.r", sep = "/"))
        full.model.func()
        #source(paste(glob.wd, "backwards-deletion.r", sep = "/"))
        backwards.deletion.func()
        #source(paste(glob.wd, "model-sum.r", sep = "/"))
        try(model_summary.func(model = fit.full))
        try(model_summary.func(model = fit.final))
        try(model_summary.func(model = base.mod))
        message(paste("Summary tables (if applicable) for base, full, and final", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "models saved to file", sep = " "))

        if(analyze_covs == "Yes"&sum(!names(x = fit.final$coefficients) %in% exp.met)>1){
          OR.path <<- paste(end.path[grep(pattern = end.dir.name, x = end.path)], "OR", sep = "/")
          if(!dir.exists(paths = OR.path)){
            message(paste("Creating folder for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "OR plot...", sep = " "))
            dir.create(path = OR.path)
          } # if
          # Create OR plot
          setwd(dir = OR.path)
          oddsplot.func(.mod = fit.final)
          message(paste("OR plot created for final", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "model", sep = " "))
        } else{ # if
          message(paste("No Odds-Ratio results created for ",
                        names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))],
                        ".", sep = ""))
        } # else
        # remove(list = c())
      }, silent = F) # try
    } # for
  } else{ # if
    message(paste("No endpoints had greater than ", p.yes.low, "% occurrence of a yes/no category", sep = ""))
  } # else
  setwd(dir = glob.wd)
  save.image(file = save.name)
}

