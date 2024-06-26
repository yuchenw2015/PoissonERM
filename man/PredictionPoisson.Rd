% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionPoisson.r
\name{PredictionPoisson}
\alias{PredictionPoisson}
\title{Predict the Incidence Rate using New Exposure Data set}
\usage{
PredictionPoisson(
  pathRunType = getwd(),
  prediction.input = NULL,
  model.RData = "myEnvironment.RData",
  save.name = model.RData
)
}
\arguments{
\item{pathRunType}{A directory path to the working folder. Default value is getwd().}

\item{prediction.input}{File path of the control script to source. Default value is NULL.}

\item{model.RData}{The saved modeling result from ERModPoisson(). Default value is "myEnvironment.RData".}

\item{save.name}{The name for the saved R object of the modeling and prediction result. Default value is model.Rdata which will overwirte the previously saved modeling results.}
}
\value{
The function does not return any specific object but several objects will be saved as global values during the run.
The model result with the prediction results will be saved as save.name in the directory of pathRunType.
}
\description{
With the provided control script prediction-user-input-sim.r, simulated exposure data set .csv, and the ERModPoisson result .Rdata from ERModPoisson() (must be located under the same folder),
PredictionPoisson() generates incidence rate prediction for new exposures using strudtual model (covariates are ignored).
The prediction curve is only generated when there is a significant relationship between the exposure and the endpoint,
though the summaries of observed incidence rate and the simulated exposure distribution are always generated.
}
\examples{
\dontrun{
#after run user-input.r manually
PredictionPoisson()

#after setting the path to the working folder
#prediction-user-input-sim.r and the data set should be under the working folder already
PredictionPoisson(prediction.input = "prediction-user-input-sim.r", model.RData = "myEnvironment.RData")

#prediction-user-input-sim.r and the data set should be under the working folder already
#the modeling object .RData will be overwritten
folder.dir <- getwd()
PredictionPoisson(pathRunType = folder.dir,
prediction.input = "prediction_user_input_sim.R",
model.RData = "myEnvironment.RData")

#save the new modeling result with prediction as a new .RData file
#' folder.dir <- getwd()
PredictionPoisson(pathRunType = folder.dir,
prediction.input = "prediction_user_input_sim.R",
model.RData = "myEnvironment.RData",
save.name = "newResult.RData")

}
}
