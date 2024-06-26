# PoissonERM: An Automation Tool for Binary-Endpoint(s) ER Analysis via Poisson Regression

PoissonERM is a package-like automation tool for performing exposure-response (ER) analyses of binary endpoints using poisson regression to model the incidence rate over time. 

PoissonERM reads information from user's control scripts, loads specified data set, then conducts a full ER analysis including data cleaning, missing value imputation, exposure metrics (or scales) selection, variable selection, model summaries, and basic simulations. 

There is also the option to provide an additional dataset with simulated exposures from dosing regimens of interest. PoissonERM can predict the incidence rates for the simulated exposures and summarize the simulation results based on a given label e.g. treatment group or patient type. 

An automated .Rmd file will be generated to show the main modeling result (with or without additional prediction results), including an executive summary of the main results, modeling options specified by users, observed data summary (tables and figures), and complete model results.

The full section of analysis method can be found here: https://yuchenw2015.github.io/PoissonERM.

To install this tool 

    install.packages("devtools")
    library(devtools)
    install_github("yuchenw2015/PoissonERM",build = FALSE)

If see an error of "! System command 'Rcmd.exe' failed", try running 

    Sys.setenv(R_REMOTES_STANDALONE="true") 
    
before installation.

If see an error of "Bad credentials", you could unset the personal access token

    Sys.unsetenv("GITHUB_PAT")
    
or update your personal access token 

    gitcreds_set()

and select "2". See how to get a new access token here: https://docs.github.com/en/enterprise-server@3.9/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens. 


To use `PoissonERM`, you will need:

  - A control script (user input) and a data set to fit the model(s).
  - A control script (prediction input) and a simulated/external exposure data set to create additional predicted incidence rates. This is optional.

The Quick-Start tutorial of `PoissonERM` is here: https://yuchenw2015.github.io/PoissonERM-QuickStart

The Example repository is here: https://github.com/yuchenw2015/PoissonERM-Example

# Tips for Users

`PoissonERM` was firstly designed as an internal tool and is developed into a package-like version for external use. Although there are 3 main functions for completing the analysis, there are hundreds of functions behind them and none of them is supposed to be used by user directly. On the other hand, `ModelPoisson()` and `PredictionPoisson()` return values to the global environment and the modeling results are saved by saving the global environment into an RData object. Unlike most of the R packages, `PoissonERM` uses the global environment as a painting canvas to conduct most of the operations.

Below is several suggestions for users to avoid errors while using `PoissonERM`

- Make sure the control script is error-free by running the codes line by line. 
- Always clean the global environment via rm(list = ls(all = TRUE)) before running `ModelPoisson()`. `ModelPoisson()` sources information from the control script but it will everything available in the global environment, so `ModelPoisson()` will be affected by anything that was not provided in control script but remained in the global environment from the previous run. 
- Similarly, remember to clean the global environment before running `PredictionPoisson()` if the last operation is not `ModelPoisson()`. `PredictionPoisson()` sources information from the control script and loads essential modeling results from the saved RData.
- The modeling results (from `ModelPoisson()`) and the modeling results with additional prediction (from `PredictionPoisson()`) can be saved into two different RData files. However, while running `ReportPoisson()` with the modeling results from `ModelPoisson()`, make sure there is no "Prediction" folder under each endpoint folder. It is recommended to re-run the `ModelPoisson()` to clean the folders if the prediction results are not needed.
 - One project, one folder. The data sets and the control scripts are stored in the same directory as all analysis results. Using one folder for multiple projects may bring confusions.
 - The directory path must be an absolute path. After running `ModelPoisson()`, the working directory will be the provided `pathRunType`. If `pathRunType` was a relative path, it will not work in `PredictionPoisson()`. It is recommended to always use an absolute path, or always use `pathRunType = getwd()` (also the default value of `pathRunType`) in `PredictionPoisson()` or `ReportPoisson()` after running `ModelPoisson()`.

### Package Dependencies
compiler(>=4.1.1), stats(>=4.1.1), tidyverse(>=1.3.1), broom(>=0.7.8), gridExtra(>=2.3), R.utils(>=2.10.1), stringr(>=1.4.0), data.table(>=1.14.0), ggcorrplot(>=0.1.3), DescTools(>=0.99.42), cowplot(>=1.1.1), PKNCA(>=0.9.4), nortest(>=1.0.4), glue(>=1.4.2), Hmisc(>=4.5.0), xtable(>=1.8.4), survival(>=3.2.11), ggridges(>=0.5.3), kableExtra(>=1.3.4), bookdown(>=0.22).


