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

If see an error of "! System command 'Rcmd.exe' failed", try running Sys.setenv(R_REMOTES_STANDALONE="true") before installation.

To use this tool, you will need:

  - A control script (user input) and a data set to fit the model(s).
  - A control script (prediction input) and a simulated/external exposure data set to create additional predicted incidence rates. This is optional.

The Quick-Start tutorial of `PoissonERM` is here: https://yuchenw2015.github.io/PoissonERM-QuickStart

The Example repository is here: https://github.com/yuchenw2015/PoissonERM-Example



