# Read in analysis dataset
#-------------------------------------------------------------------------------
read.dataset.func <- function(){
  # Detect .csv files in inventory
  if(exists("input.data.name")){
    message("Reading in input dataset to R workspace...")
    csv_list <- input.data.name
  } else{ #if
    # Find working directory and determine number of csv files
    message("Searching for input dataset...")
    print(getwd())
    csv_list <- list.files(pattern = ".csv")
    listLength <- length(csv_list)
    # Determine that there is only one csv file in the step inventory.
    if(listLength == 1){
      message("Potential dataset located.")
      csv_list
    } else if (listLength == 0){ # if
      stop("No .csv files found")
    } else{ # if
      stop("There are too many .csv files in the inventory.")
    } # else
  } # else
  #------------------------------------------------------------
  # Exclude .csv files that do not
  # look like an input analysis dataset
  obsdf <- read.csv(csv_list, na.strings = c(".", "NA", "-99", "-999"),
                     header = T, stringsAsFactors = F) %>%
    dplyr::filter(is.na(C))
  print(dim(obsdf))
  print(class(obsdf))
  print(head(obsdf))
  obsdf <- obsdf[(obsdf[, endpcolName] %in% endpoints),]
  # Only keep user-specified studies
  if("PROJ" %in% colnames(obsdf)&exists("project.code")) obsdf <- obsdf[obsdf$PROJ %in% project.code, ]
  if("PROT" %in% colnames(obsdf)&exists("studies")) obsdf <- obsdf[obsdf$PROT %in% studies, ]
  if(nrow(obsdf)==0) stop("No observations found with the provided enpoints, PROJ, and study numbers.")
  if(!"UID2" %in% colnames(x = obsdf)){
    if("PROT" %in% colnames(obsdf)){
      obsdf$UID2 <- paste(obsdf[,"PROT"], obsdf[, pat.num], sep = " ")
    }else{
      obsdf$UID2 <- paste(obsdf[, pat.num], sep = " ")
    }
    if("PROJ" %in% colnames(obsdf)){
      obsdf$UID2 <- paste(obsdf[,"PROJ"], obsdf[, "UID2"], sep = " ")
    }else{
      obsdf$UID2 <- obsdf$UID2
    }
  } else{ # if
    message("UID2 was already present in the dataset. Care should be taken to ensure that UID2 is a unique patient identifier.")
  } # else

  if(summary_covs == "Yes"&log_covs == "Yes"&(!is.null(orig.con))){
    # Impute 0s to 0.001
    if(0 %in% as.matrix(x = obsdf[names(x = orig.con)])){
      obsdf[names(x = orig.con)][obsdf[names(x = orig.con)] == 0] <- 0.0001
    } # if
    # Create log(con cov) for any con cov if not already present
    if(!all(names(x = log.con) %in% colnames(x = obsdf))){
      mis.log.con <- names(x = log.con)[!names(x = log.con) %in% colnames(x = obsdf)]
      pres.log.con <- substr(x = names(x = log.con)[names(x = log.con) %in% colnames(x = obsdf)],
                             start = 2, stop = nchar(x = names(x = log.con)[names(x = log.con) %in%
                                                                              colnames(x = obsdf)]))
      l.con.covs <- map2(.x = mis.log.con,
                         .y = names(x = orig.con)[!names(x = orig.con) %in% pres.log.con],
                         .f = function(x, y){
                           obsdf[x] <- log(obsdf[, y])
                           l.obsdf <- obsdf[, x]
                           return(l.obsdf)
                         } # function(x, y)
      ) # map2
      names(x = l.con.covs) <- mis.log.con
      l.con.covs <- as.data.frame(x = l.con.covs)
      obsdf <- cbind(obsdf, l.con.covs)
    } # if
  } # if
  if(log_exp == "Yes"){
    # Impute 0s to 0.0001
    if(0 %in% as.matrix(x = obsdf[names(x = orig.exposureCov)])){
      obsdf[names(x = orig.exposureCov)][obsdf[names(x = orig.exposureCov)] == 0] <- 0.0001
    } # if
    # Create log(exp metrics) for any variable, if not already present
    if(!all(names(x = log.exposureCov) %in% colnames(x = obsdf))){
      mis.log.exp <- names(x = log.exposureCov)[!names(x = log.exposureCov) %in% colnames(x = obsdf)]
      pres.log.exp <- substr(x = names(x = log.exposureCov)[names(x = log.exposureCov) %in% colnames(x = obsdf)],
                             start = 2, stop = nchar(x = names(x = log.exposureCov)[names(x = log.exposureCov) %in%
                                                                                      colnames(x = obsdf)]))
      l.exp <- map2(.x = mis.log.exp,
                    .y = names(x = orig.exposureCov)[!names(x = orig.exposureCov) %in% pres.log.exp],
                    .f = function(x, y){
                      obsdf[x] <- log(obsdf[, y])
                      l.obsdf <- obsdf[, x]
                      return(l.obsdf)
                    } # function(x, y)
      ) # map2
      names(x = l.exp) <- mis.log.exp
      l.exp <- as.data.frame(x = l.exp)
      obsdf <- cbind(obsdf, l.exp)
    }else{}
  }else{}

  if(sqrt_exp == "Yes"){
    # Create log(exp metrics) for any variable, if not already present
    if(!all(names(x = sqrt.exposureCov) %in% colnames(x = obsdf))){
      mis.sqrt.exp <- names(x = sqrt.exposureCov)[!names(x = sqrt.exposureCov) %in% colnames(x = obsdf)]
      pres.sqrt.exp <- substr(x = names(x = sqrt.exposureCov)[names(x = sqrt.exposureCov) %in% colnames(x = obsdf)],
                              start = 5, stop = nchar(x = names(x = sqrt.exposureCov)[names(x = sqrt.exposureCov) %in%
                                                                                        colnames(x = obsdf)]))
      s.exp <- map2(.x = mis.sqrt.exp,
                    .y = names(x = orig.exposureCov)[!names(x = orig.exposureCov) %in% pres.sqrt.exp],
                    .f = function(x, y){
                      obsdf[x] <- sqrt(obsdf[, y])
                      s.obsdf <- obsdf[, x]
                      return(s.obsdf)
                    } # function(x, y)
      ) # map2
      names(x = s.exp) <- mis.sqrt.exp
      s.exp <- as.data.frame(x = s.exp)
      obsdf <- cbind(obsdf, s.exp)
    }else{}
  }else{}

  obsdf$Endpoint <- sapply(
    seq_len(length.out = nrow(x = obsdf)), function(.){
      names(x = endpName)[obsdf[., endpcolName] == unname(obj = sapply(X = endpName, "[[", 1))]
    } # function(.)
  ) # sapply
  # Convert response variable to factor
  obsdf[, dvf] <- factor(x = obsdf[, dv], levels = dv.factor$levels, labels = dv.factor$labels)

  if(!demog_grp_var_label%in%colnames(obsdf)) {
    demog_grp_var <<- "PROTnew"
    demog_grp_var_label <<- "PROTnew"
    obsdf <- obsdf %>% mutate(PROT = "All observations")
  }


  ###########
  ##_CHECK_##
  ###########
  # Check that a dataset has actually been pulled into the workspace
  # If so, then print the first 6 rows of that dataset
  obsdf <<- obsdf
  if(exists("obsdf")){
    message(paste0("Data frame dimensions: ", paste0(dim(obsdf), collapse = " ")))
  } else{ # if
    stop("Reading in dataset failed")
  } # else

  # Check class types
  sapply(names(x = obsdf), function(i){class(obsdf[[i]])})

  # High level view of the dataset
  head(obsdf)
  obsdf <<- obsdf
  # Write input.data.name to .tex
  writeLines(gsub("_", "\\\\_", csv_list), "inputdata.tex")
}
