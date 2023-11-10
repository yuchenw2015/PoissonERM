# Create a workspace that defines labels for covariates
# ------------------------------------------------------------------------------
cov.labels.func <- function(){
  if(summary_covs == "Yes"&(!is.null(full.cat))){
    # Identify unique categorical covariate values
    covvaldf <<- obsdf %>%
      dplyr::select(names(x = full.cat)) %>%
      map(function(i) {
        vars <- unique(i) %>%
          sort()
      }) # map

    # Change character to numeric, if present
    if("character" %in% sapply(X = covvaldf, FUN = class)){
      for(i in 1:length(x = names(x = which(x = sapply(X = covvaldf, FUN = class) == "character")))){
        ch.var <- covvaldf[names(x = which(x = sapply(X = covvaldf, FUN = class) == "character"))][1]
        n.els <- length(x = ch.var[[1]])
        covvaldf[[names(ch.var)]] <- 0:(n.els-1)
      } # for
    } # if

    # Combine with reference data frame
    covlabdf <<- as.list(x = names(x = full.cat)) %>%
      {pmap_dfr(list(covvaldf, full.cat, .), function(val, lab, name) {
        data.frame("Covariate" = name, "Factor" = tail(lab, 1),
                   "Value" = as.numeric(paste0(val)), "Label" = head(lab, -1))
      })} # pmap_dfr

    # Merge covlabdf with obsdf
    for(cov in unique(x = covlabdf$Covariate)){
      obsdf <<- covlabdf %>%
        dplyr::filter(Covariate == cov) %>%
        mutate(Label = factor(x = Label, levels = Label)) %>%
        dplyr::select(-Covariate, -Factor) %>%
        dplyr::rename(!!cov := Value, !!as.character(x = covlabdf[covlabdf$Covariate == cov, "Factor"])[1] := Label) %>%
        {full_join(obsdf,.,by = cov)} %>%
        dplyr::select(-UQ(as.name(cov)))
    } # for

    # Convert to character class
    full.cat.lab <<- sapply(X = names(x = full.cat), FUN = function(x) paste0(as.character(x = covlabdf$Factor[covlabdf$Covariate == x])[1]))

  }else{
    covlabdf <<- c()
    full.cat.lab <<- c()
  }

  if(summary_covs == "Yes"&(!is.null(orig.con))){
    con.covs.names.list <<- c(names(x = full.con), names(x = exposureCov))
    con.covs.titles.list <<- c(full.con, exposureCov)
  } else{ # if
    con.covs.names.list <<- names(x = exposureCov)
    con.covs.titles.list <<- exposureCov
  } # else

  # Identify unique continuous covariate values
  con.covvaldf <- obsdf %>%
    dplyr::select(con.covs.names.list) %>%
    map(function(i) {
      vars <- unique(i) %>%
        sort()
    }) # map

  # Combine with reference data frame (con covs)
  con.covlabdf <<- as.list(con.covs.names.list) %>%
    {pmap_dfr(list(con.covs.titles.list, .), function(title, name){
      data.frame("Covariate" = name, "Title" = title)
    })} # pmap_dfr

  if(summary_covs == "Yes"){
    # Save created data frames to .Rdata
    save(covlabdf, con.covlabdf, file = "cov_labels.Rdata")
  } else{ # if
    # Save created data frames to .Rdata
    save(con.covlabdf, file = "cov_labels.Rdata")
  } # else

  message("Labels created for covariates")
}
