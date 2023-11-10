# Impute missing categorical covs if < 10% for all patients.
# Remove categorical cov from list if missing > 10%.
# ------------------------------------------------------------------------------
impute.cat.func <- function(){
  for(w in unname(obj = full.cat.lab)){
    p.miss <<- 0
    if(any(is.na(x = obsdf[[w]]))){
      p.miss <<- length(x = which(x = is.na(x = obsdf[[w]])))/length(x = obsdf[[w]])*100
    } else{ # if
      if("Missing" %in% obsdf[[w]]){
        p.miss <<- length(x = which(x = obsdf[[w]] == "Missing"))/length(x = obsdf[[w]])*100
      } # if
    } # else
    if(p.miss < p.icat & p.miss > 0){
      mode <<- names(x = which.max(x = table(obsdf[[w]])))
      if(any(is.na(x = obsdf[[w]]))){
        obsdf[[w]][is.na(x = obsdf[[w]])] <<- mode
        obsdf[[w]] <<- droplevels(x = obsdf[[w]])
      } else{
        if("Missing" %in% obsdf[[w]]){
          obsdf[[w]][obsdf[[w]] == "Missing"] <<- mode
          obsdf[[w]] <<- droplevels(x = obsdf[[w]])
        } # if
      } # else
    } else{ # if
      if(p.miss > p.icat){
        obsdf[[w]] <<- obsdf[[w]]
        full.cat.lab <<- full.cat.lab[unname(obj = full.cat.lab) != w]
        message(paste0(w, " was dropped from the analysis because there was at least ", p.icat, "% missing considering all patients in the dataset"))
      } else{ # if
        obsdf[[w]] <<- obsdf[[w]]
      } # else
    } # else
  } # for

}
