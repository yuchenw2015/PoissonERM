# Develop the base E-R model
# ------------------------------------------------------------------------------
base.mod.func <- function(){
  message("Developing base model...")

  # Create folder to save model object
  mod.path <<- paste(end.path[grep(pattern = end.dir.name, x = end.path)], "Models", sep = "/")
  if(!dir.exists(paths = mod.path)){
    message(paste("Creating folder for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "E-R models...", sep = " "))
    dir.create(path = mod.path)
  } # if
  setwd(dir = mod.path)

  # Fit base model without exposure
  fitbasewoexp.func(.dv = dv, .time = EVDUR, .df = mod.df)

  # Subset exposures list by by endpoint
  exposureCov.mod <<- exposureCov
  for(. in names(x = exposureCov)){
    if(z %in% lapply(full.desc.exposureCov.1, "[[", "end.p")[names(x = lapply(full.desc.exposureCov.1, "[[", "end.p")) == .][[1]]){
      exposureCov.mod[[.]] <<- exposureCov.mod[[.]]
    } else{ # if
      exposureCov.mod[[.]] <<- NULL
    } # else
  } # for

  # Fit base model with exposure
  fitbasewexp.func(.dv = dv, .time = EVDUR, .df = mod.df, .expcov = names(x = exposureCov.mod))

  # Summarize base model result for each exposure parameter
  basemodsum_exp.func(.mod = baseEXPO)

  # Find significant exposure covariates by p_val (user input)
  find_sig_expcov.func(.sum = baseEXPO_sum_pvals)

  # Determine base model and exposure metric, if there is one
  if(length(x = sigpreds) > 0){
    exp.met <<- sigpreds
    fitbasemod.func(.exp = exp.met)
    message(paste("Base model for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "includes", sigpreds, sep = " "))
  } else{ # if
    base.mod <<- baseNULL
    exp.met <<- NULL
    fitbasemod.func(.exp = "1")
    message(paste("Base model for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "is the null", sep = " "))
  } # else

  # Save base model
  save(base.mod, file = paste0(gsub(pattern = " ", replacement = "_", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "_base.mod.RData"))

}
