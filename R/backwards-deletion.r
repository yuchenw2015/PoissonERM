# Full model backward selection
# ------------------------------------------------------------------------------
backwards.deletion.func <- function(){
  if(analyze_covs == "Yes"){
    if(exclude.exp.met.bd == "Yes"){
      cov.scope <<- full.var
    } else{ # if
      cov.scope <<- c(full.var, exp.met)
    } # else
  } else if(exclude.exp.met.bd == "No"){ # if
    cov.scope <<- exp.met
  }else{# else if
    cov.scope <<- c(NULL)
  }

  fit.final <<- model_backwards.func(fit.full,
                                     scope = cov.scope,
                                     remove_p = p_val_b,
                                     outfile = paste0(gsub(pattern = " ", replacement = "-", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "-Backwards-Deletion.log"))
  message(paste("Final model for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "developed", sep = " "))

  # Save final model
  save(fit.final, file = paste0(gsub(pattern = " ", replacement = "_", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "_fit.final.RData"))

}
