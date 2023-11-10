# Create a workspace that defines reference values for covariates
# ------------------------------------------------------------------------------
cov.refs.func <- function(){
  if(summary_covs == "Yes"){
    # Categorical covariate reference values
    if(!is.null(full.cat)){
      cat.ref.list <<- sapply(X = names(x = full.cat), FUN = cat_ref_vals.func)
      # Combine with reference data frame
      # pmap_dfr
      covlabdf <<- as.list(x = names(x = full.cat)) %>%
        {pmap_dfr(list(covvaldf, full.cat, ., cat.ref.list), function(val, lab, name, ref) {
          data.frame("Covariate" = name,"Factor" = tail(lab, 1),
                     "Value" = as.numeric(paste0(val)),"Label" = head(lab, -1), "Ref" = paste0(ref))
        })}
    }else{
      covlabdf <<- c()
    }

    # Continuous covariate reference values
    if(!is.null(orig.con)){
      con.ref.df <<- as.data.frame(x = sapply(X = c(names(x = full.con), names(x = exposureCov)), FUN = con_ref_vals.func))
    }else{
      con.ref.df <<- as.data.frame(x = sapply(X = names(x = exposureCov), FUN = con_ref_vals.func))
    }
  } else{ # if
    # Continuous covariate reference values
    con.ref.df <<- as.data.frame(x = sapply(X = names(x = exposureCov), FUN = con_ref_vals.func))
  } # else

  message("Reference values calculated for covariates")

}
