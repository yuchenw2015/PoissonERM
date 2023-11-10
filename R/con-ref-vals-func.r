# Calculate continuous cov reference values
# ------------------------------------------------------------------------------
con_ref_vals.func <- function(vars){
  if("TRTG" %in% colnames(x = obsdf)){
    if(vars %in% names(x = exposureCov)){
      df <- obsdf[!duplicated(x = obsdf$UID2) & obsdf$TRTG != "Placebo", ]
    } else{ # if
      df <- obsdf[!duplicated(x = obsdf$UID2), ]
    } # else
  } else{ # if
    df <- obsdf[!duplicated(x = obsdf$UID2), ]
  } # else
  refs <- list("med" = median(x = df[, vars], na.rm = T),
               "min" = min(df[, vars], na.rm = T),
               "max" = max(df[, vars], na.rm = T))
  return(refs)
} # con.ref.assign
