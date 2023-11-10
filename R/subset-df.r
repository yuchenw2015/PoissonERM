# Subset the data for each endpoint
# ------------------------------------------------------------------------------
subset.df.func <- function(){
  message(paste("Subsetting data based on ", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "...", sep = ""))
  if(analyze_covs == "Yes"){
    if("TRTG" %in% colnames(x = obsdf)){
      obsdf <<- obsdf[, c("TRTG", "UID2", unname(obj = full.cat.lab), names(x = full.con.t), dv, dvf, endpcolName, names(x = exposureCov), "Endpoint", EVDUR, add_col)]
    } else{ # if
      obsdf <<- obsdf[, c("UID2", unname(obj = full.cat.lab), names(x = full.con.t), dv, dvf, endpcolName, names(x = exposureCov), "Endpoint", EVDUR, add_col)]
    } # else
  } else{ # if
    if("TRTG" %in% colnames(x = obsdf)){
      obsdf <<- obsdf[, c("TRTG", "UID2", dv, dvf, endpcolName, names(x = exposureCov), "Endpoint", EVDUR, add_col)]
    } else{ # if
      obsdf <<- obsdf[, c("UID2", dv, dvf, endpcolName, names(x = exposureCov), "Endpoint", EVDUR, add_col)]
    } # else
  } # else
  mod.df <<- obsdf[obsdf[, endpcolName] == z, ] %>%
    filter_at(vars(names(x = exposureCov)), all_vars(!is.na(.)))

  ###Correct continuous variable with reference value
  if(exists("con.ref")&con.model.ref=="Yes"){
    if(con.ref$already.adjusted.in.data == F){
      for(vars in names(con.ref$ref)[names(con.ref$ref) %in% colnames(mod.df)])
        mod.df[,vars] <<- mod.df[,vars] - unlist(con.ref$ref[vars])
    }
    for(vars in names(x = full.con.t)[!names(x = full.con.t)%in%names(con.ref$ref)]){
      con.ref$ref <<- c(con.ref$ref, round(unlist(con.ref.df["med", vars]),2))
      names(con.ref$ref)[length(con.ref$ref)] <<- vars
      mod.df[,vars] <<- mod.df[,vars] - unlist(con.ref$ref[vars])
    }
  }

  message("Data subset complete")
  # Save data subset
  save(mod.df, file = paste0(gsub(pattern = " ",
                                  replacement = "_", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]),
                             "_data_subset.RData"))

}
