# Develop full model
# ------------------------------------------------------------------------------
full.model.func <- function(){
  if(analyze_covs == "Yes"){
    # Subset full.con.t by endpoint
    full.con.t.es <<- full.con.t
    for(. in names(x = full.con.t)){
      if(z %in% lapply(full.con.1, "[[", "end.p")[names(x = lapply(full.con.1, "[[", "end.p")) == .][[1]]){
        full.con.t.es[[.]] <<- full.con.t.es[[.]]
      } else{ # if
        full.con.t.es[[.]] <<- NULL
      } # else
    } # for

    # Choose between any strongly correlated continuous covariates (>= 0.6), if necessary
    if(exists(x = "corr.6.pairs")){
      if(sum(names(full.con.t.es)%in%names(corr.6.pairs))>1){
        # Grab the con cov names from full.con.t.es
        ind.con.covs <<- names(x = full.con.t.es)[names(x = full.con.t.es) %in% names(x = corr.6.pairs)]
        fitbasewconcov.func(ind.con.covs, .time = EVDUR,.dv = dv, .df = mod.df)
        basemodsum_concov.func(.mod = baseCon)
        # Decide which con covs to keep
        corr.names.con <<- names(x = corr.6.pairs)[names(x = corr.6.pairs) %in% ind.con.covs]
        winner.con <<- sapply(X = corr.names.con, simplify = F, USE.NAMES = T, FUN = function(.){
          el.name <<- .
          el.val <<- corr.6.pairs[names(x = corr.6.pairs) == .]
          pair.s.con <<- unlist(x = c(el.name, el.val))
          lower.p.con <<- min(baseCon_sum_con_pvals$pvalue[baseCon_sum_con_pvals$Parameters %in% pair.s.con])
          winner.con <<- baseCon_sum_con_pvals$Parameters[baseCon_sum_con_pvals$pvalue == lower.p.con]
          return(winner.con)
        } # function(.)
        ) # sapply
        winner.con.un <<- unlist(x = unname(obj = winner.con)[names(winner.con)==unname(obj = winner.con)])
        # Subset full.con.t.es
        carry.on.con <<- unname(obj = winner.con.un)[!duplicated(x = unname(obj = winner.con.un))]
        drop.con <<- names(corr.6.pairs)[!names(corr.6.pairs) %in% carry.on.con]
        full.con.t.es <<- full.con.t.es[!names(x = full.con.t.es) %in% drop.con]
        win.message.list <<- winner.con[names(winner.con) %in% drop.con]
        message(paste(names(win.message.list), "was dropped from the analysis in favor of", unname(obj = win.message.list), "\n", sep = " "))
      } # if
    }
    # subset full.cat.lab by endpoint
    full.cat.lab.es <<- full.cat.lab
    for(. in names(x = full.cat.lab)){
      if(z %in% lapply(full.cat.1, "[[", "end.p")[names(lapply(full.cat.1, "[[", "end.p")) == .][[1]]){
        full.cat.lab.es[[.]] <<- full.cat.lab.es[[.]]
      } else{ # if
        full.cat.lab.es <<- full.cat.lab.es[names(x = full.cat.lab.es) != .]
      } # else
    } # for
    # Remove any categorical cov from list if all events occur only in one category
    flag.print <<- 0
    for(b in unname(obj = full.cat.lab.es)){
      cat.events.check <<- table(obsdf[[b]][obsdf$Endpoint == names(x = sapply(X = endpName, "[[", 1))[unname(obj = sapply(X = endpName, "[[", 1)) == z]],
                                obsdf[[dv]][obsdf$Endpoint == names(x = sapply(X = endpName, "[[", 1))[unname(obj = sapply(X = endpName, "[[", 1)) == z]])[, "1"]
      cat.levels <<- length(x = cat.events.check)
      no.events <<- length(x = which(x = cat.events.check == 0))
      if(no.events == (cat.levels - 1)){
        flag.print <<- 1
        full.cat.lab.es <<- full.cat.lab.es[unname(obj = full.cat.lab.es) != b]
        message(paste(b, paste("was dropped from the analysis of",
                               names(x = sapply(X = endpName, "[[", 1))[unname(obj = sapply(X = endpName, "[[", 1)) == z],
                               "because all of its events occurred in only one of its categories", sep = " ")))
      } # if
    } # for
    if(!flag.print) message("None of the categorical covariates under current consideration have all of their events in only one category")

    # Choose between any strongly correlated categorical covariates (>= 0.6), if necessary
    if(exists(x = "chisq.t.6.pairs")){
      names.chisq.t.6.pairs <<- gsub(" ", "~", names(chisq.t.6.pairs))
      chisq.t.6.pairs <<- gsub(" ", "~", chisq.t.6.pairs) %>% as.list()
      names(chisq.t.6.pairs) <<- names.chisq.t.6.pairs
      if(sum(full.cat.lab.es%in%names(chisq.t.6.pairs))>1){
        # Grab the cat cov names from full.cat.lab.es
        ind.cat.covs <<- unname(obj = full.cat.lab.es)[unname(obj = full.cat.lab.es) %in% names(x = chisq.t.6.pairs)]
        fitbasewcatcov.func(ind.cat.covs, .time = EVDUR,.dv = dv, .df = mod.df)
        basemodsum_catcov.func(.mod = baseCat)
        # Decide which cat covs to keep
        corr.names.cat <<- names(x = chisq.t.6.pairs)[names(x = chisq.t.6.pairs) %in% ind.cat.covs]
        winner.cat <<- sapply(X = corr.names.cat, FUN = function(.){
          el.name <<- .
          el.val <<- chisq.t.6.pairs[names(x = chisq.t.6.pairs) == .]
          pairs.s.cat <<- unlist(x = c(el.name, el.val))
          lower.p.cat <<- min(baseCat_sum_cat_pvals$pvalue[baseCat_sum_cat_pvals$Parameters %in% pairs.s.cat])
          winner.cat <<- baseCat_sum_cat_pvals$Parameters[baseCat_sum_cat_pvals$pvalue == lower.p.cat]
          return(winner.cat)
        } # function(.)
        ) # sapply
        winner.cat.un <<- unlist(x = unname(obj = winner.cat))
        # Subset full.cat.lab.es
        carry.on.cat <<- unname(obj = winner.cat.un)[!duplicated(x = unname(obj = winner.cat.un))]
        drop.cat <<- names(chisq.t.6.pairs)[!names(chisq.t.6.pairs) %in% carry.on.cat]
        full.cat.lab.es <<- full.cat.lab.es[!unname(obj = full.cat.lab.es) %in% drop.cat]
        win.message.list.cat <<- winner.cat[names(winner.cat) %in% drop.cat]
        if(length(win.message.list.cat)>0){
          message(paste(names(x = win.message.list.cat), "was dropped from the analysis in favor of", unname(obj = win.message.list.cat), "\n", sep = " "))
        }
      }
    } # if
    # Formula
    colnames(mod.df) <<- var_name_tidy.func(text_to_convert = colnames(mod.df))

    full.var <<- var_name_tidy.func(text_to_convert = c(names(x = full.con.t.es), unname(obj = full.cat.lab.es)))

    full.formula <<- paste(dv, paste(c(paste0("offset(log(", EVDUR, "))"),full.var, exp.met),
                                    collapse = " + "), sep = " ~ ")
  } else{ # if
    # Formula
    full.formula <<- paste(dv, paste(c(exp.met,paste0("offset(log(", EVDUR, "))")),
                                    collapse = " + "), sep = " ~ ")
  } # else

  full.formula <<- as.formula(full.formula)

  # Fit model

  fit.full <<- glm(full.formula, family = poisson, data = mod.df)
  if(sum(names(x = fit.full$coefficients) != "(Intercept)") > 0){
    message(paste("Full model for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "developed", sep = " "))
  }else{
    message(paste("Full model for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "is the null", sep = " "))
  }

  # Save full model
  save(fit.full, file = paste0(gsub(pattern = " ", replacement = "_", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "_fit.full.RData"))

}
