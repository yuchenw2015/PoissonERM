# Impute missing continuous covs if < 10% for all patients.
# Remove continous cov from list if missing > 10%.
# ------------------------------------------------------------------------------
impute.con.func <- function(){
  for(v in names(x = full.con.t)){
    p.miss <<- length(x = which(x = is.na(x = obsdf[[v]])))/length(x = obsdf[[v]])*100
    if(p.miss < p.icon & p.miss > 0){
      obsdf[[v]] <<- ifelse(test = is.na(x = obsdf[[v]]),
                           yes = eval(expr = parse(text = paste0("con.ref.df$", v, "$med"))),
                           no = obsdf[[v]])
    } else{ # if
      if(p.miss > p.icon){
        obsdf[[v]] <<- obsdf[[v]]
        drop.cov <<- convert_express_latex.func.cmpf(text_to_convert = unname(obj = full.con.t)[names(x = full.con.t) == v])
        full.con.t[v] <<- NULL
        message(drop.cov, " was dropped from the analysis because there was at least ", p.icon, "% missing considering all patients in the dataset")
      } else{ # if
        obsdf[[v]] <<- obsdf[[v]]
      } # else
    } # else
  } # for

}
