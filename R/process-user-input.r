process.user.input.func <- function(){
  #proj <<- project.code
  if(exists("project.code")){
    print(project.code) # data checking
  } else{
    print("No project code.")
  }

  if(exists("studies")){
    print(studies) # data checking
  } else{
    print("No studies codes.")
  }

  dvf <<- paste(dv, "_F", sep = "")
  dv.factor <<- setNames(object = list(dv.levels, dv.labels), nm = c("levels", "labels"))

  print(endpoints)  # data checking

  print(endpName)  # data checking

  print(input.data.name) # data checking
  #print(pred_data_sim)

  desc.exposureCov <<- sapply(X = desc.exposureCov.1, simplify = F, USE.NAMES = T, FUN = function(j){
    title <- j$title
    label <- j$label
    lab.title.list <- c(title, label)
    return(lab.title.list)
  } # function(j)
  ) # sapply

  orig.exposureCov <<- lapply(desc.exposureCov, function(x)x[[1]][1])

  if(!exists("log_exp")) {log_exp <<- "Yes"}
  if(!exists("sqrt_exp")) {sqrt_exp <<- "Yes"}
  if(!exists("con.model.ref")) {con.model.ref <<- "No"}
  if(!exists("add_col")) {add_col <<- NULL}

  full.desc.exposureCov.1 <<- desc.exposureCov.1
  exposureCov <<- orig.exposureCov
  names(x = exposureCov) <<- names(x = desc.exposureCov.1)
  desc.exposureCov.all <<- desc.exposureCov

  if(sqrt_exp != "No"){
    sqrt.desc.exposureCov.1 <<- desc.exposureCov.1
    sqrt.desc.exposureCov.1 <<- sapply(X = sqrt.desc.exposureCov.1, simplify = F, USE.NAMES = T, FUN = function(.){
      title = paste("Sqrt(", .$title, ")", sep = "")
      label = paste("Square-root of", .$label, sep = " ")
      end.p = .$end.p
      list(title = title, label = label, end.p = end.p)
    } # function(.)
    ) # sapply
    names(x = sqrt.desc.exposureCov.1) <<- paste("Sqrt", names(x = desc.exposureCov.1), sep = "")
    full.desc.exposureCov.1 <<- c(full.desc.exposureCov.1, sqrt.desc.exposureCov.1)

    sqrt.exposureCov <<- as.list(x = paste("Sqrt(", unname(obj = orig.exposureCov), ")", sep = ""))
    names(x = sqrt.exposureCov) <<- paste("Sqrt", names(x = desc.exposureCov.1), sep = "")
    exposureCov <<- c(exposureCov, sqrt.exposureCov)

    desc.exposureCov.sqrt <<- list()
    for(i in 1:length(desc.exposureCov)){
      desc.exposureCov.sqrt[[i]] <<- c(paste0("Sqrt(",desc.exposureCov[[i]][1],")"),
                                      paste("Square-root of",desc.exposureCov[[i]][2]))
    }
    names(x = desc.exposureCov.sqrt) <<- paste("Sqrt", names(x = desc.exposureCov), sep = "")
    desc.exposureCov.all <<- c(desc.exposureCov.all,desc.exposureCov.sqrt)

  }else{}

  if(log_exp != "No"){
    log.desc.exposureCov.1 <<- desc.exposureCov.1
    log.desc.exposureCov.1 <<- sapply(X = log.desc.exposureCov.1, simplify = F, USE.NAMES = T, FUN = function(.){
      title = paste("Log(", .$title, ")", sep = "")
      label = paste("Logarithm of", .$label, sep = " ")
      end.p = .$end.p
      list(title = title, label = label, end.p = end.p)
    } # function(.)
    ) # sapply
    names(x = log.desc.exposureCov.1) <<- paste("L", names(x = desc.exposureCov.1), sep = "")
    full.desc.exposureCov.1 <<- c(full.desc.exposureCov.1, log.desc.exposureCov.1)

    log.exposureCov <<- as.list(x = paste("Log(", unname(obj = orig.exposureCov), ")", sep = ""))
    names(x = log.exposureCov) <<- paste("L", names(x = desc.exposureCov.1), sep = "")
    exposureCov <<- c(exposureCov, log.exposureCov)

    desc.exposureCov.log <<- list()
    for(i in 1:length(desc.exposureCov)){
      desc.exposureCov.log[[i]] <<- c(paste0("Log(",desc.exposureCov[[i]][1],")"),
                                     paste("Logarithm of",desc.exposureCov[[i]][2]))
    }
    names(x = desc.exposureCov.log) <<- paste("L", names(x = desc.exposureCov), sep = "")
    desc.exposureCov.all <<- c(desc.exposureCov.all,desc.exposureCov.log)
  }else{}

  print(exposureCov) # data checking


  #################Covariates###################
  #full.cat <<- c()
  #full.cat.1 <<- c()
  #orig.con <<- c()
  #orig.con.1 <<- c()

  #Add full.cal and orig.con as NULL
  if(!exists("full.cat")) full.cat <<- c()
  if(!exists("orig.con")) orig.con <<- c()
  if(is.null(full.cat)) full.cat.1 <<-c()
  if(is.null(orig.con)) {
    orig.con.1 <<- c()
    log_covs <<- "No"
    con.model.ref <<- "No"
  }
  #summary_covs requires at least one covariates
  if(!exists("summary_covs")) {
    if(is.null(full.cat)&is.null(orig.con)){
      summary_covs <<- "No"
    }else{
      summary_covs <<- "Yes"
    }
  }else{
    if(is.null(full.cat)&is.null(orig.con)){
      summary_covs <<- "No"
    }
  }

  #analyze_covs requires summary_covs == "Yes"
  if(summary_covs == "No") analyze_covs <<- "No"
  if(!exists("analyze_covs")) {
    if(summary_covs == "Yes"){analyze_covs <<- "Yes"
    }else{
      analyze_covs <<- "No"
    }
  }

  #Defult to not use reference value in modeling
  if(!exists("con.model.ref")) {con.model.ref <<- "No"}
  if(con.model.ref=="No"&exists("con.ref")){
    rm("con.ref")
  }

  if(!exists("log_covs")) {log_covs <<- "No"}

  var.sum.only <<- NULL
  if(summary_covs == "Yes"){
    print(full.cat)  # data checking
    print(orig.con)  # data checking

    if(!is.null(full.cat)){
      full.cat <<- sapply(X = full.cat.1, simplify = F, USE.NAMES = T, FUN = function(.){
        levels <- .$levels
        label <- .$label
        lab.title.list <- c(levels, label)
        return(lab.title.list)
      } # function(.)
      ) # sapply
      var.sum.only <<- c(var.sum.only, names(full.cat.1)[names(full.cat.1) %>%
                                                          sapply(function(var){
                                                            sum(full.cat.1[[var]]$end.p %in% endpoints)==0
                                                          })])
    }

    if(!is.null(orig.con)){
      orig.con <<- lapply(X = orig.con.1, "[[", "title")
      full.con.1 <<- c(orig.con.1)
      full.con <<- c(orig.con)

      if(log_covs == "Yes"){
        log.con.1 <<- orig.con.1
        log.con.1 <<- sapply(X = log.con.1, simplify = F, USE.NAMES = T, FUN = function(.){
          title = paste("Log(", .$title, ")", sep = "")
          end.p = .$end.p
          list(title = title, end.p = end.p)
        } # function(.)
        ) # sapply
        names(x = log.con.1) <<- paste("L", names(x = orig.con), sep = "")

        log.con <<- lapply(X = log.con.1, "[[", "title")

        log.con <<- as.list(x = paste("Log(", unname(obj = orig.con), ")", sep = ""))
        names(x = log.con) <<- paste("L", names(x = orig.con), sep = "")

        full.con.1 <<- c(full.con.1, log.con.1)
        full.con <<- c(full.con, log.con)

        #add reference value if there is any
        if(exists("con.ref")){
          new.ref <<- c(con.ref$ref, round(log(unname(con.ref$ref[names(x = orig.con)])),2))
          names(new.ref) <<- c(names(con.ref$ref),paste0("L",names(x = orig.con)))
          con.ref$ref <<- new.ref
          print(con.ref)
        }
      }else{
        full.con.1 <<- c(orig.con.1)
        full.con <<- c(orig.con)
      }

      print(full.con) # data checking
      var.sum.only <<- c(var.sum.only, names(full.con.1)[names(full.con.1) %>%
                                                          sapply(function(var){
                                                            sum(full.con.1[[var]]$end.p %in% endpoints)==0
                                                          })])

    }else{
      full.con <<- c()
      full.con.1 <<- c()
    }
    if(analyze_covs == "No") var.sum.only <<- c(names(full.con.1), names(full.cat.1))
    if(length(var.sum.only)==0){
      message("All variables will be used in modeling for at least one endpoint.")
    }else if(analyze_covs == "No"|sum(!c(names(full.con.1), names(full.cat.1))%in%var.sum.only)==0){
      message("All variables will be used in demographic summary table only.")
      analyze_covs <<- "No"
    }else{
      message(paste(paste0(var.sum.only[var.sum.only%in%c(names(orig.con),names(full.cat.1))], collapse = ", "), "will be used in demographic summary table only."))
    }
    # taking list with variable descriptions and making logarithm
  } else{ # if
    if(exists("orig.con")){
      orig.con <<- c()
      full.con <<- c()
      message("`summary_covs` is set to `No`, while a definition exists for `full.con`. Please check user_input.R for any typos")
    } # if
    if(exists("full.cat")){
      full.cat <<- c()
      message("`summary_covs` is set to `No`, while a definition exists for `full.cat`. Please check user_input.R for any typos")
    } # if
  } # else

  full.con.t <<- full.con

  if(!exists("exclude.exp.met.bd")) {exclude.exp.met.bd <<- "Yes"}

  if(!exists("useDeltaD")) {useDeltaD <<- FALSE}

  # Significance level to determine exposure metric in the model
  # EXAMPLE:

  if(!exists("p_val")) {p_val <<- 0.01} # if p_val does not exists, then default

  if(length(p_val)!=1){p_val <<- 0.01}
  if(length(p_val)==1){
    if(!is.numeric(p_val)) p_val <<- 0.01
    if(p_val<=0 | p_val>=1) p_val <<- 0.01
  }

  # Significance level to determine final model covariates
  # during backwards deletion
  # default p-value is 0.01
  # checking if user input valid

  if(!exists("p_val_b")) {p_val_b <<- 0.01}

  if(length(p_val_b)!=1){p_val_b <<- 0.01}
  if(length(p_val_b)==1){
    if(!is.numeric(p_val_b)) p_val_b <<- 0.01
    if(p_val_b<=0 | p_val_b>=1) p_val_b <<- 0.01
  }

  # Number of simulation reps for final model
  # EXAMPLE: default = 100


  if(!exists("n.sims")) {n.sims <<- 100}

  if(length(n.sims)!=1){n.sims <<- 100}
  if(length(n.sims)==1){
    if(!is.numeric(n.sims)) n.sims <<- 100
    if(n.sims<1 | n.sims>1000000) n.sims <<- 100
  }

  # Percentage threshold used for continuous covariate imputation to the median
  # Should be treated as the level below which imputation will occur (while also >0)
  # Above which the covariate will be removed from consideration
  # EXAMPLE:
  # default = p.icon <<- 10

  if(!exists("p.icon")) {p.icon <<- 10}

  if(length(p.icon)!=1){p.icon <<- 10}
  if(length(p.icon)==1){
    if(!is.numeric(p.icon)) p.icon <<- 10
    if(p.icon<0 | p.icon>100) p.icon <<- 10
  }

  # Percentage threshold used for categorical covariate imputation to the mode
  # Should be treated as the level below which imputation will occur (while also >0)
  # Above which the covariate will be removed from consideration
  # EXAMPLE:
  # default = p.icat <<- 10


  if(!exists("p.icat")) {p.icat <<- 10}

  if(length(p.icat)!=1){p.icat <<- 10}
  if(length(p.icat)==1){
    if(!is.numeric(p.icat)) p.icat <<- 10
    if(p.icat<0 | p.icat>100) p.icat <<- 10
  }


  # Percentage threshold used for final endpoint determination
  # Should be treated
  # EXAMPLE:
  # default = p.yes.up <<- 90

  if(!exists("p.yes.up")){
    if(!exists("p.yes.low")){
      p.yes.up <<- 90
    }else{
      p.yes.up <<- 1-p.yes.low
      }
  }

  if(length(p.yes.up)!=1){p.yes.up <<- 90}
  if(length(p.yes.up)==1){
    if(!is.numeric(p.yes.up)) p.yes.up <<- 90
    if(p.yes.up<0 | p.yes.up>100) p.yes.up <<- 90
  }

  if(!exists("p.yes.low")) {p.yes.low <<- 100 - p.yes.up}

  if(length(p.yes.low)!=1){p.yes.low <<- 100 - p.yes.up}
  if(length(p.yes.low)==1){
    if(!is.numeric(p.yes.low)) p.yes.low <<- 100 - p.yes.up
    if(p.yes.low<0 | p.yes.low>100) p.yes.low <<- 100 - p.yes.up
  }
  p.yes.up <<- 100 - p.yes.low

  # if demog_grp_var is also a variable for modeling
  if(demog_grp_var %in% names(full.cat)){
    demog_grp_var_label <<- full.cat.1[[which(names(full.cat.1)==demog_grp_var)]]$label
  }else{
    demog_grp_var_label <<- demog_grp_var
  }

  if(!exists("OR_con_perc")){
    OR_con <<- "No"
  }else{
    OR_con <<- "Yes"
  }

  if(OR_con == "Yes"){
    if(sum(OR_con_perc >1|OR_con_perc < 0)>0) stop("The percentile(s) in OR_con_perc must be between 0 and 1.")
  }

  if(OR_con == "Yes"){
    if(length(OR_con_perc)<1) stop("At least one percentile number should be provided in OR_con_perc.")
  }

  if(!exists("OR_tab")) OR_tab <<- "Yes"
  if(!exists("OR_fig")) OR_fig <<- "Yes"

  if(!exists("LaTex.table")) LaTex.table <<- FALSE
  sigdigit <<- 3

}
