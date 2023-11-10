######################################################################
#Input the simulated data for incidence across exposure
######################################################################
read.sim.data.func <- function(){
  if(!exists("expo_pred_tab_caption")) expo_pred_tab_caption <<- "Predicted exposure metric for each dose are derived from simulated patients with randomly drawn random effect parameters as described by the final population PK model and body weights sampled from observations."

  if(!exists("sim_inc_expo_data")){
    stop("Simulation data set (across exposures) is not provided.")
  }else{
    aucdf <- read.csv(sim_inc_expo_data, na.strings = c(".", "NA", "-99", "-999"), header = T, stringsAsFactors = F)
    if(!exists("grp_colname")) grp_colname <<- "no_colname"
    if(!exists("bin_n_obs")) bin_n_obs <<- 5
    if(!exists("Obs_Expo_list")) Obs_Expo_list <<- names(orig.exposureCov)
    if(sum(!Obs_Expo_list %in% names(orig.exposureCov))>0) stop("Obs_Expo_list must be the exposure metrics used in the obs data. ")
    if(!exists("Sim_Expo_list")) Sim_Expo_list <<- Obs_Expo_list
    if(!exists("grp_colname_tab")) grp_colname_tab <<- "Label"
    if(grp_colname == "no_colname"){
      #if there is no group_by column, exposure will displyed as one signle group
      aucdf <- aucdf %>% mutate(no_colname = "All")
    }
    if(!exists("Center_Metric")) Center_Metric <<- "geomean"
    if(!exists("Center_Metric_name")){
      if(Center_Metric == "geomean"){
        Center_Metric_name <<- "geometric mean"
      }else{
        Center_Metric_name <<- "median"
      }

    }
    if(exists("filter_condition")) aucdf <- aucdf %>% dplyr::filter(eval(parse(text = filter_condition)))
    if(!grp_colname %in% colnames(aucdf)){
      stop("Required group_by column (grp_colname) is not found in the simulated data set (across exposures).")
    }
    if(sum(!Sim_Expo_list %in% colnames(aucdf))>0){
      stop("Required exposure column(s) (Metric_expo_sim) is not found in the simulated data set.")
    }
    aucdf <- aucdf %>%
      select_at(c(grp_colname, Sim_Expo_list))

    #rename the exposure columns in Sim data with the column names in Obs data
    for(i in 1:length(Sim_Expo_list)){
      colnames(aucdf)[colnames(aucdf)==Sim_Expo_list[i]] <- Obs_Expo_list[i]
    }

    #use Obs_Expo_list hereafter for all availabel exposure in Sim data
    Metric_names <<- Obs_Expo_list

    # remove 0 values in exposure (original scale)
    for(i in Obs_Expo_list){
      aucdf <- aucdf[aucdf[,i]>0,]
    }

    sum_aucdf <<- aucdf %>%
      pivot_longer(cols = Metric_names,
                   names_to = "Metric",values_to = "Value") %>%
      group_by_at(c("Metric", grp_colname)) %>%
      group_split() %>%
      map_dfr(function(metric.df) {
        a <- metric.df
        colnames(a)[colnames(a)==grp_colname] <- "grp_colname"
        b <- a %>%
          summarise(
            geomean.val = exp(mean(log(Value),na.rm = TRUE)),
            geocv.val = PKNCA::geocv(Value,na.rm = TRUE),
            median.val = median(Value,na.rm = TRUE),
            lo50.val = quantile(Value,probs = 0.25,na.rm = TRUE),
            hi50.val = quantile(Value,probs = 0.75,na.rm = TRUE),
            lo90.val = quantile(Value,probs = 0.05,na.rm = TRUE),
            hi90.val = quantile(Value,probs = 0.95,na.rm = TRUE)
          ) %>% # summarise
          mutate(Metric = metric.df$Metric[1],
                 grp_colname = unlist(metric.df[1, grp_colname]),
                 center.val = ifelse(Center_Metric == "geomean", geomean.val, median.val)) %>%
          dplyr::filter(Metric %in% Obs_Expo_list) %>%
          dplyr::select(grp_colname,Metric,center.val)

        colnames(b)[colnames(b)=="grp_colname"] <- grp_colname
        return(b)
      })%>% # map_dfr
      pivot_wider(names_from = Metric, values_from = center.val)

    if(log_exp == "Yes"){
      # Create log(exp metrics) for any variable, if not already present
      if(!all(names(x = log.exposureCov) %in% colnames(x = sum_aucdf))){
        mis.log.exp <- names(x = log.exposureCov)[!names(x = log.exposureCov) %in% colnames(x = sum_aucdf)]
        pres.log.exp <- substr(x = names(x = log.exposureCov)[names(x = log.exposureCov) %in% colnames(x =sum_aucdf)],
                               start = 2, stop = nchar(x = names(x = log.exposureCov)[names(x = log.exposureCov) %in%
                                                                                        colnames(x = sum_aucdf)]))
        l.exp <- map2(.x = mis.log.exp,
                      .y = names(x = orig.exposureCov)[!names(x = orig.exposureCov) %in% pres.log.exp],
                      .f = function(x, y){
                        if(y %in% colnames(sum_aucdf)){
                          dat <- sum_aucdf
                          dat[x] <- log(sum_aucdf[, y]+0.0001)
                          l.sum_aucdf <- dat[x]
                          return(l.sum_aucdf)
                        }else{
                          return(NULL)
                        }

                      } # function(x, y)
        ) # map2
        names(x = l.exp) <- mis.log.exp
        l.exp <- as.data.frame(x = l.exp[names(l.exp)[!sapply(l.exp, is.null)]])
        sum_aucdf <<- cbind(sum_aucdf, l.exp)
        Metric_names <<- c(Metric_names, colnames(l.exp))
      }else{}
    }else{}

    if(sqrt_exp == "Yes"){
      # Create log(exp metrics) for any variable, if not already present
      if(!all(names(x = sqrt.exposureCov) %in% colnames(x = sum_aucdf))){
        mis.sqrt.exp <- names(x = sqrt.exposureCov)[!names(x = sqrt.exposureCov) %in% colnames(x = sum_aucdf)]
        pres.sqrt.exp <- substr(x = names(x = sqrt.exposureCov)[names(x = sqrt.exposureCov) %in% colnames(x = sum_aucdf)],
                                start = 5, stop = nchar(x = names(x = sqrt.exposureCov)[names(x = sqrt.exposureCov) %in%
                                                                                          colnames(x = sum_aucdf)]))
        s.exp <- map2(.x = mis.sqrt.exp,
                      .y = names(x = orig.exposureCov)[(!names(x = orig.exposureCov) %in% pres.sqrt.exp)],
                      .f = function(x, y){
                        if(y %in% colnames(x = sum_aucdf)){
                          dat <- sum_aucdf
                          dat[x] <- sqrt(sum_aucdf[, y])
                          s.sum_aucdf <- dat[x]
                          return(s.sum_aucdf)
                        }else{
                          return(NULL)
                        }

                      } # function(x, y)
        ) # map2
        names(x = s.exp) <- mis.sqrt.exp
        s.exp <- as.data.frame(x = s.exp[names(s.exp)[!sapply(s.exp, is.null)]])
        sum_aucdf <<- cbind(sum_aucdf, s.exp)
        Metric_names <<- c(Metric_names, colnames(s.exp))
      }else{}
    }else{}

    sum_aucdf[,grp_colname] <- gsub(pattern = "_", replacement = " ", x = sum_aucdf[,grp_colname])

    Metric_names <<- Metric_names
    sum_aucdf <<- sum_aucdf
    aucdf <<- aucdf
  }

  ###########
  ##_CHECK_##
  ###########

  # High level view of the dataset
  head(aucdf)
  head(sum_aucdf)

  # Write input.data.name to .tex
  writeLines(gsub("_", "\\\\_", sim_inc_expo_data), "preddata.tex")
}
