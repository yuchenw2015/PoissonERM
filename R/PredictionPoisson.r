#'Predict the Incidence Rate using New Exposure Data set
#'
#' With the provided control script prediction-user-input-sim.r, simulated exposure data set .csv, and the ERModPoisson result .Rdata from ERModPoisson() (must be located under the same folder),
#' PredictionPoisson() generates incidence rate prediction for new exposures using strudtual model (covariates are ignored).
#' The prediction curve is only generated when there is a significant relationship between the exposure and the endpoint,
#' though the summaries of observed incidence rate and the simulated exposure distribution are always generated.
#'
#' @param pathRunType A directory path to the working folder. Default value is getwd().
#' @param prediction.input File path of the control script to source. Default value is NULL.
#' @param model.RData The saved modeling result from ERModPoisson(). Default value is "myEnvironment.RData".
#' @param save.name The name for the saved R object of the modeling and prediction result. Default value is model.Rdata which will overwirte the previously saved modeling results.
#' @export
#' @returns The function does not return any specific object but several objects will be saved as global values during the run.
#' The model result with the prediction results will be saved as save.name in the directory of pathRunType.
#' @examples
#' \dontrun{
#' #after run user-input.r manually
#' PredictionPoisson()
#'
#' #after setting the path to the working folder
#' #prediction-user-input-sim.r and the data set should be under the working folder already
#' PredictionPoisson(prediction.input = "prediction-user-input-sim.r", model.RData = "myEnvironment.RData")
#'
#' #prediction-user-input-sim.r and the data set should be under the working folder already
#' #the modeling object .RData will be overwritten
#' folder.dir <- getwd()
#' PredictionPoisson(pathRunType = folder.dir,
#' prediction.input = "prediction_user_input_sim.R",
#' model.RData = "myEnvironment.RData")
#'
#' #save the new modeling result with prediction as a new .RData file
#' #' folder.dir <- getwd()
#' PredictionPoisson(pathRunType = folder.dir,
#' prediction.input = "prediction_user_input_sim.R",
#' model.RData = "myEnvironment.RData",
#' save.name = "newResult.RData")
#'
#'}
PredictionPoisson <- function(pathRunType = getwd(), prediction.input = NULL, model.RData = "myEnvironment.RData", save.name = model.RData){
  setwd(pathRunType)
  if(!is.null(prediction.input)) source(prediction.input)
  load(file = model.RData,envir = .GlobalEnv)
  read.sim.data.func()
  default.units <- "in"  # when using "ggsave" function
  default.width <- 8.5  # when using "ggsave" function
  default.height <- 11  # when using "ggsave" function
  default.res <- 300 # Default resolution
  # Custom colour palette
  pfizerblue <- "#0093D0"
  cPalette <- c(pfizerblue,"firebrick3","forestgreen","darkviolet",
                "darkorange1","deeppink3","navyblue","olivedrab3","orange","saddlebrown",
                "black")
  # Custom shape palette
  sPalette <- c(16,17,18,15,0,1,2,3,4,6,7,8)
  # Define black and white ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 14))
  theme_bw2 <- theme_update(plot.margin = unit(c(0.25,1,0.25,1),"lines"))
  # Log 10 breaks
  log10.breaks <- c(0.000001,0.00001,0.0001,0.001,0.01,0.1,1,10,100,1000,
                    10000,100000)
  if(!exists("levels.grp")) levels.grp <- sort(unique(aucdf[,grp_colname]))
  if(!exists("labels.grp")) labels.grp <- levels.grp
  aucdf[,grp_colname] <- factor(aucdf[,grp_colname], levels = levels.grp, labels = labels.grp)

  for(zi in endpoints){
    z <<- zi
    end.dir.name <- gsub(pattern = " ",
                         replacement = "-",
                         x = names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))])
    setwd(dir = paste(pathRunType, end.dir.name, sep = "/"))
    load(list.files()[grep(".*\\.RData$",list.files())])
    try(expr = {
      flag.df <- mod.df

      setwd(dir = paste(pathRunType, end.dir.name, "Models", sep = "/"))
      load(list.files()[grep(".*base.mod.RData$",list.files())])
      #setwd(dir = paste0(pathRunType, end.dir.name))
      setwd(dir = paste(pathRunType, end.dir.name, sep = "/"))
      summ.base.mod <- summary(base.mod)
      sim.path <- paste(pathRunType, end.dir.name, "Prediction", sep = "/")
      if(!dir.exists(paths = sim.path)){
        message(paste("Creating folder for", names(endpName)[z == unname(obj = sapply(X = endpName, "[[", 1))], "base model prediction...", sep = " "))
        dir.create(path = sim.path)
      } # if
      setwd(dir = sim.path)
      capture.output(summ.base.mod,file = paste0("basemodsummary.tex"))
      exp.met <- broom::tidy(base.mod)$term[broom::tidy(base.mod)$term%in%names(exposureCov)]
      if(length(exp.met)!=1){
        Expo_to_across_expo_plot <- Obs_Expo_list[1]
      }else{
        #find the corresponding original-scale exposure name for the selected exposure metric in the model
        Expo_to_across_expo_plot <- ifelse(substr(exp.met, 2, nchar(exp.met))%in% names(orig.exposureCov), substr(exp.met, 2, nchar(exp.met)),
                                           ifelse(substr(exp.met, 5, nchar(exp.met))%in% names(orig.exposureCov), substr(exp.met, 5, nchar(exp.met)),
                                                  ifelse(exp.met %in% names(orig.exposureCov), exp.met, NULL)))
        if(!Expo_to_across_expo_plot %in% Obs_Expo_list){
          stop(paste0(Expo_to_across_expo_plot, " was not found in the simulation data."))
        } else{
          message(paste0(Expo_to_across_expo_plot, " will be the exposure metric (x) axis for ",
                         names(endpName)[unname(obj = sapply(X = endpName, "[[", 1))==z]))
        }
      }
      flag.df <- flag.df %>%
        mutate(EXPOBIN = as.numeric(paste0(Hmisc::cut2(flag.df[,Expo_to_across_expo_plot],
                                                       g = bin_n_obs,
                                                       levels.mean = TRUE))))
      if(length(exp.met)!=1){
        slp.flag <- 0
      }else{
        slp.flag <- base.mod %>%
          broom::tidy(conf.int = TRUE) %>%
          mutate(lo95.ci = signif(conf.low, sigdigit),
                 hi95.ci = signif(conf.high, sigdigit)) %>%
          dplyr::filter(term %in% exp.met) %>%
          mutate(sig = ifelse(lo95.ci*hi95.ci > 0, 1, 0)) %>%
          pull(sig)
      }

      param.tab <- base.mod %>%
        # Pull parameter estimates from base model
        broom::tidy(conf.int = TRUE) %>%
        mutate(
          # Calculate 95% confidence intervals for parameter estimate
          lo95.ci = signif(conf.low, sigdigit),
          hi95.ci = signif(conf.high, sigdigit),
          # Rename parameter labels
          term =
            ifelse(term == "(Intercept)","\\hspace{3mm}Intercept",
                   ifelse(term %in% exp.met, paste0("\\hspace{3mm} ", exposureCov[[which(names(exposureCov)==exp.met)]]),
                          ""
                   )) # if/else
        ) %>% # mutate
        # Convert to 3 significant digits
        mutate(
          estimate = as.character(signif(estimate, sigdigit)),
          summ.95ci = paste0("(",signif(lo95.ci, sigdigit),", ",signif(hi95.ci, sigdigit),")")
        ) %>% # mutate
        # Retain necessary columns
        dplyr::select(term,estimate,summ.95ci) %>%
        # Add a row label for "Population Parameter"
        add_row(term = "Population Parameter",estimate = "",summ.95ci = "",
                .before = 1)
      colnames(param.tab)[3] <- "95\\% CI"
      param.tab$term <- convert_express_latex.func(text_to_convert = param.tab$term)
      # Save table to .tex
      sink(paste0("basemod_param",".tex"))
      print(xtable::xtable(param.tab),type = "latex",
            sanitize.text.function = function(x){x},include.rownames = F,
            only.contents = T)
      sink()

      colnames(flag.df)[colnames(flag.df)==dv] <- "dv"
      colnames(flag.df)[colnames(flag.df)==EVDUR] <- "EVDUR"

      obs.events <- flag.df %>%
        ungroup() %>%
        group_by(EXPOBIN) %>%
        summarise(
          nid = length(UID2),
          totalevents = sum(dv, na.rm = T),
          totaltime = sum(EVDUR, na.rm = T)/365.25,
          inc.events.rate = 100*totalevents/totaltime
        ) # summarise

      colnames(flag.df)[colnames(flag.df)=="dv"] <- dv
      colnames(flag.df)[colnames(flag.df)=="EVDUR"] <- EVDUR

      # Calculate confidence intervals for Poisson rates
      obs.ci <- survival::cipoisson(k = obs.events$totalevents,time = obs.events$totaltime,
                                    p = 0.95,method = c("exact")) %>%
        as.data.frame(row.names = NULL)
      # Combine observed incidence and confidence intervals into single data frame
      obs.events.df <- obs.events %>%
        bind_cols(obs.ci) %>%
        mutate(
          CAVEBIN = as.numeric(paste0(EXPOBIN)),
          lower = 100*lower,
          upper = 100*upper
        ) # mutate

      # Model predicted incidence of events per x patient years for each Cavg bin
      # Create data frame of just Exposure and event time
      input.pred.df <- flag.df %>%
        select_at(c(intersect(names(exposureCov), names(sum_aucdf)), EVDUR)) %>%
        # Add a column specifying that these are observed values
        mutate(data = "obs")
      # Add simulated median Caverage for each dose followed-up for 1 year

      input.pred.df.sim <- sum_aucdf[,intersect(names(exposureCov), names(sum_aucdf))] %>%
        mutate(EVDUR = 365.25, #For one year
               data = sum_aucdf[,grp_colname]
        )
      colnames(input.pred.df.sim)[colnames(input.pred.df.sim)=="EVDUR"] <- EVDUR

      input.pred.df <- input.pred.df.sim %>%
        rbind(input.pred.df)# add_row

      # Generate model predictions for each observed Caverage
      pred.df <- predict.glm(base.mod,newdata = input.pred.df,
                             se.fit = TRUE) %>%
        as.data.frame() %>%
        mutate(
          Expo = input.pred.df[,Expo_to_across_expo_plot],
          data = input.pred.df[,"data"],
          logpred = fit-log(input.pred.df[, EVDUR]/365.25), # Substract the offset
          logpred.lo95 = logpred+qnorm(0.025)*se.fit,
          logpred.hi95 = logpred+qnorm(0.975)*se.fit,
          pred = 100*exp(logpred),
          pred.lo95 = 100*exp(logpred.lo95),
          pred.hi95 = 100*exp(logpred.hi95),
          pred.val = paste0(signif(pred, sigdigit)),
          pred.summ = paste0(signif(pred, sigdigit)," (",signif(pred.lo95, sigdigit),", ",
                             signif(pred.hi95, sigdigit),")")
        ) # mutate
      save(pred.df, file = "pred_expo.RData")
      # Plot incidence of events per 100 patient years for each CAVE bin
      p.ev <- NULL
      p.ev <- ggplot()
      # Observed incidence
      p.ev <- p.ev + geom_point(aes(x = EXPOBIN,y = inc.events.rate),
                                data = obs.events.df,size = 2,colour = "grey30")
      p.ev <- p.ev + geom_errorbar(aes(x = EXPOBIN,ymin = lower,
                                       ymax = upper),data = obs.events.df,size = 1,width = 0.1,colour = "grey30")
      # Only plot model predictions if slope was statistically significant
      if (slp.flag > 0) {
        # Model prediction for observed Cavg
        p.ev <- p.ev + geom_line(aes(x = Expo,y = pred),colour = cPalette[1],
                                 data = pred.df %>% dplyr::filter(data == "obs"),size = 1)
        p.ev <- p.ev + geom_ribbon(aes(x = Expo,ymin = pred.lo95,
                                       ymax = pred.hi95),data = pred.df %>% dplyr::filter(data == "obs"),
                                   alpha = 0.3,fill = cPalette[1])
        # Model prediction for simulated Cavg
        p.ev <- p.ev + geom_point(aes(x = Expo,y = pred),
                                  data = pred.df %>% dplyr::filter(data != "obs"),
                                  colour = cPalette[2],size = 3,shape = 18)
        p.ev <- p.ev + geom_errorbar(aes(x = Expo,ymin = pred.lo95,
                                         ymax = pred.hi95),data = pred.df %>% dplyr::filter(data != "obs"),
                                     colour = cPalette[2],width = 0.1,size = 1)
        # Labels for model prediction at simulated Cavg
        p.ev <- p.ev + geom_text(aes(x = Expo,y = pred.hi95*1.5,label = pred.val),
                                 data = pred.df %>% dplyr::filter(data != "obs"),colour = cPalette[2],size = 3)
      } # if
      # Axes
      # p.ev <- p.ev + scale_y_continuous("Incidence per 100 Patient Years")
      p.ev <- p.ev + scale_y_log10("Incidence per 100 Patient Years",
                                   breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                   label = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000))
      p.ev <- p.ev + scale_x_log10(parse(text = exposureCov[[which(names(exposureCov) == Expo_to_across_expo_plot)]]),
                                   breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                   label = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                   lim = c(min(pred.df$Expo[pred.df$Expo != 0])*0.95,max(pred.df$Expo)))
      p.ev
      # Save plot
      ggsave(plot = p.ev,filename = paste0("pred_incidence_across_exposure.png"),
             width = default.height,height = default.width*0.75,
             units = default.units)
      save(p.ev, file = "pred_incidence_across_exposure.RData")

      # Generate a figure of prediction exposures for each dose on the same
      # scale as incidence plot
      p.expo <- NULL
      p.expo <- ggplot()
      # Densities
      p.expo <- p.expo + ggridges::geom_density_ridges(aes(x = aucdf[,Expo_to_across_expo_plot],
                                                           y = aucdf[,grp_colname]),
                                                       scale = 0.75,size = 0.25,rel_min_height = 0.01,
                                                       data = aucdf)
      # Axes
      p.expo <- p.expo + scale_y_discrete("")
      p.expo <- p.expo + scale_x_log10(parse(text = exposureCov[[which(names(exposureCov) == Expo_to_across_expo_plot)]]),
                                       breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                       label = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                       lim = c(min(pred.df$Expo[pred.df$Expo != 0])*0.95,max(pred.df$Expo)))
      p.expo
      # Save plot
      ggsave(plot = p.expo,filename = "Expo_dist.png",
             width = default.height,height = default.width*0.333,
             units = default.units)
      save(p.expo,file = "Expo_dist.RData")

      # Remove axis label for x-axis before combining plots
      p.ev2 <- p.ev + scale_x_log10("",
                                    breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                    label = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000))
      p.ev2 <- p.ev2 + scale_x_log10(parse(text = exposureCov[[which(names(exposureCov) == Expo_to_across_expo_plot)]]),
                                     breaks = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                     label = c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000,300000,1000000,3000000),
                                     lim = c(min(pred.df$Expo[pred.df$Expo != 0])*0.95,max(pred.df$Expo)))
      # Combine plots together
      comb.plot <- cowplot::plot_grid(plotlist = list(p.ev2,p.expo),ncol = 1,align = "v",
                             rel_heights = c(3,2))

      # Save combined plot
      ggsave(plot = comb.plot,
             filename = paste0("model_prediction.png"),
             width = default.width,height = default.height*0.75,units = default.units)
      # Save combined plot for presentation
      ggsave(plot = comb.plot,
             filename = paste0("presmodel_prediction.png"),
             width = default.width,height = default.height*0.5,units = default.units)

      # Return pred.df to generate a table after all graphical summaries are
      # created
      if (slp.flag > 0) {
        table.label <- names(endpName)[unlist(sapply(endpName, FUN = function(x){x[1]}))==z]
        output.tab <- pred.df %>%
          dplyr::filter(data != "obs") %>% # Retain only simulated Caverages
          mutate(data = factor(data, levels = levels.grp, labels = labels.grp)) %>%
          dplyr::select(data,Expo,pred.summ) %>% # Retain only labels, Caverage and prediction
          arrange(data) %>%
          mutate(Expo = as.character(signif(Expo, sigdigit))) %>%# 3 significant digits
          data.table::as.data.table()

        colnames(output.tab)[colnames(output.tab)=="data"] <- "Group"
        colnames(output.tab)[colnames(output.tab)=="Expo"] <- convert_express_latex.func(text_to_convert = exposureCov[[Expo_to_across_expo_plot]])
        colnames(output.tab)[colnames(output.tab)=="pred.summ"] <- "Cumulative Incidence per 100 Patient Years (Mean [95\\% CI])"
        # Write to .tex
        if(LaTex.table){
          write.table(x = output.tab, file = "mod_pred.tex", sep = " & ",
                      eol = "\\\\\n", quote = F, row.names = F)
        }else{
          write.table(output.tab, "mod_pred.tsv", sep = "\t", row.names = F, quote = F)
        }

      }
      setwd(pathRunType)

    }, silent = F) # try
  }
  setwd(pathRunType)
  save.image(file = save.name)
}
