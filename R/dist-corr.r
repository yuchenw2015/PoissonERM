# Covariate EDA
# ------------------------------------------------------------------------------
# Create folder to save EDA results
dist.corr.func <- function(){
  eda.path <<- paste(glob.wd, "Cov-EDA", sep = "/")
  if(!dir.exists(paths = eda.path)){
    message(paste("Creating folder for covariate EDA results...", sep = ""))
    dir.create(path = eda.path)
  } # if
  setwd(dir = eda.path)

  if(analyze_covs == "Yes"){
    # Compare correlations between continous variables
    if(length(full.con.t)>1&sum(!names(x = full.con.t)%in%var.sum.only)>1){
      corr.con <<- cor(obsdf %>%
                         distinct(UID2, .keep_all = T) %>%
                         select_at(names(x = full.con.t)[!names(x = full.con.t)%in%var.sum.only]) %>%
                         data.table::setnames(x = ., old = colnames(x = .),
                                              new = gsub(pattern = "~", replacement = " ",
                                                         x = as.character(x = unname(obj = full.con.t)))),
                       method = "spearman") %>% round(digits = 2)
      #corr.con <<- round(x = corr.con, digits = 2)
      # Plot
      pcorr.con <<- ggcorrplot::ggcorrplot(
        corr.con,
        type = "lower",
        outline.color = "black",
        hc.order = T,
        lab_size = 8,
        p.mat = abs(x = corr.con),
        sig.level = 0.59,
        legend.title = "Correlation"
      ) + theme(axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 8))
      ggsave(filename = "correlation-cont.png", plot = pcorr.con, width = 6, height = 5)
    }else{
      corr.con <<- NA
    }

    if(length(full.cat.lab)>1&sum(!names(x = full.cat.lab)%in%var.sum.only)>1){
      # Compare correlations between categorical variables (Cramer's V correlation)
      chisq.t <- expand.grid(x = unname(obj = full.cat.lab)[!names(full.cat.lab)%in%var.sum.only],
                             y = unname(obj = full.cat.lab)[!names(full.cat.lab)%in%var.sum.only]) %>%
        mutate_all(.funs = as.character)
      chisq.t <- cbind(chisq.t, cv = sapply(1:nrow(x = chisq.t), function(i){
        DescTools::CramerV(x = demog[[chisq.t$x[i]]], y = demog[[chisq.t$y[i]]])
      } # function(i)
      ) # sapply
      ) #cbind
      chisq.t[,c("x", "y")] <- chisq.t[,c("x", "y")] %>%
        map(~convert_express_latex.func(text_to_convert = .x)) %>%
        as.data.frame()
      chisq.t <- chisq.t %>% pivot_wider(names_from = y, values_from = cv) %>%
        as.data.frame()
      rownames(chisq.t) <- chisq.t$x
      chisq.t <- chisq.t[-1] %>% as.matrix
      # Plot
      if(nrow(x = chisq.t) > 1){
        pcorr.cat <<- ggcorrplot::ggcorrplot(
          chisq.t,
          type = "lower",
          outline.color = "black",
          hc.order = T,
          p.mat = abs(chisq.t),
          sig.level = 0.59,
          legend.title = "Correlation"
        )
        ggsave(filename = "correlation-cat.png", plot = pcorr.cat, width = 6, height = 5)
      } # if
      chisq.t <<- chisq.t
    }else{
      chisq.t <<- NA
    }
  } # if

  # Distribution of exposure (original and log)
  exp_dist_total.func(df = demog, groups = demog_grp_var_label)

  if(EVDUR %in% colnames(x = obsdf)){
    # Time of first AE for safety endpoints
    aetime <- map2(.x = endpoints, .y = names(endpName), function(i, j){
      tmp <- obsdf[obsdf[, endpcolName] == i & obsdf[, dvf] == "Yes", ]
      colnames(tmp)[colnames(tmp) == EVDUR] <- "EVDUR"
      ggplot(tmp, aes(x = EVDUR)) +
        geom_histogram(fill = "#0095FF", binwidth = 30, alpha = 0.5) +
        xlab(paste0("Time of AE Occurence", ifelse(exists("EVDUR.unit"), paste0(" (", EVDUR.unit, ")"), paste0(" (", EVDUR, ")")))) +
        ggtitle(j) +
        theme(plot.title = element_text(size = ifelse(nchar(j)<=25,10,ifelse(nchar(j)<=40,9,8)), colour = "black"),
              axis.title =element_text(size = 9, colour = "black"),
              axis.text = element_text(size = 8, colour = "black"))
    } # function(i, j)
    ) # map2
    n.rows <- round(x = sqrt(x = length(x = aetime)), digits = 0)
    n.cols <- ceiling(x = length(x = aetime)/n.rows)
    m1 <- gridExtra::arrangeGrob(grobs = aetime, ncol = n.cols, nrow = n.rows)
    ggsave(plot = m1, filename = "AETime.png", width = 6, height = 5*n.rows/n.cols)
  } # if

  study.col <<- RColorBrewer::brewer.pal(
    length(x = unique(x = obsdf[[demog_grp_var_label]])),
    "Dark2") # study.col
  # Compare the safety events with Exposure (original and log)
  for(k in unname(obj = sapply(X = endpName, "[[", 1))){
    # Subset exposures list by by endpoint
    exposureCov.plot <- exposureCov
    for(. in names(x = exposureCov)){
      if(k %in% lapply(full.desc.exposureCov.1, "[[", "end.p")[names(x = lapply(full.desc.exposureCov.1, "[[", "end.p")) == .][[1]]){
        exposureCov.plot[[.]] <- exposureCov.plot[[.]]
      } else{ # if
        exposureCov.plot[[.]] <- NULL
      } # else
    } # for
    p_safe_exp <- map2(.x = names(x = exposureCov.plot), .y = unname(obj = exposureCov.plot), function(i, j){
      obsdf %>%
        dplyr::filter(!is.na(dvf) & obsdf[, endpcolName] == k) %>%
        ggplot(., aes_string(x = dvf, y = i)) +
        geom_boxplot(outlier.shape = NA, lwd = 0.4) +
        geom_point(aes(color = factor(get(x = demog_grp_var_label))),
                   position = position_jitterdodge(jitter.width = 0.1), alpha = 0.4, size = 0.5) +
        ylab(parse(text = j)) +
        xlab(label = NULL) +
        labs(color = "Group") +
        scale_color_manual(values = study.col) +
        guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
        theme(axis.title = element_text(size = 12, colour = "black"))
    } # function(i, j)
    ) # map2
    p.legend <- cowplot::get_legend(plot = p_safe_exp[[1]])
    p_safe_exp <- lapply(X = p_safe_exp, FUN = function(.){
      . + theme(legend.position = "none")
    } # function(.)
    ) # lapply
    n.rows <- round(x = sqrt(x = length(x = p_safe_exp)), digits = 0)
    n.cols <- ceiling(x = length(x = p_safe_exp)/n.rows)
    m1 <- gridExtra::arrangeGrob(gridExtra::arrangeGrob(grobs = p_safe_exp, ncol = n.cols, nrow = n.rows), p.legend, ncol = 2, widths = c(6, 1))
    ggsave(plot = m1, filename = paste(gsub(pattern = " ", replacement = "-", x = names(endpName)[k == sapply(X = endpName, "[[", 1)]),
                                       "-exp.png", sep = ""), width = 9, height = 6.25) # ggsave
  } # for
  dev.off()
  message("EDA complete")

}
