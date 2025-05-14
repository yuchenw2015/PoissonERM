# Effects forest plot (Odds Ratio)
# ------------------------------------------------------------------------------
oddsplot.func <- function(.mod){
  oddsdf <- tidy(.mod, exponentiate = T, conf.int = T) %>% slice(-1)
  oddsdf <- oddsdf %>% dplyr::filter(!term %in% exp.met)
  if(analyze_covs == "Yes"&nrow(oddsdf)>0){
    con.covs <- names(x = attr(x = .mod$terms, which = "dataClasses"))[
      var_name_tidy.func(names(x = attr(x = .mod$terms, which = "dataClasses")), tex =T) %in% names(x = full.con.t.es)] # con.covs
    cat.covs <- names(x = attr(x = .mod$terms, which = "dataClasses"))[
      var_name_tidy.func(names(x = attr(x = .mod$terms, which = "dataClasses")), tex =T) %in% unname(obj = full.cat.lab.es)] # cat.covs

    con.plot.labs <- unlist(unname(obj = full.con.t.es)[names(x = full.con.t.es) %in% con.covs])
    cat.plot.labs <- sapply(X = cat.covs, simplify = F, USE.NAMES = F, FUN = function(.){
      paste(., levels(x = mod.df[, .])[2:length(x = levels(x = mod.df[, .]))], sep = " ")

    } # function(.)

    ) # sapply
    cat.tab.labs <- sapply(X = cat.covs, simplify = F, USE.NAMES = F, FUN = function(.){
      paste(., levels(x = mod.df[, .])[2:length(x = levels(x = mod.df[, .]))], "ref. to", levels(x = mod.df[, .])[1],sep = " ")
    } # function(.)
    )

    cat.plot.labs <- var_name_tidy.func(unlist(cat.plot.labs), tex = T)
    cat.plot.labs <- gsub(pattern = " ", replacement = "~", x = cat.plot.labs)

    cat.tab.labs <- var_name_tidy.func(unlist(cat.tab.labs), tex = T)
    cat.tab.labs <- gsub(pattern = " ", replacement = "~", x = cat.tab.labs)

    if(OR_con == "Yes"){
      plot.labs <- c(con.plot.labs, cat.plot.labs)
      tab.labs <- c(con.plot.labs, cat.tab.labs)
    }else{
      plot.labs <- c(cat.plot.labs)
      tab.labs <- c(cat.tab.labs)
      oddsdf <- oddsdf[!oddsdf$term %in% con.covs,]
    }

    oddsdf$label <- plot.labs
    oddsdf$tab.label <- tab.labs

    if(length(con.plot.labs)>0&OR_con == "Yes"){
      for(i in 1:length(con.plot.labs)){
        con_cov_diff <- mod.df[,con.covs[i]]
        if(con.model.ref == "Yes"){
          con_cov_diff <-  con_cov_diff + unlist(con.ref$ref[con.covs[i]])
        }
        values <- unname(round(quantile(con_cov_diff, OR_con_perc),2))
        tmp.df <- oddsdf[oddsdf$tab.label == con.plot.labs[i],]
        tmp.df <- tmp.df[rep(1, length(OR_con_perc)),]
        row.n <- which(oddsdf$tab.label == con.plot.labs[i])
        tmp.df$label <- paste0(oddsdf$label[oddsdf$label == con.plot.labs[i]],
                               " (", round(quantile(con_cov_diff, OR_con_perc),2),"~ref.~to~",
                               unlist(ifelse(exists("con.ref"), ifelse(con.covs[i]%in%names(con.ref$ref), con.ref$ref[names(con.ref$ref) == con.covs[i]], round(unlist(con.ref.df["med", con.covs[i]]),2)), round(unlist(con.ref.df["med", con.covs[i]]),2))),")")
        tmp.df$tab.label <- paste0(oddsdf$label[oddsdf$label == con.plot.labs[i]],
                                   " (", round(quantile(con_cov_diff, OR_con_perc),2),", ref. to ",
                                   unlist(ifelse(exists("con.ref"), ifelse(con.covs[i]%in%names(con.ref$ref), con.ref$ref[names(con.ref$ref) == con.covs[i]], round(unlist(con.ref.df["med", con.covs[i]]),2)), round(unlist(con.ref.df["med", con.covs[i]]),2))),")")
        con_cov_diff <- unname(quantile(con_cov_diff, OR_con_perc) - unlist(ifelse(exists("con.ref"), ifelse(con.covs[i]%in%names(con.ref$ref), con.ref$ref[names(con.ref$ref) == con.covs[i]], round(unlist(con.ref.df["med", con.covs[i]]),2)), round(unlist(con.ref.df["med", con.covs[i]]),2))))
        tmp.df[, c("estimate","conf.low", "conf.high")] <- exp(con_cov_diff*log(tmp.df[, c("estimate","conf.low", "conf.high")]))
        oddsdf <- rbind(oddsdf[0:(row.n-1),], tmp.df, oddsdf[-c(0:(row.n)),])
      }
    }else{
      oddsdf <- oddsdf
    }

    if(nrow(oddsdf)> 0){
      oddsdf <- oddsdf %>% mutate(low = conf.low, high = conf.high) %>%
        mutate(conf.low = ifelse(low < high, low, high),
               conf.high = ifelse(low < high, high, low)) %>%
        dplyr::select(-c(low, high))

      odds.tab <- oddsdf %>% dplyr::select(tab.label, estimate,conf.low, conf.high) %>%
        mutate(CI = paste0("(", signif(conf.low, 3), " ", signif(conf.high, 3),")"),
               estimate = as.character(signif(estimate, 3))) %>%
        dplyr::select(tab.label, estimate, CI)

      colnames(odds.tab) <- c("Variable", "Risk Ratio", "95\\% Confidence Interval")
      odds.tab$Variable <- convert_express_latex.func(text_to_convert=odds.tab$Variable)

      if(nrow(odds.tab > 0)){
        if(LaTex.table){
          write.table(x = odds.tab, file = "OR_table.tex", sep = " & ",
                      eol = "\\\\\n", quote = F, row.names = F)
        }else{
          write.table(x = odds.tab, file = "OR_table.tsv", sep = "\t",
                      quote = F, row.names = F)
        }

      }


      print(logscale[which.min(abs(logscale-floor(min(oddsdf$conf.low))))])
      print(logscale[which.max(logscale > max(oddsdf$conf.high))])
      scale <- logscale[logscale >= logscale[which.min(abs(logscale-floor(min(oddsdf$conf.low))))] &
                          logscale <= logscale[which.max(logscale > ceiling(max(oddsdf$conf.high)))]] # scale
      print(scale)
      oddsdf$ci <- paste0("(", round(oddsdf$conf.low,2), ", ", round(oddsdf$conf.high,2), ")")
      podds <- ggplot(data = oddsdf, mapping = aes(x = estimate, y = factor(x = label, levels = label, ordered = T))) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
        geom_text(aes(x = estimate, y = label, label = ci), vjust = -2) +
        scale_y_discrete(labels = parse(text = oddsdf$label)) +
        scale_x_log10() +
        # scale_x_log10(breaks = scale,
        #               labels = scale,
        #               limits = c(0, logscale[which.max(logscale > max(oddsdf$conf.high))])
        #               ) +
        xlab("Risk Ratio (log scale)") +
        ylab(label = "Effects")
      filename <- paste(gsub(pattern = " ", replacement = "-", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "odds.png", sep = "-")
      if(nrow(oddsdf) >= 5){
        height = 6
      } else if(nrow(oddsdf) <= 2){ # if
        height = 2
      } else{ # if
        height = 3
      } # else
      if(any(nchar(oddsdf$label) > 30)){
        podds <- podds + theme(axis.text.y = element_text(size = 8))
        width <- 7
      }else{
        width <- 6
      }
      ggsave(filename = filename, podds, width = 6, height = height)
      save(podds, oddsdf, file = paste0(gsub(pattern = " ", replacement = "_", x = names(endpName)[z == sapply(X = endpName, "[[", 1)]), "_OR_forest_plot_results.RData"))
    }

  }
} # oddsplot.func
