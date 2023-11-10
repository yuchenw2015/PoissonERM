# Make continuous cov plots
# ------------------------------------------------------------------------------
con_plotter.func <- function(covs){
  .data <- get(x = paste("sim.df.", covs, sep = ""))
  x.lab <- as.character(x = con.covlabdf$Title[con.covlabdf$Covariate == covs])
  y.lab <- "Predicted probability"
  if("TRTG" %in% colnames(x = obsdf)){
    cov.percs <- obsdf[!duplicated(x = obsdf$UID2) & obsdf$TRTG != "Placebo", covs]
  } else{ # if
    cov.percs <- obsdf[!duplicated(x = obsdf$UID2), covs]
  } # else
  pcon <- p +
    geom_vline(mapping = aes(xintercept = quantile(cov.percs, obs.perc, na.rm = T)),
               colour = perc.line.color, linetype = perc.linetype) + # geom_vline
    geom_line(mapping = aes(x = .data[[covs]], y = Prob),
              data = .data, color = pred.line.color) + # geom_line
    geom_ribbon(mapping = aes(x = .data[[covs]], y = Prob,
                              ymin = lower, ymax = upper),
                data = .data, alpha = con.rib.alpha,
                fill = rib.fill.color) + # geom_ribbon
    geom_rug(mapping = aes(x = mod.df[mod.df[, dv] == 1, covs],
                           y = mod.df[mod.df[, dv] == 1, dv]),
             data = mod.df[mod.df[, dv] == 1, ], sides = "t") + # geom_rug
    geom_rug(mapping = aes(x = mod.df[mod.df[, dv] == 0, covs],
                           y = mod.df[mod.df[, dv] == 0, dv]),
             data = mod.df[mod.df[, dv] == 0, ], sides = "b") + # geom_rug
    xlab(label = parse(text = x.lab)) + ylab(label = y.lab)
  .from <- floor(x = eval(expr = parse(text = paste0("con.ref.df$", covs, "$min"))))
  .to <- ceiling(x = eval(expr = parse(text = paste0("con.ref.df$", covs, "$max"))) * 0.99)
  if((.to - .from) < 4){
    .by <- round(x = (.to - .from) / 4, digits = 1)
  } else{ # if
    .by <- floor(x = (.to - .from) / 4)
  } # else
  pcon <- pcon + scale_x_continuous(breaks = seq(from = .from, to = .to, by = .by),
                                    limits = range(.data[[covs]])) # scale_x_continuous
  pcon <- pcon + scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0, 1))
  # Save the plot to the workspace
  assign(x = paste(covs, ".sim.plot", sep = ""), value = pcon, envir = globalenv())
} # con.plotter
