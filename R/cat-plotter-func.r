# Make categorical cov plots
# ------------------------------------------------------------------------------
cat_plotter.func <- function(covs){
  .data <- get(x = paste0("sim.df.", covs))
  y.lab <- "Predicted probability"
  # sum <- count(x = mod.df[mod.df[[dv]] == 1, ],
  # mod.df[[dv]][mod.df[[dv]] == 1],
  # mod.df[[covs]][mod.df[[dv]] == 1]) # count
  # sum.2 <- count(x = mod.df[mod.df[[dv]] == 0, ],
  # mod.df[[dv]][mod.df[[dv]] == 0],
  # mod.df[[covs]][mod.df[[dv]] == 0]) # count
  # events <- setNames(object = sum$n,
  # nm = sum$`mod.df[[covs]][mod.df[[dv]] == 1]`) # setNames
  # non.events <- setNames(object = sum.2$n,
  # nm = sum.2$`mod.df[[covs]][mod.df[[dv]] == 0]`) # setNames
  pcat <- p +
    geom_col(mapping = aes(x = .data[[covs]],
                           y = Prob,
                           fill = .data[[covs]]),
             data = .data) + # geom_col
    geom_errorbar(mapping = aes(x = .data[[covs]], y = Prob,
                                ymin = lower, ymax = upper),
                  data = .data,
                  width = err.width,
                  color = "black") + # geom_errorbar
    # geom_text(mapping = aes(x = .data[[covs]], y = 0.95, label = paste(events, "E", sep = " ")),
    # position = position_dodge(width = 0.9),
    # color = "black", hjust = 0.5, angle = 0, data = .data) +
    # geom_text(mapping = aes(x = .data[[covs]], y = 0.85, label = paste(non.events, "X", sep = " ")),
    # position = position_dodge(width = 0.9),
    # color = "black", hjust = 0.5, angle = 0, data = .data) +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0, 1)) +
    ylab(label = y.lab)
  # Save the plot to the workspace
  assign(x = paste(covs, ".sim.plot", sep = ""), value = pcat, envir = globalenv())
} # cat.plotter
