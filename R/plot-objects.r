# Create plot objects for predicted probability graphs
# ------------------------------------------------------------------------------
# Gather working directory
plot.objects.func <- function(){
  glob.wd <- getwd()
  # Set global ggplot2 theme
  theme_set(theme_bw() +
              theme(axis.title =element_text(size= 12, colour = "black"),
                    axis.text = element_text(size = 10, colour = "black"),
                    legend.text = element_text(size = 10, colour = "black"),
                    legend.title = element_text(size = 10, colour = "black"),
                    plot.title = element_text(hjust = 0.5, size = 11, colour = "black"))) # theme_set

  # Max number of columns in final predicted probability grid(s)
  # Recommend no more than 6
  max.cols <<- 4
  # Units for plot dimensions
  dim.unit <<- "in"
  # Plot device
  plot.dev <<- "png"
  # Grid width
  grid.width <<- 9
  # Grid height
  grid.height <<- 6
  # Ind. plot width
  ind.p.width <<- 5
  # Ind. plot height
  ind.p.height <<- 3
  # Color for predicted line
  pred.line.color <<- "blue"
  # Z score for sim datasets and plots (CI)
  z.score <<- 1.96
  # Color for ribbon around predicted line
  rib.fill.color <<- "blue"
  # Alpha value for ribbon around predicted line
  con.rib.alpha <<- 0.3
  # Percentiles of observed data added to con cov pred plots
  obs.perc <<- c(0.05, 0.25, 0.50, 0.75, 0.95)
  # Color for lines corresponding to observed percentiles
  perc.line.color <<- "black"
  # Linetype for observed percentiles
  perc.linetype <<- "dashed"
  # Width of geom_errorbar for cat. plots 95% CI
  err.width <<- 0.4
  # Color for geom_errorbar
  err.color <<- "black"

  # Add more or less width to the grid if greater or fewer columns
  # are to be used per grid
  if(max.cols != 4){
    grid.width <<- grid.width * max.cols / 4
  } # if

  # Create plot object with layers that should remain the same between cov plots:
  p <- ggplot() + ylim(c(0, 1)) + theme(legend.position = "bottom")

  # Create scale for effects forest plot
  logscale <<- c(0, 2^(0:floor(log(500,2))))

  # Save objects
  objects <- ls(envir = .GlobalEnv)
  object.list <<- list()
  for(i in 1:length(objects)){
    object.list[[objects[i]]] <<- get(objects[i])
  } # for
  save(object.list,file = "objects.Rdata")
  message("Predicted probability plot objects created")
  glob.wd <<- glob.wd

}
