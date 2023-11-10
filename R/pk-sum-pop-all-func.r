# Combined summary table of population PK parameters/exposures
# ------------------------------------------------------------------------------
pk_sum_pop_all.func <- function(df, groups, parameters, parameters_label){
  # Generate the summary table
  parameters_label.latex <- lapply(X = parameters_label, FUN = function(.){
    convert_express_latex.func(text_to_convert = .)
   } # function(.)
  ) # lapply
  est <- map2_dfr(.x = parameters, .y = parameters_label.latex,
                  .f = function(x, y) pk_sum_pop.func(df = df, groups = groups, var = x, label = y))
  # Format headers
  header <- as.data.frame(strsplit(colnames(est), split = "_")) %>%
    mutate_all(as.character)
  colnames(header) <- colnames(est)
  return(est)
} # pk_sum_pop_all.func
