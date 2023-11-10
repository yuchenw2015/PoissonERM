# Summarize continuous variables
# ------------------------------------------------------------------------------
get_cont_sum_prot.func <- function(df, groups){
  y.latex <- lapply(X = unname(obj = orig.con), FUN = function(.){
    convert_express_latex.func(text_to_convert = .)
   } # function(.)
  ) # lapply
  contSum_df <- map2_dfr(.x = names(x = orig.con), .y = y.latex,
                         .f = function(x, y) cont_sum.func(df,
                                                                groups = groups,
                                                                var = x, label = y)) # contSum_df
  return(contSum_df)
} # get_cont_sum_prot.func
