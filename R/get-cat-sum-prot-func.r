# Summarize categorical variables
# ------------------------------------------------------------------------------
get_cat_sum_prot.func <- function(df, groups){
  y.latex <- lapply(X = unname(obj = full.cat.lab)[!names(x = full.cat.lab)%in%c("PROT", demog_grp_var, demog_grp_var_label)], FUN = function(.){
    convert_express_latex.func(text_to_convert = .)
  } # function(.)
  ) # lapply
  catSum_df <- map2_dfr(.x = unname(obj = full.cat.lab)[!names(x = full.cat.lab)%in%c("PROT", demog_grp_var, demog_grp_var_label)],
                        .y = y.latex,
                        .f = function(x, y) cat_sum.func(df,
                                                         groups = groups,
                                                         var = x, label = y)) # catSum_df
  catSum_df$Statistics <- convert_express_latex.func(text_to_convert = catSum_df$Statistics)
  return(catSum_df)
} # get_cat_sum_prot.func
