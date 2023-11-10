# Summary of continuous variables - 1 grouping variable (study)
# ------------------------------------------------------------------------------
cont_sum.func <- function(df, groups, var, label){
  df1 <- df %>%
    mutate_at(var, as.numeric) %>%
    group_by(!!!syms(groups))
  df_sum <- df1 %>%
    summarise_at(var,
                 list("Median" = ~ median_pop.func(.),
                      "Range (Min, Max)" = ~ range_pop.func(.),
                      "Mean (SD)" = ~ meansd_pop.func(.),
                      "Missing (\\%)" = ~ missing_pop.func(.)
                 )) %>%
    mutate(groupvar = paste(!!!syms(groups), sep ="_")) %>%
    ungroup() %>%
    add_row(groupvar = "Total", Median = median_pop.func(df1[[var]]), "Range (Min, Max)" = range_pop.func(df1[[var]]),
            "Mean (SD)" = meansd_pop.func(df1[[var]]), "Missing (\\%)" = missing_pop.func(df1[[var]])) %>%
    select_at(c("groupvar", "Median", "Range (Min, Max)", "Mean (SD)", "Missing (\\%)")) %>%
    t
  colnames(df_sum) <- df_sum[1, ]
  rownames(df_sum) <- paste0("  ", rownames(df_sum))
  varcol <- ""
  df_sum <- rbind(varcol, df_sum)
  rownames(df_sum)[1] <- label
  df_sum <- df_sum %>%
    as.data.frame %>%
    rownames_to_column(., var = "Statistics") %>%
    slice(-2) %>%
    mutate_all(as.character)
  return(df_sum)
} # cont_sum.func
