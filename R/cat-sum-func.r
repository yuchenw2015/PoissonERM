# Summary of categorical variables - 1 grouping variable
# ------------------------------------------------------------------------------
cat_sum.func <- function(df, groups, var, label){
  df1 <- df %>%
    mutate_at(var, fct_explicit_na, na_level = "Missing") %>%
    group_by(!!!syms(c(groups, var)))
  df_sum <- df1 %>%
    count() %>%
    group_by(!!!syms(groups)) %>%
    mutate(freq = round(n/sum(n) * 100, 1),
           stat = paste0(n," (", freq, ")"),
           groupvar = paste(!!!syms(groups), sep = "_")) %>%
    ungroup() %>%
    select_at(c("groupvar", var, "stat"))
  df_tot <- df1 %>%
    group_by(!!!syms(var)) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n/sum(n) * 100, 1),
           stat = paste0(n," (", freq, ")"),
           groupvar = "Total") %>%
    ungroup() %>%
    select_at(c("groupvar", var, "stat"))
  df_sum <- bind_rows(df_sum, df_tot) %>%
    pivot_wider(names_from = var, values_from =  stat) %>%
    t
  df_sum[is.na(df_sum)] <- "0 (0)"
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
} # cat_sum.func
