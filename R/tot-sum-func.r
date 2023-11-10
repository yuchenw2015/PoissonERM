# Summary of total number of subjects
# ------------------------------------------------------------------------------
tot_sum.func <- function(df, groups){
  #print(n)
  df1 <- df %>%
    group_by(!!!syms(c(groups)))
  df_sum <- df1 %>%
    count() %>%
    ungroup() %>%
    mutate(perc = round(n/sum(n)*100, 1),
           nperc = paste0(n, " (", perc, ")"),
           groupvar = paste(!!!syms(groups), sep = "_")) %>%
    add_row(groupvar = "Total", nperc = paste0(nrow(df1), " (100)")) %>%
    select_at(c("groupvar", "nperc")) %>%
    t
  colnames(df_sum) <- df_sum[1, ]
  df_sum <- df_sum %>%
    as.data.frame %>%
    rownames_to_column(., var = "Statistics") %>%
    slice(-1) %>%
    mutate_all(as.character)
  df_sum[1, 1] <- "N (\\%)"
  return(df_sum)
} # tot_sum.func
