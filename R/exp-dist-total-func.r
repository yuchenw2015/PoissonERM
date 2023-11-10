# Distribution of Exposure by Protocol
# ------------------------------------------------------------------------------
exp_dist_total.func <- function(df, groups){
  for(i in unique(df[, groups])){
    df_sub <- df[df[, groups] == i, ]
    if(i != "Placebo"){
      distByStudy = get_exp_dist.func(df = df_sub)
      ggsave(distByStudy[[1]], filename = paste0("exposuredist-", i,".png"),  width = 9, height = 6.25*distByStudy[[2]])
    } # if
  } # for
} # exp_dist_total.func
