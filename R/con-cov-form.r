con.cov.form.func <- function(){
  if(log_covs == "Yes"){
    set.seed(seed = 12345)
    # Normal quantiles to compare with quantiles of continuous cov
    q.norm <- qnorm(p = c(0.25, 0.75))
    # Calculate slopes of original scale and log scale continuous cov against
    # normal quantiles
    nobs <- obsdf %>% distinct(UID2) %>% nrow
    # shapiro test is only appropriate for n between 3 and 5000
    # see details in documentation of shapiro.test or https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
    # ad.test (Anderson-Darling Test) usually has a lower power than shapiro test when shapiro test is applicable.
    if(nobs <= 3|nobs>=5000){
      pvalues <<- map2(.x = names(x = orig.con), .y = names(x = log.con), .f = function(x, y){
        pvalue.orig <<- nortest::ad.test(x = obsdf %>% distinct(UID2, .keep_all =T) %>% select_at(x) %>% unlist)$p.value
        pvalue.log <<- nortest::ad.test(x = obsdf %>% distinct(UID2, .keep_all =T) %>% select_at(y) %>% unlist)$p.value
        pvalue.list <<- setNames(object = list(pvalue.orig, pvalue.log), nm = c(x, y))
        return(pvalue.list)
      } # function(x, y)
      ) # map2
    }else{
      pvalues <<- map2(.x = names(x = orig.con), .y = names(x = log.con), .f = function(x, y){
        pvalue.orig <<- shapiro.test(x = obsdf %>% distinct(UID2, .keep_all =T) %>% select_at(x) %>% unlist)$p.value
        pvalue.log <<- shapiro.test(x = obsdf %>% distinct(UID2, .keep_all =T) %>% select_at(y) %>% unlist)$p.value
        pvalue.list <<- setNames(object = list(pvalue.orig, pvalue.log), nm = c(x, y))
        return(pvalue.list)
      } # function(x, y)
      ) # map2
    }

    rm(nobs)
    # Keep the final list of continuous covs for modeling
    # based on the slope that is closest to 1 per pair
    full.con.t <<- sapply(X = pvalues, FUN = closest.func, sv = 1)
    dropped.con <<- names(x = full.con[!names(x = full.con) %in% full.con.t])
    full.con.t <<- full.con[names(x = full.con) %in% full.con.t]
    message(paste("each of the following:", paste(dropped.con, collapse = " "), "was dropped from the analysis in favor of its scaled counterpart:", paste(names(x = full.con.t), collapse = " "),
                  "based on the normality of its distribution", sep = " "))
  }else{
    full.con.t <<- full.con
  }

}
