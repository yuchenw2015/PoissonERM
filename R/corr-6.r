corr.6.func <- function(){
  corr.6 <<- which(x = abs(x = corr.con) >= 0.6 & abs(x = corr.con) < 1-1e-10, arr.ind = F)
  if(length(x = corr.6) > 0){
    message("Grouping highly correlated (rho >= 0.6) continuous covariates")
    corr.6 <<- which(x = abs(x = corr.con) >= 0.6 & abs(x = corr.con) < 1-1e-10, arr.ind = T)
    corr.6.pairs <<- sapply(X = seq_len(length.out = nrow(x = corr.6)), FUN = function(.){
      first <- rownames(x = corr.6)[.]
      sapply(X = first, simplify = F, USE.NAMES = T, FUN = function(k){
        other_s <<- rownames(x = corr.6)[corr.6[., 1] == corr.6[, 2]]
        return(other_s)
      } # function(k)
      ) # sapply
    } # function(.)
    ) # sapply
    names.corr.6.pairs <<- gsub(pattern = " ", replacement = "~", x = names(x = corr.6.pairs))
    names(x = corr.6.pairs) <<- names.corr.6.pairs
    names(x = corr.6.pairs) <<- sapply(X = names(x = corr.6.pairs), FUN = function(q){
      names(x = full.con.t)[unname(obj = full.con.t) %in% q]
    } # function(x)
    ) # sapply
    corr.6.pairs <<- sapply(X = corr.6.pairs, simplify = F, USE.NAMES = T, FUN = function(j){
      step1 <<- gsub(pattern = " ", replacement = "~", x = j)
      names(x = full.con.t)[unname(obj = full.con.t) %in% unname(step1)]
    } # function(j)
    ) # sapply
    corr.6.pairs <<- corr.6.pairs[!duplicated(names(x = corr.6.pairs))]
  } else{ # if
    message("No highly correlated (rho >= 0.6) continuous covariates")
  } # else
  # Checking categorical covariates
  chisq.t.6 <<- which(x = abs(x = chisq.t) >= 0.6 & abs(x = chisq.t) < 1-1e-10, arr.ind = F) # 0.6
  if(length(x = chisq.t.6) > 0){
    message("Grouping highly correlated (rho >= 0.6) categorical covariates")
    chisq.t.6 <<- which(x = abs(x = chisq.t) >= 0.6 & abs(x = chisq.t) < 1-1e-10, arr.ind = T)
    chisq.t.6.pairs <<- sapply(X = seq_len(length.out = nrow(x = chisq.t.6)), FUN = function(.){
      first <- rownames(x = chisq.t.6)[.]
      sapply(X = first, simplify = F, USE.NAMES = T, FUN = function(k){
        other_s <<- rownames(x = chisq.t.6)[chisq.t.6[., 1] == chisq.t.6[, 2]]
        return(other_s)
      } # function(k)
      ) # sapply
    } # function(.)
    ) # sapply
    chisq.t.6.pairs <<- chisq.t.6.pairs[!duplicated(names(x = chisq.t.6.pairs))]
  } else{ # if
    message("No highly correlated (rho >= 0.6) categorical covariates")
  } # else


}
