# Drop endpoint(s) from analysis if one category is < 10%
# ------------------------------------------------------------------------------
final.endpoints <- function(){
  for(u in unique(x = obsdf$Endpoint)){
    obsdf.2 <<- df.sum.total2[(df.sum.total2[endpcolName] == endpName[[u]][1]), ]
    p.yes <<- ceiling(x = obsdf.2$Events_per)
    if(p.yes > p.yes.up | p.yes < p.yes.low){
      #endpoints <<- endpoints[endpoints != unname(obj = sapply(X = endpName, "[[", 1))[
      #  names(x = sapply(X = endpName, "[[", 2)) == grep(pattern = u, x = names(x = sapply(X = endpName, "[[", 2)),
      #                                                   ignore.case = T,value = T)]] # endpoints
      endpName[[u]] <<- NULL #remove u from the endpName list
      endpoints <<- unname(sapply(X = endpName, "[[", 1)) # endpoints
      message(paste(u, " was dropped from the analysis due to less than ", p.yes.low, "% occurrence of a yes/no category", sep = ""))
    } # if
  } # for
  if(length(x = endpName) > 0){
    message(paste("All remaining endpoints have greater than ", round(x = p.yes.low, digits = 1), "% occurrence of a yes/no category", sep = ""))
  } # if
}
