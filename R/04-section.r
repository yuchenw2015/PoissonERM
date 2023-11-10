####################################
# create the results section of the report.
Result <- function(title = NULL){
  write(c("# RESULTS ", " "),file=title,append=TRUE)
  folder.location = paste(glob.wd, "ReportComponents", sep = "/")
  for(jj in report.endpoints){
    result <- build.endpoint.results.body(jj)
    result <- gsub("~", " ", result)
    writeLines(result, con=paste(folder.location,paste(gsub(" ","-",jj),"body-results.txt",sep="-"),sep="/"))
    if(!is.null(file)) write(result,file=title,append=TRUE)
  }
}
