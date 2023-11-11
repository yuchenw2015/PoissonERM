####################################
# create the results section of the report.
Conclusion <- function(title = NULL){
  folder.location = paste(glob.wd, "ReportComponents", sep = "/")
  conclusion_text <- c("# CONCLUSION ", " ")
  writeLines(conclusion_text,con=paste(folder.location,"Conclusions.txt",sep="/"))
  write(conclusion_text,file=title,append=TRUE)

  for(jj in report.endpoints){
    endpoint_conclusions_formatted <- paste(c("- ","A Poisson regression model was developed for ",EndpointNameLatex(jj),"."),collapse="")
    endpoint_conclusions_formatted <- c(endpoint_conclusions_formatted, Conclusions_endpoint(jj) )
    write(endpoint_conclusions_formatted,paste(glob.wd, "ReportComponents","Conclusions.txt", sep = "/"),append=T)
    if(!is.null(file)) write(endpoint_conclusions_formatted,file=title,append=TRUE)
  }
}
