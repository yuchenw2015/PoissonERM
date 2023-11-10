Introduction <- function(title = NULL){
  n.subsections <- length(endpName)
  report.endpoints <- names(lapply(endpName,names))
  text.start <- "Poisson regression models were developed for "
  for(jj in report.endpoints){
    text.start <- c(text.start,paste0(EndpointNameLatex(jj),", "))
  }
  text.start[length(text.start)] <- paste(" and",text.start[length(text.start)] )
  text.start[length(text.start)] <- gsub(", ",".",text.start[length(text.start)] )
  if(length(report.endpoints) == 0) text.start <- "Poisson regression model was not developed for any endpoint. It might be a result of low incidence rate. See Methods section."
  intro.object <- c(
    "# EXECUTIVE SUMMARY",
    "",
    paste(text.start,collapse=""),
    ""
  )
  folder.location = paste(glob.wd, "ReportComponents", sep = "/")
  intro.object <- gsub("~", " ", intro.object)

  writeLines(intro.object,con=paste(folder.location,"Introduction.txt",sep="/"))
  if(!is.null(file)) write(intro.object,file=title, append = TRUE)

  exec_sum_items <- NULL
  for(jj in report.endpoints){
    endpoint_sum_text <- InterpretModel(jj) %>% paste(collapse=" ")
    endpoint_sum_text <- paste("- ",endpoint_sum_text)
    exec_sum_items <- c(exec_sum_items, endpoint_sum_text)
  }
  exec_sum_items <- gsub("~", " ", exec_sum_items)
  write(exec_sum_items,file=paste(folder.location,"Introduction.txt",sep="/"),append=TRUE)
  if(!is.null(file)) write(exec_sum_items,file=title,append=TRUE)
  n.subsections <<- n.subsections
  report.endpoints <<- report.endpoints
}
