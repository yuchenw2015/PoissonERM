# Convert data frame with grouping variable in the row to latex format
# ------------------------------------------------------------------------------
df_to_latex.func <- function(df, grouping_rows=NULL, file, LaTex = FALSE){
  if(LaTex&(!is.null(grouping_rows))){
    df_latex <- apply(df, 1, paste, collapse = " & ")
    df_latex <- paste(df_latex,"\\\\")
    ncols <- ncol(df)
    for(i in grouping_rows){
      df_latex[i] <- paste0("\\multicolumn{", ncols, "}{l}{", df[i,1],"} \\\\")
    } # for
    # Add hline between groups
    grps <- cumsum(seq_len(length(df_latex)) %in% grouping_rows)
    df_latex.list <- split(df_latex, grps)
    df_latex.list <- lapply(df_latex.list, function(i) c("\\hline", i))
    df_latex <- unname(unlist(df_latex.list))
    header <- paste(colnames(df), collapse = " & ")
    header <- paste(header, "\\\\")
    df_latex <- c(header, df_latex)
    writeLines(df_latex, gsub(".tsv",".tex",file))
  }else{
    write.table(df,  gsub(".tex",".tsv",file), row.names=FALSE, sep="\t", quote = FALSE)
  }

} # df_to_latex.func
