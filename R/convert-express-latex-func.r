# Convert R expression syntax to LaTeX
# ------------------------------------------------------------------------------
# E.g., converts "C[avg~sd]~(ng/mL)" to "C\textsubscript{avg,sd}(ng/mL)"
convert_express_latex.func <- function(text_to_convert=NULL){
  # replacing square brackets with subscript syntax
  x <- text_to_convert
  x <- gsub("~"," ",x)
  x <- gsub("\\]\\,\\(", "\\]\\(", x)
  #x <- gsub("\\[","\\\\textsubscript{",x)
  x <- gsub("\\[","(",x)
  #x <- gsub("\\]","}",x)
  x <- gsub("\\]",")",x)
  x <- gsub("}\\(","} \\(",x)
  return(x)
} # convert_express_latex.func
