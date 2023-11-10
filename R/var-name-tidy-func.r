var_name_tidy.func <- function(text_to_convert, tex = F){
  if(!tex){
    converted <- gsub(" ", "._.", text_to_convert)
    converted <- gsub("~", "_._", converted)
    converted <- gsub("\\(", ".left_p.", converted)
    converted <- gsub("\\)", ".right_p.", converted)
  }else{
    converted <- gsub(".right_p.", "\\)", text_to_convert)
    converted <- gsub(".left_p.", "\\(", converted)
    converted <- gsub("_._", "~", converted)
    converted <- gsub("._.", " ", converted)
  }
  converted
}
