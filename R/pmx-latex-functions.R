define_uID <- function(path_name,object_type="tab"){
  uniqueID = paste0(object_type,":",path_name)
  uniqueID=gsub("\\_","",gsub("/","",uniqueID))
  uniqueID=gsub(" ","",gsub("/","",uniqueID))
  uniqueID=gsub("-","",gsub("/","",uniqueID))
  if(object_type=="tab"){
    uniqueID=gsub(".tex","",uniqueID)
    uniqueID=gsub(".csv","",uniqueID)
    uniqueID=gsub(".txt","",uniqueID)
  }
  if(object_type == "fig"){
    uniqueID=gsub(".pdf","",uniqueID)
    uniqueID=gsub(".png","",uniqueID)
    uniqueID=gsub(".tiff","",uniqueID)
  }
  return(uniqueID)
}

make_pmx_table_chunk <- function(table_path,table_title=NULL,table_caption=NULL, LaTex = F){
  table_title <- gsub("\\\\%", "%", gsub("\\Delta", "\\\\Delta",table_title))
  table_caption <- gsub("\\\\%", "%", gsub("\\Delta", "\\\\Delta",table_caption))
  if(substr(table_caption, nchar(table_caption), nchar(table_caption))!=".") table_caption <- paste0(table_caption,".")
  if(LaTex){
    table_template <- c(
    "```{r echo=FALSE, message=F, warning=F, paged.print=FALSE}",
    "L <- readLines(\"*{filepath}*\")",
    "L2 <- gsub(\"\\\\hline\", \"\", L)",
    "L3 <- unname(sapply(L2, function(x) substr(x, 1, nchar(x)-2)))",
    "L3 <- L3[L3!=\"\"]",
    "n_col <- max(str_count(L3, \"\\\\&\"))+1",
    "boldn <- NULL",
    "boldn <- (1:length(L3))[grepl(\"AIC\",L3)]",
    # take care of multicolumn
    "multi.index <- L3[grepl(\"\\\\multicolumn\", L3)]",
    "multi.index.new <- gsub(\"\\\\}\",\"\",strsplit(multi.index, split = \"\\\\}\\\\{\") %>% sapply(function(x) x[3]) %>% unlist())",
    "L4 <- L3",
    "L4[grepl(\"\\\\multicolumn\", L3)] <- paste(multi.index.new, paste(rep(\"&\", n_col-1), collapse = \" \"))",
    "DF <- read.table(text = L4, sep = \"&\", header = TRUE, strip.white = TRUE, check.names = FALSE)",
    "kbl(DF, caption = \"<span style=\\\"color: purple;\\\">*{title}*</span>\") %>%",
    "  footnote(general = c(\"*{caption}*\"),",
    "           general_title = \"Note.\", ",
    "           footnote_as_chunk = TRUE) %>%",
    "  kable_styling(full_width = T) %>%",
    "  row_spec(c(boldn,(1:length(L3))[grepl(\"\\\\multicolumn\", L3)])-1,bold=T,hline_after = T)",
    "```",
    "  ",
    "  "
    )
  }else{
    table_template <- c(
      "```{r echo=FALSE, message=F, warning=F, paged.print=FALSE}",
      "L <- readLines(\"*{filepath}*\")",
      "boldn <- (1:length(L))[grepl(\"AIC\",L)]",
      "DF <- read.table(text = L, sep = \"\\t\", header = TRUE, strip.white = TRUE, check.names = FALSE)",
      "kbl(DF, caption = \"<span style=\\\"color: purple;\\\">*{title}*</span>\") %>%",
      "  footnote(general = c(\"*{caption}*\"),",
      "           general_title = \"Note.\", ",
      "           footnote_as_chunk = TRUE) %>%",
      "  kable_styling(full_width = T) %>%",
      "  row_spec(c(boldn,(1:length(L))[grepl(\"\\t\\t\\t\\t\", L)])-1,bold=T,hline_after = T)",
      "```",
      "  ",
      "  "
      )
  }
  out.tab <- glue(paste0(table_template, collapse = "\r\n"),
                  .open = "*{",
                  .close = "}*",
                  filepath = table_path,
                  title = table_title,
                  caption = paste0(table_caption, " Source:",table_path))
  strsplit(out.tab, split = "\r\n") %>% unlist
}

#########################################
# function to make the pmxfigure chunk for figures

make_pmx_figure_chunk <- function(figure_path,figure_title=NULL,figure_caption=NULL){
  figure_title <- gsub("\\%", "%", gsub("\\Delta", "\\\\Delta",figure_title))
  if(substr(figure_caption, nchar(figure_caption), nchar(figure_caption))!=".") figure_caption <- paste0(figure_caption,".")
  figure_template <- c(
    "```{r, echo=FALSE, out.width=\"100%\", fig.cap=\"\\\\label{*{label}*} <span style=\\\"color: purple;\\\">*{title}*</span>.\" ,fig.topcaption=TRUE, out.width = '85%'}",
    "knitr::include_graphics(\"*{filepath}*\")",
    "```",
    "<font size=3> *Note. * *{caption}* </font>",
    "  ",
    "  "
  )

  out.fig <- glue(paste0(figure_template, collapse = "\r\n"),
                  .open = "*{",
                  .close = "}*",
                  label = define_uID(path_name=figure_path,object_type="fig"),
                  filepath = figure_path,
                  title = figure_title,
                  caption = paste0(figure_caption, " Source:",figure_path))
  strsplit(out.fig, split = "\r\n") %>% unlist
}


comma_and_syntax <- function(any_vector){
  # for english syntax need to know how many levels in case need to be separated by "and" or
  # "commas" with "and" for last level
  sentence_sequence <- NULL
  if(length(any_vector)==1){
    sentence_sequence<- any_vector
  }
  if(length(any_vector)==2){
    sentence_sequence<- paste(any_vector,collapse=" and ")
  }
  if(length(any_vector)>2){
    sentence_sequence<- paste(paste(any_vector[-length(any_vector)],collapse=", "),any_vector[length(any_vector)],sep=" and ")
  }
  return(sentence_sequence)
}
