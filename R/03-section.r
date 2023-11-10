ObservedData <- function(title = NULL){
  dem_table_sums <- paste(paste0("Demog-Sum"),list.files(paste0(glob.wd,"/Demog-Sum")),sep="/")
  # remove Rplots.png
  #try(file.remove(paste0(glob.wd,"/Cov-EDA/Rplots.png")))
  cov_sums_all <- paste(paste0("Cov-EDA"),list.files(paste0(glob.wd,"/Cov-EDA")),sep="/")

  # all tables should be .tex files. All figures should be .png.
  # figure out which ones are .tex. It is likely that there will only be a .tex
  # file where there are highly correlated covariates and the table comparing
  # them is created  (ie from corr_6.R).
  # grep gives the location in the vector of these elements with .tex
  if(LaTex.table){
    ind.eda <- grep(".tex",cov_sums_all)
  }else{
    ind.eda <- grep(".tsv",cov_sums_all)
  }

  # if there are .tex files then figure out which are tables and which are figures
  if(length(ind.eda)>0){
    # since grep gives the location, we will need to use setdiff to figure out which arent tables
    ind.table <- setdiff(1:length(cov_sums_all),ind.eda)
    cov_fig_sums <- cov_sums_all[ind.table]
    cov_table_sums <- cov_sums_all[ind.eda]
  }else{
    # if there are now tables then ind.eda will have length zero.
    cov_table_sums <- NULL
    cov_fig_sums <- cov_sums_all # all files in the folder are figures
  }

  # demographic tables
  body_sum_tables <- dem_table_sums[grep("Demographics",dem_table_sums)]
  body_sum_tables <- c(body_sum_tables, dem_table_sums[grep("EventsByProtocol\\.",dem_table_sums)])
  # all of the other tables will be in the appendix
  app_sum_tables <- setdiff(dem_table_sums,body_sum_tables)

  # if cov_table_sums is null, union() wont break
  if(is.null(cov_table_sums)) app_sum_tables <- union(app_sum_tables,cov_table_sums)

  # exposure summaries should be in the body of the report.
  body_cov_fig_sums <- cov_fig_sums[which(grepl("-exp",cov_fig_sums))]
  # all others will go into the appendix
  app_cov_fig_sums <- setdiff(cov_fig_sums,body_cov_fig_sums)

  # making the latex output for the figures for the body of the report.
  out_cov_figs <- NULL
  for(i in body_cov_fig_sums){
    figure_title <- smart_title_detect(i)
    figure_caption <- "This figure was made from an automated script."
    out_cov_figs <- c(out_cov_figs,
                      make_pmx_figure_chunk(figure_path=i,
                                            figure_title=figure_title,
                                            figure_caption=figure_caption) )
  }

  # making the latex output for the figures for the body of the report.
  out_dem_tables <- NULL
  for(i in body_sum_tables){
    table_title <-smart_title_detect(i)
    table_caption <- "This table was made from an automated script."
    out_dem_tables <- c(out_dem_tables,
                        make_pmx_table_chunk(table_path=i,
                                             table_title=table_title,
                                             table_caption=table_caption,
                                             LaTex = LaTex.table))
  }
  #writeLines(out_dem_tables)


  out_results_components <- c("","# Observed Data \\label{sec:observed}","",out_dem_tables,out_cov_figs)
  out_results_components <- gsub("~", " ", out_results_components)
  folder.location = paste(glob.wd, "ReportComponents", sep = "/")
  writeLines(out_results_components,con=paste(folder.location,"ObservedData.txt",sep="/"))
  if(!is.null(file)) write(out_results_components,file=title,append=TRUE)

}
