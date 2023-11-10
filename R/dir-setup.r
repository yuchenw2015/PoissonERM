# Create a parent folder for each endpoint
# ------------------------------------------------------------------------------
dir.setup.func <- function(){
  end.path <<- paste(glob.wd, gsub(pattern = " ", replacement = "-", x = names(endpName)), sep = "/")
  if(!all(dir.exists(paths = end.path))){
    message("Creating parent folder(s) for selected endpoints...")
    end.path.mod <<- end.path[!dir.exists(paths = end.path)]
    for(i in 1:length(end.path.mod)){
      dir.create(path = end.path.mod[i])
    } # for
    message("Parent folder(s) created")
  } # if

}
