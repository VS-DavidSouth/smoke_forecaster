#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  machine_name <- args[1]
}else{
  machine_name <- "local"
}

if(machine_name == "salix"){
  
  setwd("/srv/www/rgan/smoke_forecaster/")
  # define path to repository for the server for writing files
  home_path <- paste0("/srv/www/rgan/smoke_forecaster/")
  
}else{
  # Local development taking place. 
  home_path <- paste0(getwd(), "/")
}

# Get HMS smoke data
urlBase <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/"

extensions <- c(".dbf", ".shp", ".shx")

for (ext in extensions){
  destFile <- paste0("./data/HMS/latest_smoke", ext)
  url <- paste0(urlBase, "latest_smoke", ext)
  download.file(url=url, destfile=destFile)
}
# TODO: Consider sharing this information with the user on the site. 
print(paste("HMS smoke plumes updated at:", Sys.time()))