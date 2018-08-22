#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  machine_name <- args[1]
}else{
  machine_name <- "local"
}

library(rgdal)

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

# Read in for formating 
latest_smoke <- rgdal::readOGR(dsn=paste0(home_path, "data/HMS"), 
                               layer="latest_smoke")

# Format the annoying time format YYYDD HH
latest_smoke$Start <- as.POSIXct(latest_smoke$Start, format = "%Y%d %H", tz="UTC")
latest_smoke$End <- as.POSIXct(latest_smoke$End, format = "%Y%d %H", tz="UTC")

# rewrite the file
rgdal::writeOGR(obj = latest_smoke, 
                dsn = paste0(home_path,"data/HMS"), 
                layer = "latest_smoke", 
                driver = "ESRI Shapefile", 
                overwrite_layer = T)

# TODO: Consider sharing this information with the user on the site. 
print(paste("HMS smoke plumes updated at:", Sys.time()))