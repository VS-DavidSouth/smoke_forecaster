#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  machine_name <- args[1]
}else{
  machine_name <- "local"
}

################################################################################
# Description
################################################################################
# This script downloads the latest HMS smoke plume analysis available from 
# the archive at https://www.ospo.noaa.gov/Products/land/hms.html
# If no new analysis is available, i.e. no polygons in the file linked online,
# the latest file is not used, and instead, the older polygons are retained. 

library(rgdal)
library(stringr)

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
  download.file(url=url, destfile=destFile, cacheOK=F)
}

# Read in for formatting
# NOTE: Sometimes this file contains no features. When that is the case, do not
# NOTE: write a new display file, as it would be blank and throw an error. 
try_error <- try(
  latest_smoke <- rgdal::readOGR(dsn=paste0(home_path, "data/HMS"), 
                                 layer="latest_smoke"),
  silent=T
)


if(class(try_error) == "try-error"){
  
  print("No polygon features in the latest file. Not updating for now.")
  
} else{
  
  print("Features in the latest smoke file. Updating the existing.")
  
  startString <- as.character(latest_smoke$Start)
  gap <- str_locate(startString, " ")
  len <- str_length(startString)
  YEARJDAY <- str_sub(startString, 1, gap[1])
  HOUR <- str_sub(startString, gap[,1]+1, len)
  DATE <- as.character(as.POSIXct(YEARJDAY, format = "%Y%d"))
  
  # Save the easier to read formatted date information in the dataframe
  latest_smoke$X1 <- paste0(DATE, " ", HOUR, "Z")
  
  # # Format the annoying time format YYYDD HH
  # latest_smoke$Start <- as.POSIXct(latest_smoke$Start, format = "%Y%d %H", tz="UTC")
  # latest_smoke$End <- as.POSIXct(latest_smoke$End, format = "%Y%d %H", tz="UTC")

  # rewrite the file
  rgdal::writeOGR(obj = latest_smoke, 
                  dsn = paste0(home_path,"data/HMS"), 
                  layer = "latest_smoke_display", 
                  driver = "ESRI Shapefile", 
                  overwrite_layer = T)
  
}

# TODO: Consider sharing this information with the user on the site. 
print(paste("HMS smoke plumes updated at:", Sys.time()))
print("Script run successfully.")