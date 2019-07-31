################################################################################
# Description
################################################################################
# This script downloads the latest HMS smoke plume analysis available from 
# the archive at https://www.ospo.noaa.gov/Products/land/hms.html
# If no new analysis is available, i.e. no polygons in the file linked online,
# the latest file is not used, and instead, the older polygons are retained. 

# library(rgdal)
# library(stringr)

library(tidyverse)
library(readxl)
library(writexl)
library(httr)
library(sf)
library(rvest)
library(rgdal)

# Local development taking place. 
# home_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/"
home_path <- "R:/RSTOR-Magzamen/Research/Projects/CO_Wildfires/Subprojects/smoke_forecaster/Smoke_Predictor/"

today <- Sys.Date()
today_char <- as.character(format(today, "%Y%m%d"))

# Get HMS smoke data
# urlBase <- "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/"
urlBase <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/" #updated

#' List of HMS smoke files on the website
hms_files <- read_html(urlBase) %>% 
  html_nodes("a") %>% 
  html_text(trim = T) %>% 
  as.data.frame() %>% 
  rename("hms_files" = ".") %>% 
  mutate(hms_files = as.character(hms_files)) %>% 
  filter(str_detect(hms_files, "smoke"))

hms_latest <- filter(hms_files, str_detect(hms_files, "latest")) %>% 
  filter(str_detect(hms_files, ".zip"))

#' Download the "latest smoke" file
extensions <- c(".dbf", ".shp", ".shx")

for (ext in extensions){
  destFile <- paste0(home_path, "data/HMS/latest_smoke", ext)
  url <- paste0(urlBase, "latest_smoke", ext)
  download.file(url=url, destfile=destFile, cacheOK=F)
}

#' Also have the option to get the file from 2 days ado
#' Can decide if we want to swap this shapefile out for the "latest_smoke" one
prev <- Sys.Date() - 2
prev_char <- as.character(format(prev, "%Y%m%d"))

hms_prev <- filter(hms_files, str_detect(hms_files, prev_char)) %>% 
  filter(!str_detect(hms_files, ".zip")) %>% 
  filter(!str_detect(hms_files, ".pr"))

for (i in 1:length(hms_prev$hms_files)){
  destFile <- paste0(home_path, "data/HMS/", hms_prev$hms_files[i])
  url <- paste0(urlBase, hms_prev$hms_files[i])
  download.file(url=url, destfile=destFile, cacheOK=F)
}

hms_shp <- hms_prev[which(grepl(".shp", hms_prev$hms_files)), "hms_files"]
hms_shp <- gsub(".shp", "", hms_shp)

#' Read in the shapefile
#' If there are no features, do not update the "latest_smoke" shapefile
#' We'll need to have some sort of indicator of when the smoke data were updated
#' Maybe don't allow them to be displayed if the download date doesn't match the current date?
#' I'll add an .rda object below that just stores the update date

smoke_path <- paste0(home_path, "data/HMS")

try_error <- try(
  latest_smoke <- st_read(dsn = smoke_path, layer="latest_smoke"),
  silent=T
)

try_error2 <- try(
  yest_smoke <- st_read(dsn = smoke_path, layer= hms_shp),
  silent=T
)

#' test a known file
co_shape <- "R:/RSTOR-Magzamen/Research/Secondary_Data/Colorado_Shapefiles"
test <- st_read(dsn = co_shape, layer = "Munibounds")

#' Test the same shapefile copied to another director
smoke <- "R:/RSTOR-Magzamen/Research/Secondary_Data/HMS_Smoke_Plumes"
test2 <- st_read(dsn = smoke, layer = "hms_smoke20190729")
test3 <- st_read(dsn = smoke, layer = hms_shp)


# 
# 
# if(class(try_error) == "try-error"){
#   
#   print("No polygon features in the latest file. Not updating for now.")
#   
# } else{
#   
#   print("Features in the latest smoke file. Updating the existing.")
#   
#   startString <- as.character(latest_smoke$Start)
#   gap <- str_locate(startString, " ")
#   len <- str_length(startString)
#   YEARJDAY <- str_sub(startString, 1, gap[1])
#   HOUR <- str_sub(startString, gap[,1]+1, len)
#   DATE <- as.character(as.POSIXct(YEARJDAY, format = "%Y%d"))
#   
#   # Save the easier to read formatted date information in the dataframe
#   latest_smoke$X1 <- paste0(DATE, " ", HOUR, "Z")
#   
#   # # Format the annoying time format YYYDD HH
#   # latest_smoke$Start <- as.POSIXct(latest_smoke$Start, format = "%Y%d %H", tz="UTC")
#   # latest_smoke$End <- as.POSIXct(latest_smoke$End, format = "%Y%d %H", tz="UTC")
# 
#   # rewrite the file
#   rgdal::writeOGR(obj = latest_smoke, 
#                   dsn = paste0(home_path,"data/HMS"), 
#                   layer = "latest_smoke_display", 
#                   driver = "ESRI Shapefile", 
#                   overwrite_layer = T)
#   
# }
# 
# # TODO: Consider sharing this information with the user on the site. 
# print(paste("HMS smoke plumes updated at:", Sys.time()))
# print("Script run successfully.")