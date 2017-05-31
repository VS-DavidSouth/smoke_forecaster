# ------------------------------------------------------------------------------
# Title: Creating base map with some interactive features using Leaflet package
# Author: Ryan Gan
# Date Created: 5/31/2017
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------

# load libraries
library(leaflet)
# need maptools to open bluesky kmz files
library(maptools)
library(rgdal)


# read in test kmz file of US smoke forecasts (downloaded on 5/31/2017) --------
smoke <- readOGR("./smoke_dispersion/doc.kml")
plot(smoke)
str(smoke)

?getKMLcoordinates()
# create base map of western US

west_us <- leaflet() %>% 
  # call map layer
  addTiles() %>% 
  # set bounds of map
  fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25)

west_us


