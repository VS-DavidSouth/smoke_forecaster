# ------------------------------------------------------------------------------
# Title: Creating base map with some interactive features using Leaflet package
# Author: Ryan Gan
# Date Created: 5/31/2017
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------

# load libraries
library(leaflet)
# need ncdf4 package (maybe)
library(ncdf4)
# need raster package
library(raster)

# color pallet

# add smoke raster brick 
# can read the netcdf file right in to a raster brick from netcdf
# read net cdf file of forecasts from 06/08/2017
smk_brick <- brick("./smoke_dispersion_v2.nc")

# subsetting to work with one raster
r <- smk_brick[[180]]

# define color gradient for layer
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,100), 
                    na.color = "transparent")

crs(r)

west_us <- leaflet() %>% 
  # call map layer
  addTiles() %>% 
  # set bounds of map
  fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
  # add brick
  addRasterImage(r, colors = pal, opacity = 0.5, project = F) %>%  
  addLegend(pal=pal, values=values(r), title = "Smoke ug/m^3")



west_us



