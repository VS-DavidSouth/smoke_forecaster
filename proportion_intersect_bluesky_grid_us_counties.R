# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US countys
# Purpose: To create a matrix of proportions to use in population-weighting
# Author: Ryan Gan
# Date Created: 6/28/17
# R Version: 3.3.3
# ------------------------------------------------------------------------------

# load libraries ----
library(raster)
library(sf)
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)

# load raster brick ----
nc_path <- "smk_stack_raster.nc"

# using single raster layer of next day average
smk_forecast <- brick(nc_path)

# subset to one layer and plot grid
bluesky_grid <- rasterToPolygons(smk_forecast[[1]])
plot(bluesky_grid)
bluesky_grid

# get a bounding box of the bluesky grid
bbox <- extent(bluesky_grid)
# get projection
wgs84 <- proj4string(bluesky_grid)

# get ids of cells
test <- sapply(slot(bluesky_grid, "polygons"), function(x) slot(x, "ID"))
head(test)


# load county shapefile
us_county <- readOGR(dsn = "./tl_2016_us_county/tl_2016_us_county.shp",
                     layer = "tl_2016_us_county")

# set projection of US shape to same as bluesky
us_county <- spTransform(us_county, CRS(wgs84))

proj4string(us_county)
proj4string(bluesky_grid)
# subset counties to only those in bluesky extent
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), 
                                         "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = TRUE)
}

# warning produced by gclip function, but seems okay
us_county <- gClip(us_county, bbox)
plot(us_county)
