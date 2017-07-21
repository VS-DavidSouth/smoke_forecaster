# ------------------------------------------------------------------------------
# Title: Subset US county shapefile to continental US
# Purpose: Subset counties to fall within the extent of the Bluesky grid for use
#          in other calculations and the app. Final product is a saved shapefile.
# Author: Ryan Gan
# Date Created: 7/21/17
# R Version: 3.3.3
# ------------------------------------------------------------------------------

# load libraries ----
library(rgdal)
library(rgeos)

# load wrfgrid polygon ----
# define relative path to polygon file
poly_path <- "./data/bluesky_grid"
poly_layer <- "bluesky_grid"

# read grid polygon
bluesky_grid <- readOGR(dsn = poly_path, layer = poly_layer)
bluesky_grid

# get a bounding box of the bluesky grid
bbox <- extent(bluesky_grid)
# get projection
wgs84 <- proj4string(bluesky_grid)

# load US county grid ----
# file needs to be unzipped first
co_path <- "./data/cb_2016_us_county_500k"
co_layer <- "cb_2016_us_county_500k"

# read county polygons
county <- readOGR(dsn = co_path, layer = co_layer)
# subset to counties in the us and assign wgs84 coord system
county <- spTransform(county, CRS(wgs84))

# Subsetting states in the continental US.
# I tried clipping, but i was losing information in the shapefile
# vector of FIPS that are not in the continental us
outside_bound <- c("60", "66", "15", "72", "78", "02", "81", "64", "67", 
                   "68", "69", "71")

# subset
us_county <- county[!(county$STATEFP %in% outside_bound), ]
# check
plot(us_county)

# convert to spatial polygon dataframe
us_county <- as(us_county, "SpatialPolygonsDataFrame")

# looks good; going to save this version of the shapefile
writeOGR(obj = us_county, dsn = "./data/us_county", layer = "us_county",
  driver = "ESRI Shapefile", overwrite_layer = T)
