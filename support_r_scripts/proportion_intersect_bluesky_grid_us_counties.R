# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US countys
# Purpose: To create a matrix of proportions to use in population-weighting
# Author: Ryan Gan
# Date Created: 6/28/17
# R Version: 3.3.3
# ------------------------------------------------------------------------------

# load libraries ----
library(rgdal)
library(rgeos)
# libraries to run in parallel

# load wrfgrid polygon ----
# define relative path to polygon file
poly_path <- "./data/bluesky_grid"
poly_layer <- "bluesky_grid"

# read grid polygon
bluesky_grid <- readOGR(dsn = poly_path, layer = poly_layer)
bluesky_grid

# load US county grid ----
co_path <- "./data/us_county"
co_layer <- "us_county"

# read county polygons
us_county <- readOGR(dsn = co_path, layer = co_layer)

# calculating overlap between the bluesky grid cells and counties ----
# expecting a large matrix of 3108 counties by 94068 grid cells
# save county ids (n = 3108) as a vector
county_id <- as.character(sort(us_county@data$COUNTYFP))
# assign a grid id to each bluesky cell (i could do this in the other step)
# save bluesky grid ids as a vector
bs_id <- as.character(sort(bluesky_grid@data$id))


