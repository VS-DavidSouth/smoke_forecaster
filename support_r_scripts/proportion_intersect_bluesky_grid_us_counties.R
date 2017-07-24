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

# assign a grid id to each bluesky cell (i could do this in the other step)
bluesky_grid$id <- as.character(seq(1:94068))
# save bluesky grid ids as a vector (not sure it needs to be character or numeric)
bs_id <- sort(bluesky_grid@data$id)
# reordering variables just because
bluesky_grid <- bluesky_grid[,2:1]

# load US county grid ----------------------------------------------------------
co_path <- "./data/us_county"
co_layer <- "us_county"

# read county polygons
us_county <- readOGR(dsn = co_path, layer = co_layer)

# save county ids (n = 3108) as a vector
county_id <- as.character(sort(us_county@data$COUNTYFP))

# Parallel computing of intersections ------------------------------------------
# calculating overlap between the bluesky grid cells and counties 
# creating empty large matrix of 3108 counties by 94068 grid cells
bs_county_proportion <- matrix(nrow=3108, ncol=94068, byrow=T,
   dimnames = list(county_id, bs_id))

# try a much smaller matrix to see if it works
plot(us_county)
plot(bluesky_grid, add=T)
# something seems off about the projection...