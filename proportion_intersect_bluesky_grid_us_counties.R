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

# load raster brick ----
nc_path <- "smk_stack_raster.nc"

# using single raster layer of next day average
smk_forecast <- brick(nc_path)

# subset to one layer and plot grid
bluesky_grid <- rasterToPolygons(smk_forecast[[1]])
plot(bluesky_grid)
bluesky_grid

# get ids of cells
test <- sapply(slot(bluesky_grid, "polygons"), function(x) slot(x, "ID"))
head(test)
