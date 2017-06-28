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

# load raster brick ----
nc_path <- "smk_stack_raster.nc"

# using single raster layer of next day average
smk_forecast <- brick(nc_path)

# subset to one layer and plot grid
bluesky_grid <- smk_forecast[[1]]