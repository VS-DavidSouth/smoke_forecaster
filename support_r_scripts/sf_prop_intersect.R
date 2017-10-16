# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US counties using "sf"
# Purpose: Create a matrix of proportions to use in population-weighting of smoke
#          values using the "sf" package
# Author: Ryan Gan
# Date Created: 2017-10-16
# R Version: 3.4.2
# ------------------------------------------------------------------------------

# General note: The sf package makes proportion intersect cacluations a lot faster
# and it can run on a personal computer rather than on a server. I'd still like
# to figure out how to use it in either an apply- or map-type function, but the
# for loop will work for now since I would like to move on with the project.

# load libraries
library(sf)
library(tidyverse)

# load shapefiles/polygons using st_read ----
# define relative path to polygon file
poly_path <- "./data/bluesky_grid"
poly_layer <- "bluesky_grid"
# county path
co_path <- "./data/us_county"
co_layer <- "us_county"

# read in bluesky grid
bluesky_grid <- st_read(dsn = poly_path, layer = poly_layer)
# the bluesky grid does not have an ID so we will assign each cell a number
bluesky_grid$id <- as.numeric(seq(1:94068))
# read county polygons
us_county <- st_read(dsn = co_path, layer = co_layer) %>% 
  mutate(FIPS = paste0(STATEFP, COUNTYFP))

# creating a tibble of the grid_id to join data with ----
prop_int_tibble <- bluesky_grid$id %>% 
  tibble() %>% 
  rename(grid_id = ".")

# for loop to calcuate intersection of grids in each US county ----
# start time
start_time <- Sys.time()

# for loop
for(i in 1:length(us_county$FIPS)){
  # subset county to find intersect
  county <- slice(us_county, i)
  # extract fips number for variable name
  fips_id <- paste0("fips_", county$FIPS)
  # subset grid cells that touch any part of the county
  grid <- bluesky_grid[county,]
  # output grid IDs
  grid_id <- grid$id
  # subset the intersected polygon
  inter_poly <- st_intersection(st_geometry(grid),st_geometry(county))
  # find proportion intersect with original grid
  prop_int <- as.numeric(st_area(inter_poly)/st_area(grid))
  # make a tibble
  prop_int_tibble <- tibble(grid_id, prop_int) %>% 
    set_names(c("grid_id", fips_id)) %>% 
    right_join(prop_int_tibble, by = "grid_id")
} # end loop

# end time
end_time <- Sys.time()

# difference in time
total_time <- end_time - start_time
total_time

# remember to check and save before you exit. 
# I expect a tibble with 94068 rows and 3108 columns