# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US counties using "sf"
# Purpose: Create a matrix of proportions to use in population-weighting of smoke
#          values using the "sf" package
# Author: Ryan Gan
# Date Created: 2017-10-16
# R Version: 3.4.2
# ------------------------------------------------------------------------------

# General note: The sf package makes proportion intersect cacluations faster
# and it can run on a personal computer rather than on a server. I'd still like
# to figure out how to use it in either an apply- or map-type function, but the
# for loop will work for now since I would like to move on with the project.

# load libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
# parallel libraries
library(foreach)
library(doParallel)

# load shapefiles/polygons using st_read ---------------------------------------
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

# subset LA county FIPS 06037; used to check the final product -----------------
la_county <- us_county %>% filter(FIPS == "06037")
# subset grids to orange county
la_grid <- bluesky_grid[la_county, ]
# find intersection of grid for the county
grid_area <- st_intersection(st_geometry(la_grid),st_geometry(la_county))
# caluclate proportion intersect 
prop_int <- as.numeric(st_area(grid_area)/st_area(la_grid))
# now I need to assign it back to the county shapefile
la_grid$proportion <- prop_int

# output just the id and proportion intersect and label it 1
la_grid_prop_int1 <- la_grid %>% select(id, proportion) %>% 
  rename(fips_06037 = proportion)
# remove geometry
st_geometry(la_grid_prop_int1) <- NULL

# creating a tibble of the grid_id to join data with ---------------------------
prop_int_tibble <- bluesky_grid$id %>% 
  tibble() %>% 
  rename(grid_id = ".")

# setup for parallel computing for parallel for loop ---------------------------
cores <- detectCores() 
cl <- makeCluster(cores) # use all cores on the vet cluster
registerDoParallel(cl)
# load packages on each cluster
clusterCall(cl, function() library(sf))
clusterCall(cl, function() library(tidyverse))

# for loop to calcuate intersection of grids in each US county -----------------
# start time
start_time <- Sys.time()

# for each loop
foreach(i=1:length(us_county$FIPS), .combine=cbind, .inorder=T) %dopar% {
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

# took 1.79 hours to run

# check some of the tibble
summary(prop_int_tibble[,1:10])

# setting missing values to 0
bluesky_prop_int <- prop_int_tibble %>% 
  mutate_at(-grid_id, ifelse(is.na(.),0,.))

# check the LA county FIPS from the large dataframe and compare against test
la_grid_prop_int2 <- bluesky_prop_int %>% 
  select(fips_06037) %>% filter(fips_06037 != 0)

# test if identical
identical(la_grid_prop_int1, la_grid_prop_int2)
summary(la_grid_prop_int1)
summary(la_grid_prop_int2)

# save final bluesky product ---------------------------------------------------
save_path <- "./data/bluesky_prop.csv"
write_csv(bluesky_prop_int, save_path)

