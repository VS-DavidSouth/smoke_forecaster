# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US countys
# Purpose: To create a matrix of proportions to use in population-weighting
# Author: Ryan Gan
# Date Created: 2017-12-20
# R Version: 3.4.2
# ------------------------------------------------------------------------------

# Note I've revised code to use the SF function to calculate intersections

# load libraries ----
library(tidyverse)
library(sf)
# parallel compuation
library(doParallel)
library(parallel)

# read in bluesky grid sf and us county sf --------
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
us_county <- st_read(dsn = co_path, layer = co_layer)

# create empty tibble 
# tibble of just the grid_id to join data too
prop_int_tibble <- bluesky_grid$id %>% 
  tibble() %>% 
  rename(grid_id = ".")

# set up parallel computing ----
# find number of processing nodes (cores)
cores <- detectCores() # using all the cores
# make cluster/node
cl <- makeCluster(cores)
# register do parallel cluster
registerDoParallel(cl)

# load packages on each processor of the node/cluster
clusterCall(cl, function() c(library(tidyverse), library(sf)))

# export global sf objects and empty tibble to each core
clusterExport(cl, c("bluesky_grid", "us_county", "prop_int_tibble"), 
              envir = .GlobalEnv)


# for loop ----
# start time 
start_time <- Sys.time()

foreach(i=1:length(us_county$FIPS)) %dopar% {
  # subset county to find intersect
  county <- slice(us_county, i)
  # extract fips number for variable name
  fips_id <- paste0("fips_", county$FIPS)
  # subset grid cells that touch any part of the county
  grid <- cali_grid[county,]
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

# stop cluster
stopCluster(cl)

stop_time <- Sys.time()
compute_time <- stop_time - start_time
# run time
compute_time

# that was fast enough and easier to follow
summary(prop_int_tibble)

# set missing NA values to 0
bluesky_county_pi <- prop_int_tibble %>% 
  mutate_all(funs(replace(., is.na(.), 0)))

# save final proportion intersect ----
save_path <- "./data/bluesky_county_prop_intersect.csv"
write_csv(bluesky_county_pi, save_path)