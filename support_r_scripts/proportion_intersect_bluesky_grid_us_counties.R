# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US countys
# Purpose: To create a matrix of proportions to use in population-weighting
# Author: Ryan Gan
# Date Created: 2017-12-20
# R Version: 3.4.2
# ------------------------------------------------------------------------------

# TODO: Figure out if this needs to be run in cron. 

# Note I've revised code to use the SF function to calculate intersections

# load libraries ----
library(tidyverse)
library(sf)
# parallel compuation
# library(doParallel)
# library(parallel)

################################################################################
# read in bluesky grid sf and us county sf
################################################################################

# define relative path to polygon file
poly_path <- "./data/bluesky_grid"
poly_layer <- "bluesky_grid"

# county path
co_path <- "./data/us_county"
co_layer <- "us_county"

# read in bluesky grid
bluesky_grid <- sf::st_read(dsn = poly_path, layer = poly_layer)

# the bluesky grid does not have an ID so we will assign each cell a number 
# that is simply based on the index of the grid. The total number here comes
# from nLat x nLon of the most recently downloaded smoke_dispersion nc file.
bluesky_dim <- dim(bluesky_grid)
bluesky_grid$id <- as.numeric(seq(1:bluesky_dim[1]))

# read county polygons. These should not change, unless Texas leaves the union
# or PR joins. 
us_county <- sf::st_read(dsn = co_path, layer = co_layer) %>% 
  # making a FIPS ID that I'll use later
  mutate(FIPS = paste0(STATEFP, COUNTYFP))

# create empty tibble 
# tibble of just the grid_id to join data too
prop_int_tibble <- bluesky_grid$id %>% 
  tibble() %>% 
  rename(grid_id = ".")

# TODO: Set up parallel computing 

################################################################################
# Loop through each US county. 
################################################################################
# start time 
start_time <- Sys.time()
nCounty <- length(us_county$FIPS)

print("Calculating county overlap with bluesky grids. This is a large slow loop.")
print(paste("There are", nCounty, "counties to process"))

# Turn off warnings, Areas are small not near the pole, so the warnings
# "although coordinates are longitude/latitude, st_intersection assumes that they are planar"
# can be ignored in this loop. Put these warnings messages into a file using sink. 
options(warn=-1) # turns off warnings
for(i in 1:nCounty){
  
  # subset county to find intersect
  county <- dplyr::slice(us_county, i)
  
  # extract fips number for variable name
  fips_id <- paste0("fips_", county$FIPS)
  
  # subset bluesky grid cells that touch any part of the county (warns)
  suppressMessages(grid <- bluesky_grid[county,])
  
  # subset the intersected polygon (warns)
  suppressMessages(inter_poly <- sf::st_intersection(grid, county) %>% 
    # filter only to polygon or multipolygon type 
    # to avoid errors with point or line types
    filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON"))==T)
    )
  
  # filter grid ids to only grids in the inter_poly object
  grid2 <- grid %>% filter(id %in% inter_poly$id) 
  
  # find proportion intersect with original grid
  prop_int <- as.numeric(sf::st_area(inter_poly)/sf::st_area(grid2))
  
  # subset grid id
  grid_id <- grid2$id
  
  # make a tibble
  county_grid_int <- tibble(grid_id, prop_int) %>% 
    set_names(c("grid_id", fips_id))
  
  # join with full tibble
  prop_int_tibble <- prop_int_tibble %>% 
    left_join(county_grid_int, by = "grid_id")
  
  if(i%%100==0){
    print(paste(i/nCounty*100,"% complete"))
  }
  
} # end of country loop
options(warn=0) # turn warnings back on


# stop cluster
#stopCluster(cl)

stop_time <- Sys.time()
compute_time <- stop_time - start_time

# run time
compute_time

# set missing NA values to 0
bluesky_county_pi <- prop_int_tibble %>% 
  mutate_all(funs(replace(., is.na(.), 0)))

# # output some counties to check intersect
# cali_county <- bluesky_county_pi %>% 
#   select(grid_id, contains("fips_06"))
# 
# # checking LA county against my markdown file.
# la_county <- cali_county %>% 
#   select(grid_id, fips_06037) %>% 
#   filter(fips_06037 > 0)

# save final proportion intersect ----
# TODO: make new name, do not overwrite right away 
save_path <- "./data/bluesky_county_prop_intersect_TEST.csv"
write_csv(bluesky_county_pi, save_path)

# # save california fips for checking
# save_cali <- "./data/cali_county_pi.csv"
# write_csv(cali_county, save_cali)


