# ------------------------------------------------------------------------------
# Title: Proportion intersect between bluesky grid and US counties using "sf"
# Purpose: Create a matrix of proportions to use in population-weighting of smoke
#          values using the "sf" package
# Author: Ryan Gan
# Date Created: 2017-10-16
# R Version: 3.4.2
# ------------------------------------------------------------------------------

# General note: The sf package makes proportion intersect cacluations faster
# and it can run on a personal computer rather than on a server. I

# load libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)

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

summary(bluesky_grid)
summary(us_county)
plot(us_county$geometry)

# custom functions -------------------------------------------------------------
# proportion_intersect ----
# custom function (I should write this to a package)
proportion_intersect <- function(poly_sf, poly_id, grid_sf, grid_id){
  # enquo lazy eval
  poly_id <- enquo(poly_id)
  grid_id <- enquo(grid_id)
  # subset grid that contains poly_i
  grid_i <- grid_sf[poly_sf,]
  # proportion intersect
  intersect_sf <- st_intersection(grid_i, poly_sf) %>% 
    # filter only to polygon or multipolygon type 
    # to avoid errors with point or line types
    filter(st_is(., c("POLYGON", "MULTIPOLYGON"))==T)
  # calculation of proportion intersect
  proportion <- as.numeric(st_area(intersect_sf)/st_area(grid_i)) %>% 
    data_frame() %>% rename(proportion = ".")
  # column bind the proportion to the intersect sf object
  output_df <- intersect_sf %>% 
    # eventually replace these with generic names
    select(!!grid_id, !!poly_id) %>% 
    bind_cols(proportion)
  # remove geometry
  st_geometry(output_df) <- NULL
  return(output_df)
}

# proportion intersect data frame ----
# this custom function creates the proportion intersect data frame that can be
# converted to a matrix for population-weighting
pi_matrix <- function(grid_sf, grid_id, prop_int_df, poly_id){
  # lazy eval variables
  grid_id <- enquo(grid_id)
  poly_id <- enquo(poly_id)

  # remove geometry object from grid
  st_geometry(grid_sf) <- NULL
  # prep grid dataframe
  grid_id_column <- grid_sf %>% select(!!grid_id)
  
  # full grid joined with proportion intersect grid
  output_df <- grid_id_column %>% 
    # join grid ids to grids with proportion values
    left_join(prop_int_df, by = as.character(grid_id[2])) %>% 
    mutate(poly = as.factor(paste0("poly", !!poly_id))) %>%
    # it may be important to remove the poly_id so spread just has the 
    # grid id, new poly id, and the proportions to work with
    select(-!!poly_id) %>% 
    # remove duplicates
    filter(!duplicated(.)) %>% 
    # spread
    spread(poly, proportion) %>% 
    # mutate missing to 0 at each poly var
    mutate_at(vars(contains("poly")), funs(ifelse(is.na(.), 0,
                                                  ifelse(.>1, 1, .)))) %>% 
    # remove idNA
    select(-polyNA) 
  
  # output dataframe
  return(output_df)
}  

# calculate proportion intersect -----------------------------------------------
start_time <- Sys.time()
county_bluesky_pi <- proportion_intersect(poly = us_county, poly_id = FIPS,
                                          grid = bluesky_grid, grid_id = id)
stop_time <- Sys.time()
compute_time <- stop_time - start_time
compute_time
# warning messages but should work; took ~3 minutes

start_time <- Sys.time()
# create proportion intersect dataframe(matrix)
bluesky_prop_int <- pi_matrix(grid_sf = bluesky_grid, grid_id = id, 
  prop_int_df = county_bluesky_pi,poly_id = FIPS)
stop_time <- Sys.time()
compute_time <- stop_time - start_time
compute_time
# took 4 minutes; saving the file as a dataframe csv
# it will need to be converted to a matrix and the id column will need to be 
# assigned to row variable; I should do a small area of the grid check to make
# sure it matches; will do this later

# save final bluesky product ---------------------------------------------------
save_path <- "./data/bluesky_prop_int.csv"
write_csv(bluesky_prop_int, save_path)



