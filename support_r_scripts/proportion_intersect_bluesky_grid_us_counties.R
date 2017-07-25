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
library(parallel)
library(doParallel)


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

# save state/county ids (n = 3108) as a vector
county_id <- as.character(sort(us_county@data$GEOID))

# Parallel computing of intersections ------------------------------------------
# calculating overlap between the bluesky grid cells and counties 
# creating empty large matrix of 3108 counties by 94068 grid cells
# bs_county_proportion <- matrix(nrow=3108, ncol=94068, byrow=T,
#    dimnames = list(county_id, bs_id))


# define function to output seperate countis in a list
# vector of state/county fips fed in to function
co_poly_func <- function(x){
  # name of county
  county_name <- as.character(x) 
  # limit shapefile to particular county
  county_shp <- us_county[us_county$GEOID == county_name, ]
  # convert to polygon
  county_poly <- SpatialPolygons(county_shp@polygons)
}

# define function to output seperate bluesky grid in a list
bs_poly_func <- function(x){
  # name of grid
  grid_name <- as.character(x)
  # output bluesky grid
  bs_grid <- bluesky_grid[bluesky_grid$id == grid_name, ]
  # convert to spatial polygon
  bs_poly <- SpatialPolygons(bs_grid@polygons)
}

# create list of individual county polygons
county_poly_list <- lapply(county_id, co_poly_func)
# create list of individual blue sky polygons
bs_poly_list <- lapply(bs_id, bs_poly_func)

# empty numbers list
z <- rep(1:3108, each=94068)
w <- rep(1:94068, each=3108)

# setup for parallel computing before for loop ---------------------------------
cores <- detectCores() # 48
cl <- makeCluster(cores) # use all cores on the vet cluster
registerDoParallel(cl)
# load packages on each cluster
clusterCall(cl, function() library(rgdal))
clusterCall(cl, function() library(sp))
clusterCall(cl, function() library(rgeos))

# since I have another foreach loop, I need to load foreach on the clusters
clusterExport(cl, "county_poly_list", envir = .GlobalEnv)
clusterExport(cl, "bs_poly_list", envir = .GlobalEnv)
clusterExport(cl, "z", envir = .GlobalEnv)
clusterExport(cl, "w", envir = .GlobalEnv)

# start timing
start <- proc.time()

# define function to find intersection between each object in polygons lists
intersect_func <- function(x,y){
  # find polygon of intersection
  county_bs_intersect <- gIntersection(county_poly_list[x][[1]], 
                                       bs_poly_list[y][[1]])
  # calculate proportion intersec
  grid_prop <- ifelse(is.null(county_bs_intersect), 0,
    gArea(county_bs_intersect)/gArea(bs_poly_list[y][[1]]))
}

# register parallel list object "proportion"
proportion <- mcmapply(intersect_func, z, w)
# end time
stop <- proc.time() - start
stop

# stop cluster
stopCluster(cl)

# save proportion as matrix
county_bs_proportion_mat <- matrix(proportion, nrow=3108, ncol=94068, byrow=T,
  dimnames = list(county_id, bs_id))

county_bs_proportion_df <- data.frame(county_bs_proportion_mat)

write_path <- paste0('./data/county_bluesky_prop.csv')
write_csv(county_bs_proportion_df, paste0(write_path))




# test of interseciton code ------
# 
# mt_list <- lapply()
# # test case: trying out just montana county with a bs grid 30057 ---
# mt_poly <- us_county[us_county$GEOID == "30057", ]
# bs_test_poly <- bluesky_grid[bluesky_grid$id == "22114", ]
# 
# plot(mt_poly)
# plot(bs_test_poly, add = T)
# # test intersection polygon
# test <- gIntersection(SpatialPolygons(mt_poly@polygons),SpatialPolygons(bs_test_poly@polygons))
# # need to divide by polygon
#gArea(test)/gArea(SpatialPolygons(bs_test_poly@polygons))
# g area of cell 22114 should be 0.862
     
