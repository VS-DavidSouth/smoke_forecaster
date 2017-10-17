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
bs_id <- as.character(bluesky_grid@data$id)

# reordering variables just because
bluesky_grid <- bluesky_grid[,2:1]

# load US county grid ----------------------------------------------------------
co_path <- "./data/us_county"
co_layer <- "us_county"

# read county polygons
us_county <- readOGR(dsn = co_path, layer = co_layer)

# save state/county ids (n = 3108) as a vector
county_id <- as.character(sort(us_county@data$GEOID))
plot(us_county)

# output colorado county using STATEFP
co_county <- us_county[us_county$STATEFP=="08", ]
plot(co_county)
# subset grid for just colorado
co_grid <- bluesky_grid[co_county, ]
plot(co_grid)
plot(co_county, add=T)

# works well with colorado, but colorado is a nice square, try with cali
ca_county <- us_county[us_county$STATEFP=="06", ]
# subset grid for cali
ca_grid <- bluesky_grid[ca_county, ]
plot(ca_grid)
plot(ca_county, add=T)


# I need to find a way to calculate chunks at a time. Maybe subset the state,
# and then find the grid boxes that fall within or overlap with that state and
# calculate? Then bind it all back together for the proportion intersect matrix.


# Parallel computing of intersections ------------------------------------------
# calculating overlap between the bluesky grid cells and counties 
# creating empty large matrix of 3108 counties by 94068 grid cells
# bs_county_proportion <- matrix(nrow=3108, ncol=94068, byrow=T,
#    dimnames = list(county_id, bs_id))


# define function to output seperate countis in a list
# vector of state/county fips fed in to function
co_poly_fun <- function(x){
  # name of county
  county_name <- as.character(x) 
  # limit shapefile to particular county
  county_shp <- us_county[us_county$GEOID == county_name, ]
  # convert to polygon
  county_poly <- SpatialPolygons(county_shp@polygons)
}

# define function to output seperate bluesky grid in a list
bs_poly_fun <- function(x){
  # name of grid
  grid_name <- as.character(x)
  # output bluesky grid
  bs_grid <- bluesky_grid[bluesky_grid$id == grid_name, ]
  # convert to spatial polygon
  bs_poly <- SpatialPolygons(bs_grid@polygons)
}

# create list of individual county polygons
county_poly_list <- lapply(county_id, co_poly_fun)
# create list of individual blue sky polygons
bs_poly_list <- lapply(bs_id, bs_poly_fun)

# numbers sequence of order to subset county (c) and grid (g)
# note Oct 11: this is going to vary now by number of ojects in county and grid
c <- rep(1:3108, each=94068)
g <- rep(1:94068, times=3108)


# setup for parallel computing before for loop ---------------------------------
cores <- detectCores() 
cl <- makeCluster(cores) # use all cores on the vet cluster
registerDoParallel(cl)
# load packages on each cluster
clusterCall(cl, function() library(rgdal))
clusterCall(cl, function() library(sp))
clusterCall(cl, function() library(rgeos))


# since I have another foreach loop, I need to load foreach on the clusters
clusterExport(cl, "county_poly_list", envir = .GlobalEnv)
clusterExport(cl, "bs_poly_list", envir = .GlobalEnv)
clusterExport(cl, "c", envir = .GlobalEnv)
clusterExport(cl, "g", envir = .GlobalEnv)

# start timing
start <- proc.time()

# define function to find intersection between each object in polygons lists
int_fun <- function(x,y){
  # find polygon of intersection
  county_bs_intersect <- gIntersection(county_poly_list[x][[1]], 
                                       bs_poly_list[y][[1]])
  # calculate proportion intersec
  grid_prop <- ifelse(is.null(county_bs_intersect), 0,
    gArea(county_bs_intersect)/gArea(bs_poly_list[y][[1]]))
}

# register parallel list object "proportion"
proportion <- mcmapply(int_fun, c, g)
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
write.csv(county_bs_proportion_df, file = paste0(write_path))

# test of intersection code ------
# 
# # test case: trying out just montana county with a bs grid 30057 ---
# mt_poly <- us_county[us_county$GEOID == "30057" |
#                      us_county$GEOID == "30001" |
#                      us_county$GEOID == "30011", ]
# bs_test_poly <- bluesky_grid[bluesky_grid$id == "22114" |
#                              bluesky_grid$id == "1", ]
# 
# # plot to check
# plot(bs_test_poly)
# plot(mt_poly, add = T)
# 
# # test intersection polygon
# test <- gIntersection(SpatialPolygons(mt_poly@polygons[1]),
#                       SpatialPolygons(bs_test_poly@polygons[2]))
# is.null(test)
 # need to divide by polygon
# gArea(test)/gArea(SpatialPolygons(bs_test_poly@polygons[2]))
 # g area of cell 22114 and county 30057 should be 0.862
 # g area of cell 22114 and county 30001 should be 0.14
# 
# # trying mapply
# # create list of individual county polygons
# county_poly_list <- lapply(c("30057", "30001", "30011"), co_poly_fun)
# # create list of individual blue sky polygons
# bs_poly_list <- lapply(c("22114", "1"), bs_poly_fun)
# 
# int_fun
# summary(county_poly_list[3])
# int_fun
# 
# # when using m apply or mcmapply, x vector for county polygons needs to use each
# x <- rep(1:3, each = 2)
# y <- rep(1:2, times = 3)
# 
# test_prop <- mapply(int_fun, x, y)
# test_prop
# 
# test_mat <- matrix(test_prop, nrow=3, ncol=2, byrow=T,
# dimnames = list(c("30057", "30001", "30011"), c("22114", "1")))
# 
# test_mat
