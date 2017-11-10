# ------------------------------------------------------------------------------
# Title: Population weighting bluesky output exposure by county
# Author: Ryan Gan
# Date Created: 2017-11-09
# Created under R Version: 3.4.2
# ------------------------------------------------------------------------------

# Note 2017-11-09: I think things may work faster with "sf" package rather than
# rgdal and raster but maybe not; server doesn't have ability to use "sf" anyways
# until R is updated

# General Formula (capital letters are matrices)
# ([PMGrid]%*%[PopulationGrid]%*%[PIGridCounty])%*%[CountyPopDen]^-1

# library ----------------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(raster)
library(ncdf4)

# data import ------------------------------------------------------------------
# section from the daily bluesky download where this code will be inserted

# smoke estimates by grid matrix -----------------------------------------------
# working with the raster brick of the nc file
nc_path <- paste0("./data/smoke_dispersion_v2.nc")
# brick or stack 
smk_brick <- brick(nc_path)

#test_grid <-SpatialPixels(SpatialPoints(smk_brick))

# calculate same day daily average ----
# create raster layer of same day mean value
# note Sept 13: changing to handle carry over smoke
same_day_smk <- smk_brick[[1:31]]
# create raster layer of mean value
same_day_mean_smk <- mean(same_day_smk)
# extract the date without timestamp (taking element date 29 frome 1:29)
same_day_date  <- as.numeric(substring(smk_brick@data@names, 2))[15]
# assign date time stamp in a format of month_day_year to bind with name
same_day_date <- format(as.POSIXct(same_day_date, origin="1970-1-1", tz="GMT"),
                        format = "%b %d %Y")

# calculate next day daily average -----
# subset raster brick to the 32th to 56th layer (next day MST)
next_day_smk <- smk_brick[[32:56]]
# create raster layer of daily mean value
next_day_mean_smk <- mean(next_day_smk)
# extract next day's date
next_day_date  <- as.numeric(substring(smk_brick@data@names, 2))[44]
# assign date time stamp in a format of month_day_year to bind with name
next_day_date <- format(as.POSIXct(next_day_date, origin="1970-1-1", tz="GMT"),
                        format = "%b %d %Y")

# next day vector 
# this way goes column by column, if I need to go by row, I will need to transpose t()
# as.matrix creats a 201 by 468 matrix
same_day_vector  <- as.vector(as.matrix(same_day_mean_smk))
# next day vector
next_day_vector <- as.vector(as.matrix(next_day_mean_smk))
# grid smoke values matrix (grid ids are rows and cols are dates)
smk_grid_matrix <- matrix(cbind(same_day_vector, next_day_vector), 
  # dims of matrix
  ncol=2, nrow=length(same_day_vector), 
  # names of rows and columns
  dimnames = list(paste0("grid",seq(1:length(same_day_vector))),
                  c("day1", "day2")))
# check grid
head(smk_grid_matrix)

# population by grid vector ----------------------------------------------------
# population net cdf file 
# need a matrix of population over grid Bonne provided; it's in nc format
# I believe it's the bluesky population.nc file bonne provided
bluesky_pop_path <- "./data/blueskypopulation.nc"
pop_nc <- nc_open(bluesky_pop_path)
print(pop_nc)

# extract coordinates
lon <- ncvar_get(pop_nc, "longitude")
nlon <- dim(lon)

lat <- ncvar_get(pop_nc, "latitude")
nlat <- dim(lat)
# lat and lon values dont' really make sense, but I don't think I need them
# if I assume this grid is the same blue sky grid
# still it would be good to check with Bonne on what's going on
# output matrix of total count per grid
pop_bluesky <- ncvar_get(pop_nc, "Population")
pop_bluesky <- t(pop_bluesky[,ncol(pop_bluesky):1])
# get pop density
popden_bluesky <- ncvar_get(pop_nc, "PopulationDensity")
# matrix needs to be fixed; I think columns need to be inverted and this works
# but check with bonne to do this right
popden_bluesky <- t(popden_bluesky[,ncol(popden_bluesky):1])

# add population density to smk raster to see if it proejcts right
raster_with_pop <- same_day_mean_smk
# need to transpose pop den matrix; hopefully i can fix this in the netcdf
raster_with_pop$popden <- popden_bluesky

# looks okay. a lot of smoke in San Fran area and populations in LA.
plot(raster_with_pop$popden, col=rev(RColorBrewer::brewer.pal(11, "RdBu")))
plot(raster_with_pop$layer, alpha=0.2, add=T)

# population per grid matrix ---------------------------------------------------
# convert population density matrix to vector
dim(popden_matrix)
popden_vector <- as.vector(popden_bluesky)
# make matrix
popden_matrix <- matrix(popden_vector, nrow=length(popden_vector), ncol=1,
  dimnames = list(paste0("grid",seq(1:length(same_day_vector))),"popden"))

# subset to California (fixed by making value columns integers)
cali_pi_grid <- read_csv("./data/california_prop_int.csv")
summary(cali_pi_grid)

# subset rater to just the grids in california
cali_id <- cali_pi_grid$id

# bbox for california
cali_bbox <- c(-124.40959, -114.13121, 32.53416, 42.00952)
# clip large raster to california
cali_raster <- crop(raster_with_pop, extent(cali_bbox))

plot(cali_raster$popden, col=rev(RColorBrewer::brewer.pal(11, "RdBu")))
plot(cali_raster$layer, alpha=0.5, add=T)

# population weighted average pm2.5 values for california counties -------------
dim(california_prop_int)
dim(smk_grid_matrix)

# set up matrices used
# subset the smoke grid by cells in california
# same day and next day value by grid
cali_smk_pm_matrix <- smk_grid_matrix[cali_id,]

# california proportion intersect grid to matrix
cali_pi_grid_county <- as.matrix(california_prop_int[,-1])
# set row names same as cali_smk_pm_matrix
rownames(cali_pi_grid_county) <- rownames(cali_smk_pm_matrix)
dim(cali_pi_grid_county)

# population density for california (this is actually a vector)
cali_popden_grid <- popden_matrix[cali_id,]
length(cali_popden_matrix)

# first step multiply the population vector of each grid by PM2.5 concentrations
# check
dim(cali_smk_pm_matrix)
head(cali_smk_pm_matrix)
head(cali_popden_matrix)
# multiply 
# this is the same operation as below but crashes on large matrices
# I need to remove this from past work (maybe not Washington)
# test <- diag(cali_popden_matrix) %*% cali_smk_pm_matrix  
pm_popden_matrix <- cali_popden_matrix * cali_smk_pm_matrix
head(pm_popden_matrix)

# next stop is to multiply the daily weighted value by the proportion intersect
# of the grid and county
dim(pm_popden_matrix)
dim(cali_pi_grid_county)
head(pm_popden_matrix)
head(t(cali_pi_grid_county[1:10,1:10]))
# strange, order needs to be this way or it will produce an error
pm_popden_county_matrix <- t(cali_pi_grid_county) %*% pm_popden_matrix
head(pm_popden_county_matrix)
# this should be the sum of smoke concentrations in each county
summary(t(pm_popden_county_matrix))

# estimate population density per county 
dim(cali_pi_grid_county)
# i think it needs to be a matrix
grid_pop_matrix <- t(as.matrix(cali_popden_grid))
dim(grid_pop_matrix)

county_pop_den <- t(grid_pop_matrix %*% cali_pi_grid_county)
# assign popden 
colnames(county_pop_den) <- "popdensity"

head(cali_pi_grid_county[,1:5])
head(grid_pop_matrix[,1:5])
head(county_pop_den)
dim(county_pop_den)
county_pop_den[county_pop_den == 0] <- 0.01

invert_pop_den <- 1/county_pop_den
# i think some of the counties will  have problems if I try and invert by density
# this may have been the issue I had earlier; curious if I'd get a similar answer
# if I used estimated population
dim(pm_popden_county_matrix)
dim(invert_pop_den)
county_pop_wt_pm <- invert_pop_den * pm_popden_county_matrix 

# STUFF I NEED TO FIGURE OUT



# grid county proportion intersect ---------------------------------------------
# may just use base R and not use tidyverse
grid_county_pi_df <- read_csv("./data/bluesky_prop_int.csv", 
  col_types = list(col_double())) 
# errors on import
problems(grid_county_pi_df)

summary(grid_county_pi_df[, 1:6])
dim(grid_county_pi_df)
# # convert to matrix and transpose so county are rows
# intersect_matrix <- t(as.matrix(grid_county_pi_df[,-1]))
# # set colnames
# colnames(intersect_matrix) <- paste0("grid",seq(1:length(same_day_vector)))
# 
# head(intersect_matrix[1:6,1:6])


# population_wt_function -------------------------------------------------------
# multiply population vector by pm concentration matrix for each day

# dimension of matrix of pm2.5 values per grid cell
dim(smk_grid_matrix)
# dimension of intersect matrix
dim(intersect_matrix)
dim(popden_matrix)
# Population density in each county
#[PI]%*%[PopDen]
popden_county <- intersect_matrix %*% popden_matrix

summary(popden_county)
# I will do this with population and see if it's a decent approximation to actual
# county populations


# multiple grid poulation vector by PM2.5 values
dim(smk_grid_matrix)
dim(popden_matrix)
# can't multiply by diagnol matrix of the pop vector; crashes
# maybe I don't do matrix multiplication?
pm_pop_matrix <- popden_vector * smk_grid_matrix










# next multiply pm pop matrix by proportion intersect matrix
dim(intersect_matrix)
dim(pm_pop_matrix)
county_wt_pm_matrix <- intersect_matrix %*% pm_pop_matrix
head(county_wt_pm_matrix)
summary(county_wt_pm_matrix)

# multiply (outer product) by inverse county pop vector
dim(county_wt_pm_matrix)
length(popden_county)
county_pm_smk_matrix <- county_wt_pm_matrix[,1] %o% (1/popden_county)

summary(county_smk_matrix)

# this isn't working, i need to subset to see what's going on
tail(county_smk_matrix)

# import california proportion intersect


