# ------------------------------------------------------------------------------
# Title: Population weighting bluesky output exposure by county
# Author: Ryan Gan
# Date Created: 2017-11-09
# Created under R Version: 3.4.2
# ------------------------------------------------------------------------------

# Note 2017-11-09: I think things may work faster with "sf" package rather than
# rgdal and raster but maybe not; server doesn't have ability to use "sf" anyways
# until R is updated

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

# grid county proportion intersect ---------------------------------------------
# may just use base R and not use tidyverse
grid_county_pi_df <- data.table::fread("./data/bluesky_prop_int.csv") 
head(grid_county_pi_df[1:6, 1:6])
dim(grid_county_pi_df)
# convert to matrix and transpose so county are rows
intersect_matrix <- t(as.matrix(grid_county_pi_df[,-1]))
# set colnames
colnames(intersect_matrix) <- paste0("grid",seq(1:length(same_day_vector)))

head(intersect_matrix[1:6,1:6])
# population_wt_function -------------------------------------------------------
# multiply population vector by pm concentration matrix for each day

# dimension of matrix of pm2.5 values per grid cell
dim(smk_grid_matrix)
# dimension of intersect matrix
dim(intersect_matrix)
dim(popden)
# Population density in each county
#[PI]%*%[PopDen]
popden_county <- intersect_matrix %*% popden_matrix

summary(popden_county)
# I will do this with population and see if it's a decent approximation to actual
# county populations
