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

# output matrix of total count per grid
pop_bluesky <- ncvar_get(pop_nc, "Population")
# get pop density
popden_bluesky <- ncvar_get(pop_nc, "PopulationDensity")
# matrix needs to be fixed; I think columns need to be inverted and this works

# population per grid matrix ---------------------------------------------------
# convert population density matrix to vector
dim(popden_matrix)
popden_vector <- as.vector(popden_bluesky)
# make matrix
popden_matrix <- matrix(popden_vector, nrow=length(popden_vector), ncol=1,
  dimnames = list(paste0("grid",seq(1:length(same_day_vector))),"popden"))

# convert population matrix to vector
pop_vector <- as.vector(pop_bluesky)
# make matrix
pop_matrix <- matrix(pop_vector, nrow=length(pop_vector), ncol=1,
  dimnames = list(paste0("grid",seq(1:length(same_day_vector))),"population"))

head(pop_matrix)
summary(pop_matrix)

# intersection dataframe -------------------------------------------------------
grid_county_pi <- read_csv("./data/bluesky_prop_int.csv")
summary(grid_county_pi[,1:10])

# convert to matrix
grid_county_pi_matrix <- as.matrix(grid_county_pi[,-1])
# estimate population for each county ------------------------------------------

# make sure matrix dimensions allow for matrix algebra
dim(pop_matrix)
dim(grid_county_pi)
head(pop_matrix)
head(t(grid_county_pi_matrix[1:10,1:10]))
# county names
county_names <- colnames(grid_county_pi_matrix)


# population per county
county_population <- t(pop_matrix) %*% grid_county_pi_matrix

# convert to matrix
county_population_matrix <- matrix(county_population,
  nrow = length(county_names), ncol = 1, 
  dimnames = list(county_names, "population"))

head(county_population_matrix)
# something is very wrong with the population estimates;
# the most populated county is in south carolina 

# subset to califronia ---------------------------------------------------------
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

summary(cali_raster$popden)
# population weighted average pm2.5 values for california counties -------------

# set up matrices used
# subset the smoke grid by cells in california
# same day and next day value by grid
cali_smk_pm_matrix <- smk_grid_matrix[cali_id,]

# california proportion intersect grid to matrix
cali_pi_grid_county <- as.matrix(california_prop_int[,-1])
# set row names same as cali_smk_pm_matrix
rownames(cali_pi_grid_county) <- rownames(cali_smk_pm_matrix)
dim(cali_pi_grid_county)
cali_id
# population density for california (this is actually a vector)
cali_pop_grid <- pop_matrix[cali_id,]
length(cali_pop_grid)
# convert to matrix 
cali_pop_matrix <- as.matrix(cali_pop_grid)

# first step multiply the population vector of each grid by PM2.5 concentrations
# check
dim(cali_smk_pm_matrix)
head(cali_smk_pm_matrix)
head(cali_pop_matrix)
summary(cali_smk_pm_matrix)
summary(cali_raster$layer)
# something is off the smk pm matrix doesn't match the values in the raster
summary(smk_grid_matrix)
summary(raster_with_pop$layer)
# i think the subset is messed up

# multiply 
# this is the same operation as below but crashes on large matrices
# I need to remove this from past work (maybe not Washington)
# test <- diag(cali_popden_matrix) %*% cali_smk_pm_matrix  
pm_popden_matrix <- cali_pop_matrix * cali_smk_pm_matrix

head(pm_popden_matrix)
head(cali_smk_pm_matrix)
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
colnames(invert_pop_den) <- NULL
# matrix multiplicaiton won't work.
county_pop_wt_pm <- pm_popden_county_matrix[,1] * invert_pop_den 

napa_pm <- pm_popden_county_matrix[rownames(pm_popden_county_matrix)=="poly06025",]
napa_popden <- county_pop_den[rownames(county_pop_den)=="poly06025",]

napa_pm
napa_popden

summary(pm_popden_county_matrix)
head(pm_popden_county_matrix)

county_names <- rownames(pm_popden_county_matrix)
# turn rownnames in to FIPs variable
cali_pm_pop_wt <- pm_popden_county_matrix %>% 
  as_data_frame() %>% 
  cbind(county_names) %>% 
  mutate(FIPS = stringr::str_sub(county_names, start = 5L))



# maybe I don't need to divide by population density? maybe I need to divide
# by population? try that way next time.
# seeing what would happen by population

# assign pm values to counties and plot to view ----
# i'll remove this later
california_pm <- california_sf %>% 
  left_join(cali_pm_pop_wt, by = "FIPS")

# join pm vals to grid
grid_names <- rownames(cali_smk_pm_matrix)

cali_pm_gridid <- cali_smk_pm_matrix %>% 
  as_data_frame() %>% 
  cbind(grid_names) %>% 
  mutate(id = as.numeric(stringr::str_sub(grid_names, start=5L)))

head(cali_pm_gridid)

california_grid_pm <- california_grid %>% 
  left_join(cali_pm_gridid, by = "id")



ggplot(data=california_pm) +
  geom_sf(aes(fill=day1)) +
  scale_fill_gradient(name = "Pop Wt Smoke", low = "white", high = "red") +
  theme_bw()

ggplot(data=california_grid_pm) +
  geom_sf(aes(fill=day1*10)) +
  scale_fill_gradient(name = "Grid Smoke", low = "white", high = "red") +
  theme_bw()

summary(california_grid_pm)
# projection looks off




# subset to California (fixed by making value columns integers)
# set up matrices used
# subset the smoke grid by cells in california
# same day and next day value by grid
cali_smk_pm_matrix <- smk_grid_matrix[cali_id,]

# california proportion intersect grid to matrix
cali_pi_grid_county <- as.matrix(california_prop_int[,-1])
# set row names same as cali_smk_pm_matrix
rownames(cali_pi_grid_county) <- rownames(cali_smk_pm_matrix)
dim(cali_pi_grid_county)







