#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  machine_name <- args[1]
}else{
  machine_name <- "local"
}
print(paste("Passed arguments:"))
print(args)

# ------------------------------------------------------------------------------
# Title: Daily BlueSky forecast download and data management
# Authors: Ryan Gan & Steven Brey 
# Date Created: 6/19/2017. Heavy modification began 7/20/2018. See github.
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------
#
# This is run in a crontab. The following code shows the current setting for
# the crontab on the server computer:
#
# 00 10 * * * Rscript APP_DIR/support_r_scripts/daily_bluesky_download.R salix
#
# Note: Code directly from mazamascience to download their USFS BlueSky runs
# http://mazamascience.com/Classes/PWFSL_2014/Lesson_07_BlueSky_FirstSteps.html#downloadbsoutput\

# libraries needed
library(ncdf4) 
library(stringr)
library(raster) # easier to manipulate than netcdf file
library(rgdal)
library(RCurl)
library(lubridate) # for day()
library(data.table) # will interfere with lubridate so use libridate::FUNCTION

# TODO: consider these as possible user arguments 
# RG 2018-08-16: Ask Steve to explain his thought here
# SB 2018-08-21: I think we may use different models output in the future. 
model <- "GFS-0.15deg"
PMThresh <- 2

if(machine_name == "salix"){
  
  print("-----------------------------------------------")
  print("Code running on salix")
  setwd("/srv/www/rgan/smoke_forecaster/")
  # define path to repository for the server for writing files
  home_path <- paste0("/srv/www/rgan/smoke_forecaster/")
  print(paste("Working directory set to:", home_path))
  print("-----------------------------------------------")
  
}else{
  # Local development taking place. 
  home_path <- paste0(getwd(), "/Smoke_Predictor/")
}

# download bluesky daily output -----------------------------------------------

# download fire locations from bluesky runs ----
# note right now I download only the location file, but I may work in
# fire information in the future. looks like it's contained in the json file
# Note: changed "forecast" to "combined" estimate on Sept 5 2017
url_base <- paste0("https://smoke.airfire.org/bluesky-daily/output/standard/", model,"/")

# Check to see if todays date 12Z forecast exists. 
# TODO: These if statements need to be a while loop that goes back in time 
# TODO: until the condition is satisfied. 
check_for_files <- function(URL){
  
  fire_locations_test <- FALSE
  smoke_dispersion_test <- FALSE
  
  # See if fire_locations.csv exists, if it does, download and check for size.
  fire_locations_file <- paste0(URL, "/fire_locations.csv")
  if(url.exists(fire_locations_file)){
    
    fire_locations_dest <- paste0(home_path, "data/fire_locations_test.csv")
    download.file(url = fire_locations_file,
                  destfile = fire_locations_dest, 
                  mode = "wb")
    
    # Now check the size of the downloaded file
    if (file.info(fire_locations_dest)$size > 2){
      fire_locations_test <- TRUE
    }
  }
  
  # See if smoke_dispersion.nc exists, if it does, download and check for size.
  smoke_dispersion_file <- paste0(URL, "/smoke_dispersion.nc")
  if(url.exists(smoke_dispersion_file)){
    
    smoke_dispersion_dest <- paste0(home_path, "data/smoke_dispersion_test.nc")
    download.file(url = smoke_dispersion_file,
                  destfile = smoke_dispersion_dest, 
                  mode = "wb")
    
    # Now check the size of the downloaded file
    if (file.info(smoke_dispersion_dest)$size > 2){
      smoke_dispersion_test <- TRUE
    }
  }
  
  # Every condition needs to be true in order to use data from this model run  
  combined_check <- c(fire_locations_test, smoke_dispersion_test)
  
  if(all(combined_check)){
    
    result <- TRUE
    # When we have both, rename these files, get rid of appended "test" at 
    # end of name
    file.rename(fire_locations_dest, paste0(home_path, "data/fire_locations.csv"))
    file.rename(smoke_dispersion_dest, paste0(home_path, "data/smoke_dispersion.nc"))
    
  } else{
    result <- FALSE
  }
  return(result)
  
}

dataSearch <- TRUE # while this is T, search for data 
loopCount  <- 0
date_to_check <- paste0(gsub("-","", Sys.Date())) # Start with today
while(dataSearch){
  
  print(paste("Searching for data on:", date_to_check))
  
  # Search for 12Z forecast for this date
  URL <- paste0(url_base, date_to_check, "12/combined/data")
  dataSearch <- !check_for_files(URL)
  forecast_hour <- "12"
  
  # If no 12Z forecast available for this date, check for 00Z forecast instead
  if(dataSearch){
    URL <- paste0(url_base, date_to_check, "00/combined/data")
    dataSearch <- !check_for_files(URL)
    forecast_hour <- "00"
  }
  # Store for the user, in case this is the last time through the loop 
  forecast_date <- date_to_check
  
  # Go back a day in the POSIXct space
  loopCount <- loopCount + 1
  date_to_check <- paste0(gsub("-","", Sys.Date()-loopCount))
  
}
 
print(paste("Fire and smoke data aquired for:", forecast_date, forecast_hour))

# Create a file connection that will log what this script tries to do and when
# it tries to do it. 
download_log <- file("bluesky_download_log.txt")
line1 <- paste("Download log for:", Sys.time())
line2 <- paste("Forecast Date:", forecast_date)  
line3 <- paste("Forecast Hour:", forecast_hour)

# Re-save this as an Rdataframe with subset rows so that the app runs faster
load_try <- try(fire_locations <- read.csv(paste0(home_path, "data/fire_locations.csv")), silent=T)
if(class(load_try)=="try-error"){
  stop("The fire_locations.csv contain no data. Halting download script.")
} else{
  print("fire_locations.csv contain data.")
}

# We do not want or need most rows of fire_locations
column_mask <- names(fire_locations) %in% c("latitude", "longitude", "type", "area")
fire_locations <- fire_locations[,column_mask]
paste0(home_path, "data/smoke_dispersion.nc")
save(fire_locations, file=paste0(home_path, "data/fire_locations.RData"))

line4 <- paste("Both fire locations and smoke dispersion downloaded from")
line5 <- paste(URL)
line6 <- paste("Time complete:",Sys.time())
writeLines(c(line1, line2, line3, line4, line5, line6), con=download_log)
close(download_log) 

################################################################################
# saved smoke netcdf file manipulaton 
################################################################################
# TODO: Create a new log that documents the processing of these nc data. 
fileName <- paste0(home_path, "data/smoke_dispersion.nc")

# This function loads the most recently downloaded smoke dispersion .nc file
# and uses the global attributes within that file to create a version of the file
# with nicer dimension labels. The new nc file is saved with the same name with
# "v2 appended. 
bs2v2 <- function(fileName) {

  # open nc file
  old_nc <- nc_open(fileName)
  
  # Create latitude and longitude axes 
  # Current (index) values
  row <- old_nc$dim$ROW$vals # lat 
  col <- old_nc$dim$COL$vals # lon
  
  # Useful information is found in the global attributes
  globalAttributes <- ncatt_get(old_nc, varid=0) # varid=0 means 'global'
  # NOTE: Use names(globalAttributes) to see the names of the elements contained 
  # NOTE: in this list
  # NOTE:  globalAttributes is of class 'list'
  # NOTE:  Access list elements with either 'listName[[objectName]]' or 'listName$objectName' notation
  
  XORIG <- globalAttributes[["XORIG"]] # x origin
  YORIG <- globalAttributes[["YORIG"]] # y origin
  XCENT <- globalAttributes[["XCENT"]] # x center
  YCENT <- globalAttributes[["YCENT"]] # y center
  
  # Now we have enough information about the domain to figure out 
  # the n, e, s, w corners
  w <- XORIG
  e <- XORIG + 2 * abs(XCENT - XORIG)
  s <- YORIG
  n <- YORIG + 2 * (YCENT - YORIG)  
  
  # Knowing the grid dimensions and the true corners we can define legitimate 
  # lat/lon dimensions
  lat <- seq(s, n, length.out=length(row))
  lon <- seq(w, e, length.out=length(col))
  
  # Create time axis ----
  
  # Temporal information is stored in the 'TFLAG' variable
  tflag <- ncvar_get(old_nc, "TFLAG")
  
  # NOTE: 'TFLAG' is a matrix object with two rows, one containing the year and 
  # NOTE: julian day as YYYYDDD the other containing time in HHMMSS format. 
  # NOTE: We will paste matrix elements together with 'paste()'.
  # NOTE: The 'sprintf()' function is useful for C-style string formatting.
  # NOTE: Here we use it to add leading 0s to create a string that is six characters long.
  time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))
  
  # We use 'strptime()' to convert our character index to a "POSIXct" value.
  time <- strptime(x=time_str, format="%Y%j%H%M%S", tz="GMT")
  
  # Create new ncdf4 object ----
  
  # Get PM25 values
  # NOTE:  The degenerate 'LAY' dimension disppears so that 'pm25' is now 3D, not 4D. 
  pm25 <- ncvar_get(old_nc, "PM25") # dims=c(lon, lat, time)
  
  # Convert time to numeric value for storing purposes. By default R converts
  # POSIXct to seconds from 1970-01-01. tz must be specified or local will be
  # used. 
  numericTime <- as.numeric(time)
  
  # Define dimensions
  latDim <- ncdim_def("lat", "Degrees North", lat) 
  lonDim <- ncdim_def("lon", "Degrees East", lon)  
  timeDim <- ncdim_def("time", "seconds from 1970-1-1", numericTime)  
  
  # Define variables
  pm25Var <- ncvar_def(name="PM25", units="ug/m^3", 
                       dim=list(lonDim, latDim, timeDim), 
                       missval=-1e30)
  
  # Create a new netcdf file 
  fileName_v2 <- str_replace(fileName, ".nc", "_v2.nc")
  new_nc <- nc_create(fileName_v2, pm25Var)
  
  # Put data into the newly defined variable 
  ncvar_put(new_nc, pm25Var, pm25)
  
  # Close the file
  nc_close(new_nc)
  
  print("Created the new version of the smoke_dispersion.nc file.")
  return(time) # Handy for indexing values. 
}

# Now run this function on the file we just downloaded. It returns time array
# that can be used for slicing the gridded smoke data. 
time_nc <- bs2v2(fileName) 

# working with the raster brick of the nc file
nc_path <- paste0(home_path, "data/smoke_dispersion_v2.nc")

# get nc data as raster as class "RasterBrick"
smoke_brick <- brick(nc_path)

################################################################################
# Calculate daily average smoke concentrations 
################################################################################

# Change timezone to Denver, or MDT (in the smoke season)
time_GMT    <- as.POSIXct(as.character(time_nc), tz="GMT")
time_denver <- as.POSIXct(base::format(time_GMT, tz="America/Denver", usetz=TRUE))

# Get day and unique days that this forecast covers 
forecastDay <- lubridate::day(time_denver) # of month
unique_forecast_dates <- sort(unique(forecastDay))

# Figure out the first date we have a full 24 hour forecast for
todays_day_numeric <- as.numeric(format(Sys.Date(), "%d"))

# NOTE:
# smoke_brick typed to the console will reveal that the diemsnions are:
# "201, 481, 96681, 192  (nrow, ncol, ncell, nlayers)". Default rasterstack 
# behavaior is to perform math and index over nlayers, in this case that is time. 

# Create raster layer of same day mean value and take the mean of those (hourly)
# values for the selected date. 
t_index <- which(todays_day_numeric==forecastDay)
same_day_mean_smk <- mean(smoke_brick[[t_index]])

# extract the date without timestamp (taking element date 29 from 1:29)
same_day_date <- unique( format(time_denver[t_index], format = "%b %d %Y") )

# Take the mean of the next day smoke values 
t_index <- which((todays_day_numeric+1)==forecastDay)
next_day_mean_smk <- mean(smoke_brick[[t_index]])
next_day_date <- unique( format(time_denver[t_index], format = "%b %d %Y") )

# A third day


# TODO: Same more dates data and use the actual dates as the labels. 
# creating a vector of the character dates and saving to use in shiny labels
# note I think it's easier to save as a seperate file than label the layers of 
# the shape layers; I suspect less bugs with generic names in the shapefile than
# a changing date
date_labels <- c(same_day_date, next_day_date)

# saving character string of dates
save(date_labels, file = paste0(home_path,"/data/date_label.RData"))

# create raster brick and create spatial polygon ----
# make raster brick of same_day and next_day mean smoke
# TODO: update to include more days
smoke_stack <- brick(same_day_mean_smk, next_day_mean_smk)

# RG 2018-08-16: Comment to Steve; Bonne and Jeff were really only confident in 
# model skill/accuracy on the same day and next day.  

# RG 2018-08-16: Steve, moved this up from below to regrid smoke stack
population_grid <- data.table::fread(paste0(home_path,"data/2015-bluesky_grid_population.csv"))
# Old grid was on a east-west 468 to south_north 201 dimension grid
# Defining app grid extent based on min and max lat/lon values
app_extent <- extent(min(population_grid[,1]), max(population_grid[,1]),
                     min(population_grid[,2]), max(population_grid[,2]))

# Application coordinates
app_coords <- as.matrix(population_grid[,1:2])

# Creating empty raster based on dimension of application grid
app_r <- raster(app_extent, nrow=201, ncol=468)
# Applying wgs84 crs from smoke stack to empty raster
crs(app_r) <- crs(smoke_stack)

# Binlinear interpolation of bluesky smoke stack to match lat/lon coords of
# application grid
smoke_stack_app <- resample(smoke_stack, app_r, method = 'bilinear')


# Create pm matrix of same-day and next-day values. 
# This will be used later for population-weighting.
# TODO: add more forecast days of smoke here too.... 
# NOTE: pm_mat dimensions = [values(length of latxlon) X days forecast]
# RG 2018-08-16: Steve, changing this to raster to points and only take the last
# 2 elements which are the smoke same day and next day values; first 2 are lat/lon
pm_mat <- rasterToPoints(smoke_stack_app)[,3:4]

################################################################################
# convert smoke_stack to polygon/shape for displaying in the app and overlap
# calculations.
################################################################################
print("-----------------------------------------------------------------------")
print("converting smoke_stack to polygon....")
smk_poly <- raster::rasterToPolygons(smoke_stack_app)

# Subset smk_polygon to only those with values > PMThresh. Make two different
# files for the two forecast days. 
smk_forecast_1 <- smk_poly["layer.1"][smk_poly@data$layer.1 >= PMThresh,]
smk_forecast_2 <- smk_poly["layer.2"][smk_poly@data$layer.2 >= PMThresh,]

# # remove raster files to save memory
# rm(smoke_brick, 
#    same_day_mean_smk, 
#    next_day_mean_smk, 
#    smoke_stack)

# Write gridded smoke polygon for day 1
writeOGR(obj = smk_forecast_1, 
         dsn = paste0(home_path,"/data/smk_poly"), 
         layer = "smk_poly_1", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)

# Save as RData to be loaded by the app
save(smk_forecast_1, file=paste0(home_path, "/data/smk_poly/smk_forecast_1.RData"))

# Write gridded smoke polygon for day 2
writeOGR(obj = smk_forecast_2, 
         dsn = paste0(home_path,"/data/smk_poly"), 
         layer = "smk_poly_2", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)

# Save as RData to be loaded by the app
save(smk_forecast_2, file=paste0(home_path,"/data/smk_poly/smk_forecast_2.RData"))

# remove smk poly to save space
rm(smk_poly)
####### RETURN HERE
################################################################################
# Calculate population-weighted county smoke pm2.5 values 
################################################################################
# Read in proportion-intersect matrix between grid and county shapes
# NOTE: attempting to view this table in RStudio will kill RStudio. 
# NOTE: file created by proportion_intersect_bluesky_grid_us_counties.R 
grid_county_pi <- data.table::fread(paste0(home_path,"/data/bluesky_county_prop_intersect.csv"))

# Get dimensions for subset indexing
# GCP = short for "grid_county_pi", second number is number of US counties.
# This number should not change. 
GCP_dim <- dim(grid_county_pi) 

# Convert "grid_county_pi" to matrix
# NOTE: names(grid_county_pi) = "grid_id"    "fips_19107" "fips_19189" ...
# NOTE: For multiplication, we do not want "grid_id", just actual values 
# NOTE: associated with different counties (columns other than first). 
pi_mat <- as.matrix(grid_county_pi[, 2:GCP_dim[2]])

# Now that it is available as matrix, remove grid_county_pi to save space
rm(grid_county_pi)

# Get the population density value vector. 
# This csv was created by bluesky_grid_population_vector.R 
# RG 2018-08-16: Steve, I'm moving this up to grab the lat/lons of this grid
# so I can regrid the new bluesky grid to our older grid; I also modified a couple
# lines of code in the bluesky_grid_population_vector to keep the lat/lons
# population_grid <- data.table::fread("./data/2015-bluesky_grid_population.csv")

# Get vector of population density out of the population grid used above to 
# regrid the new bluesky grid to our old grid
popden <- population_grid$popden 

# Multiply population vector by pm vector. These share a common dimension of
# county? 
# NOTE: This is the line that has been killing the app lately. 
# Dimensions: popden[? X ?] * pm_mat[bluesky grid index X forecast day]
print(paste("Dimensions of pm_mat:", dim(pm_mat)))
print(paste("Dimensions of popden:", length(popden)))

pm_pop_mat <- popden * pm_mat

# matrix multiply prop int matrix by population vector for daily summed pm
county_grid_pm_mat <- t(pi_mat) %*% pm_pop_mat

# matrix multiply prop int matrix by popden vector for popden per county
popden_county <- t(pi_mat) %*% popden

# calculate the inverse of popden_county matrix
popden_county_inverse <- 1/popden_county

# multiply county_grid_pm_mat by inverse population vector to estimate 
# county population-weighted estimate 
county_pop_wt_smk <- county_grid_pm_mat * as.vector(popden_county_inverse)

# save as dataframe
pm_county_wt <- as.data.frame(county_pop_wt_smk)
# name variables
colnames(pm_county_wt) <- c("same_day_pm", "next_day_pm")
# create FIPS variable
pm_county_wt$FIPS <- as.character(str_sub(rownames(pm_county_wt), start=6L))

# remove matrices to save space
rm(county_grid_pm_mat, county_pop_wt_smk, pi_mat, pm_mat, popden,
   pm_pop_mat, popden_county, popden_county_inverse, population_grid)

# Calculate health impact of given smoke concentration ------------------------- 

# read county populations
county_pop <- data.table::fread(paste0("./data/us_census_county_population/",
  "PEP_2015_PEPANNRES_with_ann.csv"))[-1, c(2,11)]

# assign names: FIPS and pop_2015
colnames(county_pop) <- c("FIPS", "pop_2015")
# assign pop_2015 as numeric
county_pop$pop_2015 <- as.numeric(county_pop$pop_2015)
# subset counties in smoke values
county_pop <- county_pop[county_pop$FIPS %in% pm_county_wt$FIPS, ]

# merge population with smk pm values
hia_est <- merge(county_pop, pm_county_wt, by = "FIPS")
# add in base_rate; this will be changed but i need to calculate this
hia_est$base_resp_rate <- 1.285/10000 
# add beta based on our work
hia_est$resp_beta <- log(1.052)
# calculate expected respiratory ED visits
hia_est$same_day_resp_ed <- round((hia_est$base_resp_rate * 
  (1-exp(-(hia_est$resp_beta) * hia_est$same_day_pm)) * hia_est$pop_2015),0)
# next day
hia_est$next_day_resp_ed <- round((hia_est$base_resp_rate * 
  (1-exp(-(hia_est$resp_beta) * hia_est$next_day_pm)) * hia_est$pop_2015),0)

# Notes on HIA: 2017-12-29
# need to rename hia_estimate column names to avoid truncation when saving polygon
# considering a monte-carlo; not sure it's worth it now

# Create hia shapefile for smoke_forecaster app --------------------------------

# read in shapefile
# county path
poly_path <- "./data/us_county"
poly_layer <- "us_county"

# read county polygon
us_shape <- readOGR(dsn = poly_path, layer = poly_layer)
# add fips variable to join
us_shape$FIPS <- us_shape$GEOID

# join popwt pm and hia estimates to shapefile
us_shape <- sp::merge(us_shape, hia_est, by = "FIPS")

# subset to counties with hia estimates of at least 1
us_shape <- us_shape[(us_shape$same_day_pm >= PMThresh & us_shape$same_day_resp_ed > 1) | 
                     (us_shape$next_day_pm >= PMThresh & us_shape$next_day_resp_ed > 1), ]

# rename truncated variable names; renamed hia estimates to layer_1 and layer_2
# to match gridded bluesky forecasts of smoke labels
c_names <- colnames(us_shape@data)
c_names[11:17] <- c("Pop", "Day1Pm", "Day2Pm", "RespRt", 
                    "RespB", "layer_1", "layer_2")

colnames(us_shape@data) <- c_names

# save shape with hia estimates
writeOGR(obj = us_shape, dsn = paste0(home_path,"/data/hia_poly"), 
         layer = "hia_poly", driver = "ESRI Shapefile", overwrite_layer = T)

print("Script executed without error.")
