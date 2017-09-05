# ------------------------------------------------------------------------------
# Title: Daily BlueSky forecast download and data management
# Author: Ryan Gan
# Date Created: 6/19/2017
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------

# Note: Code directly from mazamascience to download their bluesky runs
# http://mazamascience.com/Classes/PWFSL_2014/Lesson_07_BlueSky_FirstSteps.
# html#downloadbsoutput\

# libraries needed
library(ncdf4)
library(stringr)
library(raster) # easier to manipulate than netcdf file
library(rgdal)

# set up working directory
setwd("/srv/www/rgan/smoke_forecaster")
# define path to repository for the server for writing files
home_path <- paste0("/srv/www/rgan/smoke_forecaster")

# download bluesky daily output -----------------------------------------------

# date is needed for download; taking out "-" separator; adding 00 to get first
# run of the day (just in case there are two)
todays_date <- paste0(gsub("-","", Sys.Date()), "00")

# download fire locations from bluesky runs ----
# note right now I download only the location file, but I may work in
# fire information in the future. looks like it's contained in the json file
# Note: changed "forecast" to "combined" estimate on Sept 5 2017
fire_url_path <- paste0("https://smoke.airfire.org/bluesky-daily/output/standard/",
  "GFS-0.15deg/", todays_date, "/combined/data/fire_locations.csv")

download.file(url = fire_url_path, destfile = paste0(home_path,
  "/data/fire_locations.csv"), mode = "wb")

# download smoke dispersion output ----
# define URL path for smoke dispersion
url_path <- paste0("https://smoke.airfire.org/bluesky-daily/output/standard/",
  "GFS-0.15deg/", todays_date, "/combined/data/smoke_dispersion.nc")

# download a netcdf file to work with
download.file(url = url_path, destfile = paste0(home_path,
                "/data/smoke_dispersion.nc"), mode = "wb")

fileName <- paste0(home_path,"/data/smoke_dispersion.nc")

# netcdf file manipulaton ------------------------------------------------------
nc <- nc_open(fileName)

bs2v2 <- function(fileName) {

  # open nc file
  old_nc <- nc_open(fileName)
  
  # ----- Create latitude and longitude axes -----------------------------------

  # Current (index) values
  row <- old_nc$dim$ROW$vals
  col <- old_nc$dim$COL$vals
  
  # Useful information is found in the global attributes
  globalAttributes <- ncatt_get(old_nc, varid=0) # varid=0 means 'global'
  
  # Use names(globalAttributes) to see the names of the elements contained in this list
  
  # NOTE:  globalAttributes is of class 'list'
  # NOTE:  Access list elements with either 'listName[[objectName]]' or 'listName$objectName' notation
  
  XORIG <- globalAttributes[["XORIG"]] # x origin
  YORIG <- globalAttributes[["YORIG"]] # y origin
  XCENT <- globalAttributes[["XCENT"]] # x center
  YCENT <- globalAttributes[["YCENT"]] # y center
  
  # Now we have enough information about the domain to figure out the n, e, s, w corners
  w <- XORIG
  e <- XORIG + 2 * abs(XCENT - XORIG)
  s <- YORIG
  n <- YORIG + 2 * (YCENT - YORIG)  
  
  # Knowing the grid dimensions and the true corners we can define legitimate lat/lon dimensions
  lat <- seq(s, n, length.out=length(row))
  lon <- seq(w, e, length.out=length(col))
  
  # ----- Create time axis -----------------------------------------------------
  
  # Temporal information is stored in the 'TFLAG' variable
  tflag <- ncvar_get(old_nc, "TFLAG")
  
  # NOTE:  'TFLAG' is a matrix object with two rows, one containing the year and Julian day, 
  # NOTE:  the other containing time in HHMMSS format. We will paste matrix elements together
  # NOTE:  with 'paste()'.  The 'sprintf()' function is useful for C-style string formatting.
  # NOTE:  Here we use it to add leading 0s to create a string that is six characters long.
  time_str <- paste0(tflag[1,], sprintf(fmt="%06d", tflag[2,]))
  
  # We use 'strptime()' to convert our character index to a "POSIXct" value.
  time <- strptime(x=time_str, format="%Y%j%H%M%S", tz="GMT")
  
  # ----- Create new ncdf4 object ----------------------------------------------
  
  # Get PM25 values
  # NOTE:  The degenerate 'LAY' dimension disppears so that 'pm25' is now 3D, not 4D. 
  pm25 <- ncvar_get(old_nc, "PM25")
  
  # Convert time to numeric value for storing purposes
  numericTime <- as.numeric(time)
  
  # Define dimensions
  latDim <- ncdim_def("lat", "Degrees North", lat) 
  lonDim <- ncdim_def("lon", "Degrees East", lon)  
  timeDim <- ncdim_def("time", "seconds from 1970-1-1", numericTime)  
  
  # Define variables
  pm25Var <- ncvar_def(name="PM25", units="ug/m^3", 
                       dim=list(lonDim, latDim, timeDim), missval=-1e30)
  
  # Create a new netcdf file 
  fileName_v2 <- str_replace(fileName, ".nc", "_v2.nc")
  new_nc <- nc_create(fileName_v2, pm25Var)
  
  # Put data into the newly defined variable 
  ncvar_put(new_nc, pm25Var, pm25)
  
  # Close the file
  nc_close(new_nc)
  
}

# close original nc connection
nc_close(nc)
rm(nc)

# I'm beginning to think it might just be easier to work with a raster layer
# rather than ncdf
# Now run this function on the file we just downloaded
bs2v2(fileName)
list.files(pattern='*.nc')

# working with the raster brick of the nc file
nc_path <- paste0(home_path, "/data/smoke_dispersion_v2.nc")
# brick or stack 
smk_brick <- brick(nc_path)

#test_grid <-SpatialPixels(SpatialPoints(smk_brick))

# calculate same day daily average ----
# create raster layer of same day mean value
same_day_smk <- smk_brick[[1:29]]
# create raster layer of mean value
same_day_mean_smk <- mean(same_day_smk)
# extract the date without timestamp (taking element date 15 frome 1:29)
same_day_date  <- as.numeric(substring(smk_brick@data@names, 2))[15]
# assign date time stamp in a format of month_day_year to bind with name
same_day_date <- format(as.POSIXct(same_day_date, origin="1970-1-1", tz="GMT"),
                    format = "%b %d %Y")

# calculate next day daily average -----
# subset raster brick to the 30th to 54th layer (next day MST)
next_day_smk <- smk_brick[[30:54]]
# create raster layer of daily mean value
next_day_mean_smk <- mean(next_day_smk)
# extract next day's date
next_day_date  <- as.numeric(substring(smk_brick@data@names, 2))[45]
# assign date time stamp in a format of month_day_year to bind with name
next_day_date <- format(as.POSIXct(next_day_date, origin="1970-1-1", tz="GMT"),
                        format = "%b %d %Y")

# creating a vector of the character dates and saving to use in shiny labels
# note I think it's easier to save as a seperate file than label the layers of 
# the shape layers; I suspect less bugs with generic names in the shapefile than
# a changing date
date_labels <- c(same_day_date, next_day_date)
# saving character string of dates
save(date_labels, file = paste0(home_path,"/data/date_label.RData"))

# create raster brick and create spatial polygon ----
# make raster brick of same_day and next_day mean smoke
smoke_stack <- brick(same_day_mean_smk,next_day_mean_smk)

# convert smoke_stack to polygon/shape
smk_poly <- rasterToPolygons(smoke_stack)

# saving bluesky grid shapefile ----
# this will be commented out once it's done
# #subsetting just the grid so I can calculate spatial overlays
# smk_grid <- smk_poly[, 1]
# # write smoke grid that doesn't have values
# writeOGR(obj = smk_grid, dsn = "./data/bluesky_grid", layer = "bluesky_grid",
#          driver = "ESRI Shapefile")

# subsetting smk_polygon to only those with values > 5 
# to make polygon file smaller and easier to project
smk_poly <- smk_poly[smk_poly$layer.1 > 5 | smk_poly$layer.2 > 5, ]

# remove raster files to save space
rm(smk_brick, same_day_smk, same_day_mean_smk, next_day_smk,
     next_day_mean_smk, smoke_stack)

# write smoke polygon ----
writeOGR(obj = smk_poly, dsn = paste0(home_path,"/data/smk_poly"), 
         layer = "smk_poly", driver = "ESRI Shapefile", overwrite_layer = T)


