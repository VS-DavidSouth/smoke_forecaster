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

# download bluesky daily output -----------------------------------------------

# date is needed for download; taking out "-" separator; adding 00 to get first
# run of the day (just in case there are two)
todays_date <- paste0(gsub("-","", Sys.Date()), "00")

# define URL path
url_path <- paste0("https://smoke.airfire.org/bluesky-daily/output/standard/",
  "GFS-0.15deg/", todays_date, "/forecast/data/smoke_dispersion.nc")

# Download a netcdf file to work with
download.file(url = url_path, destfile = "smoke_dispersion.nc", mode = "wb")

fileName <- "smoke_dispersion.nc"

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

nc

# Now run this function on the file we just downloaded
bs2v2(fileName)
list.files(pattern='*.nc')

test_ncdf <- nc_open("smoke_dispersion_v2.nc")
test_ncdf$dim$time


# sets date time to moutain standard
date_time <- format(as.POSIXct(ncvar_get(test_ncdf, "time"), 
                               origin="1970-1-1", tz="GMT"),
                    tz = "America/Denver")
date_time
# try to subset to next day
min_date <- min(date_time)

min_date

date_time2 <- date_time[30:54]

# test subset by date_time2
next_day_nc <- subset(ncvar_get(test_ncdf, time)[30:54])
