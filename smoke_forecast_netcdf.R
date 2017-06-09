# ------------------------------------------------------------------------------
# Title: Exploring Net CDF file
# Author: Ryan Gan
# Date Created: 6/8/2017
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------

# net cdf library
library(ncdf4)
library(stringr)

# read net cdf file of forecasts from 06/08/2017
smoke_forecast <- nc_open("./smoke_dispersion.nc")

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
  pm25Var <- ncvar_def(name="PM25", units="ug/m^3", dim=list(lonDim, latDim, timeDim), missval=-1e30)
  
  # Create a new netcdf file 
  fileName_v2 <- str_replace(fileName, ".nc", "_v2.nc")
  new_nc <- nc_create(fileName_v2, pm25Var)

  # Put data into the newly defined variable 
  ncvar_put(new_nc, pm25Var, pm25)
  
  # Close the file
  nc_close(new_nc)
  
}

# Now run this function on the file we just downloaded
bs2v2("./smoke_dispersion.nc")
list.files(pattern='*.nc')
nc_close("./smoke_dispersion.nc")

smoke_forecast <- nc_open("./smoke_dispersion_v2.nc")

print(smoke_forecast)

# read in data with improved file
pm25 <- ncvar_get(smoke_forecast, "PM25")
time <- ncvar_get(smoke_forecast, "time")
time <- as.POSIXct(time, origin="1970-1-1", tz="GMT")
lat <- ncvar_get(smoke_forecast, "lat")
lon <- ncvar_get(smoke_forecast, "lon")

# close the file now that everything is in memory
nc_close(smoke_forecast)
