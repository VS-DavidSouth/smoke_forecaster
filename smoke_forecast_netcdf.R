# ------------------------------------------------------------------------------
# Title: Exploring Net CDF file
# Author: Ryan Gan
# Date Created: 6/8/2017
# Created under R Version: 3.3.3
# ------------------------------------------------------------------------------

# net cdf library
library(ncdf4)
library(raster)

?`ncdf4-package`

# read net cdf file of forecasts from 06/08/2017
smoke_forecast <- nc_open("./smoke_dispersion.nc")

# print info about file
print(smoke_forecast)

names(smoke_forecast$var)
# multi dimension of smoke PM2.5 value for each hour from 1-192 hours

# can read the netcdf file right in to a raster brick
smk_brick <- brick("./smoke_dispersion.nc")
# plot one of the smoky days
plot(smk_brick[[170]])





summary(smk_rast@data$PM25)

