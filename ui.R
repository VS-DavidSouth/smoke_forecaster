# ------------------------------------------------------------------------------
# Title: UI script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.3.3 
# ------------------------------------------------------------------------------

# load libraries ----
library(shiny)
library(leaflet)
library(raster)
library(ncdf4)

# setup ------------------------------------------------------------------------
# not sure if setup needs to be both in UI and server or if it's saved
# in local enviroment
# read net cdf file of forecasts from 06/08/2017 ----

# define direct path to file
nc_path <- "/srv/shiny-server/smoke_forecaster/smoke_dispersion_v2.nc"
# brick or stack works
smk_brick1 <- brick(nc_path)

# testing to see if it's a size of raster brick issue
smk_brick <- smk_brick1[[1:10]]


# set upper bound to 160 and anything lower to NA for nicer raster presentation
smk_brick <- calc(smk_brick, fun=function(x){
  x[x > 160] <- 160;
  x[x < 5] <- NA; return(x)
})

rast <- smk_brick[[1]]

# find the max range
max_pm <- max(summary(smk_brick)[5,])

# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,160),
                    na.color = "transparent")

# convert character names to numeric
date_time <- as.numeric(substring(smk_brick@data@names, 2))
# now assign date time stamp
date_time <- as.POSIXct(date_time, origin="1970-1-1", tz="GMT")
# minimum date
min_date <- min(date_time)
max_date <- max(date_time)

# set up shiny layout
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # initialize map
  leafletOutput("map", width = "100%", height="100%"),
  # add slider
  absolutePanel(top = 10, right = 20,
    sliderInput(inputId = "time", label = "Date & Time", min = min_date, 
      max = max_date, value = min_date, step = 3600, 
      timeFormat = "%F %T", timezone = "GMT", 
      animate = animationOptions(interval=200, loop=T))
    )) #end boot page
) # end UI function



