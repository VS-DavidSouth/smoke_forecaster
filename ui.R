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



# set up shiny layout
ui <- bootstrapPage(
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




