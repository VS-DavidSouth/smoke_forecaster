# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.3.3 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

# load libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(raster)
library(ncdf4)

# define direct path to file
#nc_path <- "/srv/shiny-server/smoke_forecaster/smk_forecast_raster.nc"
nc_path <- "smk_stack_raster.nc"

# using single raster layer of next day average
smk_forecast <- raster(nc_path)

# set upper bound to 160 and anything lower to NA for nicer raster presentation
 smk_forecast <- calc(smk_forecast, fun=function(x){
   x[x >= 200] <- 199;
   x[x < 5] <- NA; 
   return(x)
 })


# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,200),
                    na.color = "transparent")

# identify today's date
todays_date <- format(as.Date(Sys.Date()), "%B %d, %Y")
# identify tomorrows date
date_tomorrow <- format(as.Date(Sys.Date()+1), "%B %d, %Y")
 
# server section that will eventually go in it's own script
server <- (function(input, output){
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
      addRasterImage(smk_forecast, colors = pal, opacity = 0.7, project = T) %>% 
      addLegend(pal=pal, values=c(0, 200), title = "Smoke ug/m^3",
                position = "bottomright")
    
  }) #

}) # end server function

# set up shiny layout
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # add title
  titlePanel("Forecasted Wildfire Smoke"),
  # initialize map
  leafletOutput("map", width = "100%", height="100%"),
  # adding a radio button for today's or tomorrow's forecast
  absolutePanel(top = 75, right = 25,
                radioButtons("date_smoke", label = "Date of Smoke", 
                              c(todays_date=1, date_tomorrow=2),
                              selected = 1))
) # end UI function


shinyApp(ui = ui, server = server)
