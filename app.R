# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

# load libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(raster)
#library(ncdf4)

# define direct path to file
#nc_path <- "/srv/shiny-server/smoke_forecaster/smk_stack_raster.nc"
 nc_path <- "smk_stack_raster.nc"

# using single raster layer of next day average
# note july 3rd 2017: brick is causing shiny app to not run
# error message: ERROR: "R_nc4_open" not available for .C() for package "ncdf4"
# perhaps it doesn't like; also asked tyson to install gdal on server which may help
smk_forecast <- brick(nc_path)


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
 
# server section ----
server <- (function(input, output){
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
      addLegend(pal=pal, values=c(0, 200), title = "Smoke ug/m^3",
                position = "bottomright")
  })# end base leaflet
  
  # add interactive raster layer
  observeEvent(input$date_smoke,{
   # reactive raster layer
    index <- as.numeric(input$date_smoke)
    
    r <- reactive({smk_forecast[[index]]})
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      clearImages() %>%
      addRasterImage(r(), colors = pal, opacity = 0.8, project = T) 

  }) # end reactive layer
    
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
                radioButtons("date_smoke", label = h3("Date of Smoke"), 
                              choices = list("Today"=1, "Tomorrow"=2),
                              selected = 1), hr(), 
                              fluidRow(column(2,verbatimTextOutput("value"))))
  # note for the radio button; having trouble listing date based on system time
  ) # end UI function

# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)

