# ------------------------------------------------------------------------------
# Title: Server script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.3.3 
# ------------------------------------------------------------------------------

# load libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(raster)

# server section that will eventually go in it's own script
shinyServer(function(input, output, session){
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
      addLegend(pal=pal, values=c(0,max_pm), title = "Smoke ug/m^3",
                position = "bottomright")
    
  }) #
  
  # add interactive raster layer
  observeEvent(input$time,{
    
    # numeric index for subsetting smoke brick
    index <- ((as.numeric(input$time) - as.numeric(min_date))/3600)+1
    
    # reactive raster layer
    r <- reactive({smk_brick[[index]]})
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      clearImages() %>%
      addRasterImage(r(), colors = pal, opacity = 0.7, project = T) 

  })
})
