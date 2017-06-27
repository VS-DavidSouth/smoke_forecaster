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
library(ncdf4)

# define direct path to file
nc_path <- "/srv/shiny-server/smoke_forecaster/smoke_dispersion_v2.nc"
# brick or stack works
smk_brick1 <- brick(nc_path)

# testing to see if it's a size of raster brick issue
smk_brick <- smk_brick1[[1:10]]

rm(smk_brick1)

# set upper bound to 160 and anything lower to NA for nicer raster presentation
smk_brick <- calc(smk_brick, fun=function(x){
  x[x > 160] <- 160;
  x[x < 5] <- NA; return(x)
})


# find the max range
max_pm <- max(summary(smk_brick)[5,])

# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,160),
                    na.color = "transparent")

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
  #observeEvent(input$time,{
    
    # numeric index for subsetting smoke brick
  #  index <- ((as.numeric(input$time) - as.numeric(min_date))/3600)+1
    
    # reactive raster layer
  #  r <- reactive({smk_brick[[index]]})
    
    # call proxy map
  #  leafletProxy(mapId="map") %>%
  #    clearImages() %>%
  #    addRasterImage(r(), colors = pal, opacity = 0.7, project = T) 

 # })
})
