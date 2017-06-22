# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.3.3 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function.

# load libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(raster)
library(ncdf4)

# define direct path to file
#nc_path <- "/srv/shiny-server/smoke_forecaster/smoke_dispersion_v2.nc"
nc_path <- "smoke_dispersion_v2.nc"
# brick or stack works
smk_brick <- brick(nc_path)

# testing to see if it's a size of raster brick issue
#smk_brick <- smk_brick1[[1:10]]

#rm(smk_brick1)

# set upper bound to 160 and anything lower to NA for nicer raster presentation
smk_brick <- calc(smk_brick, fun=function(x){
  x[x > 160] <- 160;
  x[x < 5] <- NA; return(x)
})


# find the max range
max_pm <- max(summary(smk_brick)[5,])

# convert character names to numeric
date_time <- as.numeric(substring(smk_brick@data@names, 2))
# now assign date time stamp
date_time <- as.POSIXct(date_time, origin="1970-1-1", tz="GMT")
# minimum date
min_date <- min(date_time)
max_date <- max(date_time)

# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,160),
                    na.color = "transparent")

# server section that will eventually go in it's own script
server <- (function(input, output){
  
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
}) # end server function

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
  )
) # end UI function

shinyApp(ui = ui, server = server)
