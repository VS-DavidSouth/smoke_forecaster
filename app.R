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
library(rgdal) # read shapefile

# define direct path to polygon file
#poly_path <- "/srv/shiny-server/smoke_forecaster/smk_stack_raster.nc"
poly_path <- "smk_poly"

# read in shapefile
smk_forecast <- readOGR(dsn = poly_path, layer = poly_path)

# set upper bound to 200
smk_forecast[smk_forecast$layer_1 >= 250, ] <- 249
smk_forecast[smk_forecast$layer_2 >= 250, ] <- 249

# define color bin for layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(0, 5, 10, 25, 50, 100, 250)
pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(0,250), bins = bin,
                    na.color = "transparent")

# identify today's date
todays_date <- format(as.Date(Sys.Date()), "%B %d, %Y")
# identify tomorrows date
date_tomorrow <- format(as.Date(Sys.Date()+1), "%B %d, %Y")

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
                             choices = list("Today"="layer_1", 
                                            "Tomorrow"="layer_2"),
                             selected = "layer_1"), hr(), 
                fluidRow(column(2,verbatimTextOutput("value"))))
  # note for the radio button; having trouble listing date based on system time
) # end UI function

# server section ----
server <- (function(input, output){
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
      addLegend(pal=pal, values=c(0, 250), title = "Smoke ug/m^3",
                position = "bottomright") %>% 
      addPolygons(data = smk_forecast, 
                  color = "tranparent", 
                  fillColor = ~pal(smk_forecast$layer_2), 
                  weight = 1, smoothFactor = 2,
                  fillOpacity = 0.5,
                  # add highlight option
                  highlight = highlightOptions(
                    weight = 5, color = "blue", bringToFront = T, fillOpacity = 0.8),
                  # add smoke pm values
                  label = pm_label
      )
    
  })# end base leaflet
  
  # add interactive raster layer
  observeEvent(input$date_smoke,{
   # set index as 1 or 2 for easier index
    poly_name <- as.numeric(input$date_smoke)
   # reactive polygon layer
    poly <- reactive({smk_forecast[[index]]})
    
    # define smoke label values
    pm_label <- sprintf("%g ug/m^3 of smoke", round(smk_forecast$layer_2, 1)) %>% 
      lapply(htmltools::HTML)
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      #removeShape() %>%
      # add polygon 
      addPolygons(data = poly(), # data = smk_forecast, 
        color = "tranparent", 
        #fillColor = ~pal(smk_forecast$layer_2), 
        weight = 1, smoothFactor = 2,
        fillOpacity = 0.5,
        # add highlight option
        highlight = highlightOptions(
          weight = 5, color = "blue", bringToFront = T, fillOpacity = 0.8),
        # add smoke pm values
        label = pm_label
      )
  }) # end reactive layer
    
}) # end server function


# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


