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
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal) # read shapefile

# define relative path to polygon file

poly_path <- "./data/smk_poly"
poly_layer <- "smk_poly"

# read in smoke forecast shapefile ----
smk_forecast <- readOGR(dsn = poly_path, layer = poly_layer)

# set upper bound to 200
smk_forecast[smk_forecast$layer_1 >= 250, ] <- 249
smk_forecast[smk_forecast$layer_2 >= 250, ] <- 249

# define color bin for layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(0, 5, 10, 25, 50, 100, 250)
pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(0,250), bins = bin,
                    na.color = "transparent")

# read in saved R dates ----
load("./data/date_label.RData")

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# shiny dash board ui ----
# note: 7/14/2017: I like the dashboard layout, but it may be better to define
# the three elemnts of the dashboard outside the ui.

# uses the shinydashboard package for dashboard layout

ui <- dashboardPage(
  # dashboard header
  dashboardHeader(title = "Beta Smoke ForecasteR"),

  # dashboard side bar neads to be in function
  dashboardSidebar(disable = T),
  # dashboard body
  dashboardBody(
    # set up two rows
    fluidRow(
    # lefthand side radio button
    # adding a radio button for today's or tomorrow's forecast
      column(width = 3, 
        box(width = NULL, status = "success",
          radioButtons("date_smoke", label = h2("Date to Forecast"), 
            choices = date_list, selected = "layer_1"), hr()
               ), # end box
        # message
        p(class = "text-muted", paste("Smoke forecast uses BlueSky output",
          "and estimates daily average smoke concentrations for today and tomorrow.",
          "Since this is an early stage beta, the layout is crude and subject to changing",
          "There are still a lot of features planned including: a health component",
          "integration with daily AQS values, location of fires, etc."))
           ),
      column(width = 9, 
        # initialize map
        box(leafletOutput("map", width = "700", height="500"))
            )
      )# end fluid row
    )# end dashboard body
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
      # set a box that defines the dimensions of bluesky forecast
      addRectangles(lng1=-125, lat1=50, lng2=-67, lat2=25,
        fillColor = "transparent", color = "blue") %>%
      addLegend(pal=pal, values=c(0, 250), title = "Smoke ug/m^3",
                position = "bottomleft") 
    
  })# end base leaflet
  
  # add interactive raster layer
  observeEvent(input$date_smoke,{
   # set index as 1 or 2 for easier index
    layer_name <- as.character(input$date_smoke)
  # define smoke label values
  pm_label <- sprintf("%g ug/m^3 of smoke", 
    round(getElement(smk_forecast@data, layer_name), 1)) %>% 
    lapply(htmltools::HTML)
    
   # reactive polygon layer
    vals <- reactive({getElement(smk_forecast@data, layer_name)})
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      # add polygon 
      addPolygons(data = smk_forecast, 
        color = "tranparent", 
        fillColor = ~pal(vals()), 
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


