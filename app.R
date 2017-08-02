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

# setting working directory to salix server
wd_dir <- paste0("/srv/www/rgan/smoke_forecaster/")
setwd(wd_dir)

# read in smoke forecast shapefile ----
# define relative path to polygon file
poly_path <- "./data/smk_poly"
poly_layer <- "smk_poly"

# read bluesky forecast polygon
smk_forecast <- readOGR(dsn = poly_path, layer = poly_layer)

# set upper bound to 250
smk_forecast[smk_forecast$layer_1 >= 250, ] <- 249
smk_forecast[smk_forecast$layer_2 >= 250, ] <- 249

# default leaflet projection
# commented out for now
#grs80 <- paste0("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

#test <- spTransform(smk_forecast, CRS(grs80))  
#test

# define color bin for layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(0, 10, 20, 30, 40, 50, 100, 250)
pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(0,250), bins = bin,
                  na.color = "transparent")

# add another legend for relative risk resp
resp_bin <- round(exp((bin/10)*0.0507),2)
# resp pal
resp_pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(1,max(resp_bin)), 
                     bins = resp_bin, na.color = "transparent")
# asthma
asthma_bin <- round(exp((bin/10)*0.0733),2)
# asthma pal 
asthma_pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(1,max(asthma_bin)), 
                     bins = asthma_bin, na.color = "transparent")

# read in saved R dates ----
load("./data/date_label.RData")

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# read in fire locations ----
fire_locations <- read.csv("./data/fire_locations.csv")
# type indicates either wildfire (WF) or prescription burn (RX)
# set color of WF to red and RX to green
pal_fire <- colorFactor(
  palette = c("red", "green"),
  levels = c("WF", "RX")
  )

# read county polygon (commenting out now; slows down app)
# us_poly_path <- "./data/us_county"
# us_poly_layer <- "us_county"
# 
# # read us polygon
# us_county <- readOGR(dsn = us_poly_path, layer = us_poly_layer)

# shiny dash board ui ----
# note: 7/14/2017: I like the dashboard layout, but it may be better to define
# the three elemnts of the dashboard outside the ui.
# header
head <- dashboardHeader(title = "Beta Smoke ForecasteR",
  titleWidth = 450)
# side bar
side <- dashboardSidebar(
  radioButtons("date_smoke", label = h2("Date to Forecast"), 
               choices = date_list, selected = "layer_1")
  ) # end side bar

# body
body <- dashboardBody(
  # set tag style
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")

)# end dashboard body
  
  
# ui function  
ui <- dashboardPage(head,side,body, skin = "black") # dashboard header, side, and body

# server section ---- 
# consider adding a session function if I want to know statistics
server <- (function(input, output){
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-123.925,lng2=-74.425, lat1=48.225, lat2=33.975) %>% 
      # add fire locaiton icons
      addCircleMarkers(data = fire_locations, lat = fire_locations$latitude, 
        lng = fire_locations$longitude, color = ~pal_fire(type),
        radius = ~sqrt(area/100), fill=F, weight = 0.5) %>% 
      # add legend for smoke values
      addLegend(pal=pal, values=c(0, 250), 
        title = htmltools::HTML("Smoke <span>&#181;</span>g/m<sup>3</sup>"),
                position = "bottomleft") %>% 
      # add respiratory legend
      addLegend(pal = asthma_pal, values= c(min(asthma_bin), max(asthma_bin)),
                title = htmltools::HTML("Asthma <br> Relative Risk"),
                position = "bottomright") %>% 
      # add respiratory legend
      addLegend(pal = resp_pal, values= c(min(resp_bin), max(resp_bin)),
        title = htmltools::HTML("Respiratory <br> Relative Risk"),
        position = "bottomright") 
      # trying layer control (don't have a use for it now)
      # addLayersControl(overlayGroups = "Smoke", #baseGroups = c("Base Map",
      #   #"Blue Sky Extent", "Fire Locations", "Legend"),
      #   options = layersControlOptions(collapsed = F))
    
      # add county shapefile (all counties slow down app a lot)
      #addPolygons(data = us_county, weight = 1, smoothFactor = 5)

  })# end base leaflet
  
  # add interactive raster layer
  observeEvent(input$date_smoke,{
  # set index as 1 or 2 for easier index
  layer_name <- as.character(input$date_smoke)
  # define reactive label values of smoke concentrations and relative risks
  vals <- reactive({getElement(smk_forecast@data, layer_name)})
  # Smoke Concentration: value ug/m^3 \return
  # Relative Increase in Risk: value %
  pm_label <- sprintf(paste0(
    "<strong>Smoke Concentration: %g <span>&#181;</span>g/m<sup>3</sup></strong>",
    "<br> Respiratory Relative Risk: %g",
    "<br> Asthma Relative Risk: %g"), 
    # number for smoke concentration
    round(vals(),1),
    # number for relative risk respiratory
    round(exp(vals()/10*0.0507),2),
    # number for relative risk asthma
    round(exp(vals()/10*0.0733),2)) %>% 
    lapply(htmltools::HTML)
  
  # call proxy map
  leafletProxy(mapId="map") %>%
    clearShapes() %>% 
    # set a box that defines the dimensions of bluesky forecast
    addRectangles(lng1=-130.0,lng2= -59.95, lat1=22.5, lat2=52.5,
                  fillColor = "transparent", color = "black", weight = 2) %>%
    # add smoke polygon 
      addPolygons(data = smk_forecast, color = "tranparent", 
        fillColor = ~pal(vals()), weight = 1, smoothFactor = 1, fillOpacity = 0.5, 
        # add highlight option
        highlight = highlightOptions(weight = 5, color = "blue", 
          bringToFront = T, fillOpacity = 0.85),
        # add smoke pm values
        label = pm_label,
        labelOptions = labelOptions(style = list("font-weight" = "normal", 
          padding = "3px 8px"), textsize = "12px", direction = "auto") 
      )
  }) # end reactive layer
  
}) # end server function

# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


