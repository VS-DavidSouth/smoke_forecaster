# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan 
# Date Created: 2017-06-17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

polyOpacity <- 0.7
polyBorderOpacity <- 0.85

fireIcons <- icons(
  iconUrl = "http://thediscipleproject.net/wp-content/uploads/2013/07/fire-vector.png",
  iconWidth = 17, 
  iconHeight = 17
)

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal) # for read shapefile


# Set paths and load required data ---------------------------------------------

# read in smoke forecast shapefile ----
# define relative path to polygon file
poly_path <- "./data/smk_poly"
poly_layer <- "smk_poly"

# read bluesky forecast polygon
# TODO: Make sure the file exists first. If it does not, generate user friendly
# TODO: error message. 
# TODO: What are the different layers in this SPDF? 
smk_forecast <- readOGR(dsn = poly_path, layer = poly_layer)

# read in hia estimate ----
hia_path <- "./data/hia_poly"
hia_layer <- "hia_poly"

# hia polygon
county_hia <- readOGR(dsn = hia_path, layer = hia_layer)

# Note 2017-12-29: Decided not to cap county population-wted pm, but I will need
# to reconcile cap of grid values polygon with this

# define color bin for smoke layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(0, 10, 20, 30, 40, 50, 100, 250, 1000)
pal <- colorBin(c("gray", "darkred"), domain = c(0,1000), bins = bin,
                na.color = "transparent") # "#F0F2F0", "#000c40"

################################################################################
# Ryan, what is going on here in this section. Where do these numbers come from?
# add another legend for relative risk resp
resp_bin <- round(exp((bin/10)*0.0507),2)

# resp pal
resp_pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(1,max(resp_bin)), 
                     bins = resp_bin, na.color = "red") # "transparent" to hide, if desired
# asthma
asthma_bin <- round(exp((bin/10)*0.0733),2)

# asthma pal 
# TODO: Make different from 'resp pal'
asthma_pal <- colorBin(c("#F0F2F0", "#000c40"), domain = c(1,max(asthma_bin)), 
                       bins = asthma_bin, na.color = "transparent")
################################################################################

# define color bin for hia estimates
# Ryan comment: I do not think these values will exceed 300... but it could happen
# TODO: Make a log of when this does happen. 
hia_bin <- c(1, 10, 25, 50, 100, 150, 200, 250)

# hia pallet
hia_pal <- colorBin(c("#fcb045", "#fd1d1d"), domain = c(1, max(hia_bin)),
                    bins = hia_bin, na.color="transparent")

# Note 2017-12-29: Think about the best way to represent the scales;
# it may be okay to redefine them every day

# read in saved R dates ----
load("./data/date_label.RData")

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# read in fire locations ----
fire_locations <- read.csv("./data/fire_locations.csv")
# type indicates either wildfire (WF) or prescription burn (RX)
# set color of WF to red and RX to green
# TODO: Make sure this is communicated. I do not think green for RX makes sense
# TODO: for this app. We are trying to communicate health impacts of smoke. Green
# TODO: makes it seem like the smoke from these fires is healthy, go, or O.K.
pal_fire <- colorFactor(
  palette = c("red", "green"),
  levels = c("WF", "RX")
)


# shiny dash board ui ----
# note: 7/14/2017: I like the dashboard layout, but it may be better to define
# the three elemnts of the dashboard outside the ui.
# header
head <- dashboardHeader(
  tags$li(class = "dropdown", tags$a(href = "https://github.com/RyanGan/smoke_forecaster/blob/development/README.md", "About")),
  tags$li(class = "dropdown", tags$a(href = "mailto:sjbrey@rams.colostate.edu", "Contact us")),
  tags$li(class = "dropdown", tags$a(href = "https://github.com/RyanGan/smoke_forecaster/issues", "Report Bug")),
  title = "Smoke HIA Forecaster (beta)",
  titleWidth = 300
  )

# side bar
side <- dashboardSidebar(
  # reactive sidebar
  selectInput(inputId="date_smoke", label = h3("Date to Forecast"),
              choices = date_list, selected = "layer_1")
) # end side bar


# body
body <- dashboardBody(
  # set tag style
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")
)# end dashboard body


# ui function with dashboard header, side, and body
ui <- dashboardPage(head, side, body, skin = "black") 

# server section ---- 
# consider adding a session function if I want to know statistics
server <- (function(input, output){
  # add base leaflet map
  # TODO: Set users LOCATION as the default center of view
  # TODO: https://github.com/AugustT/shiny_geolocation
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      
      # call map layer
      addTiles() %>% 
      
      # set bounds of map
      fitBounds(lng1=-123.925,lng2=-74.425, lat1=48.225, lat2=33.975)# %>% 
      
      # add fire locaiton icons
      # TODO: Fire locations do not change with date selection. This is confusing.
      # TODO: Make fire locations a dynamic layer same as HIA and such. 
      # addCircleMarkers(data = fire_locations, lat = fire_locations$latitude, 
      #                  lng = fire_locations$longitude, color = ~pal_fire(type),
      #                  radius = ~sqrt(area/100), fill=F, weight = 0.5) #%>% 
      
      # TODO: Until these legends are more clearly explained, or have links to
      # TODO: informative documentation and are different colors, they are going
      # TODO: to be hidden. 
      # # add legend for smoke values
      # addLegend(pal=pal, values=c(0, 250), 
      #           title = htmltools::HTML("Smoke <span>&#181;</span>g/m<sup>3</sup>"),
      #           position = "bottomleft") %>% 
      # 
      # # add respiratory legend
      # addLegend(pal = asthma_pal, values= c(min(asthma_bin), max(asthma_bin)),
      #           title = htmltools::HTML("Asthma <br> Relative Risk"),
      #           position = "bottomright") %>% 
      # 
      # # add respiratory legend
      # addLegend(pal = resp_pal, values= c(min(resp_bin), max(resp_bin)),
      #           title = htmltools::HTML("Respiratory <br> Relative Risk"),
      #           position = "bottomright") 
    
  })# end base leaflet
  
  # add interactive polygon layers -----
  observeEvent(input$date_smoke,{
    
    # set index as 1 or 2 for easier index
    layer_name <- as.character(input$date_smoke)
    
    # define reactive label values of smoke concentrations and relative risks
    # TODO: Give vals a more descriptive name 
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
    
    # HIA labels
    hia_vals <- reactive({getElement(county_hia@data, layer_name)})
    
    # hia labels and popwt
    hia_label <- sprintf(
      paste0("<strong> Estimated Respiratory Emergency Department Visits: %g"), 
      # number for smoke concentration
      hia_vals()) %>% 
      lapply(htmltools::HTML)
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      clearShapes() %>% 
      # set a box that defines the dimensions of bluesky forecast
      addRectangles(lng1=-130.0,lng2= -59.95, lat1=22.5, lat2=52.5,
                    fillColor = "transparent", color = "black", weight = 2) %>%
      
      # add smoke polygons 
      addPolygons(data = smk_forecast, group = "Smoke", color = "transparent", 
                  fillColor = ~pal(vals()), weight=1, smoothFactor=1, fillOpacity=polyOpacity, 
                  # add highlight option
                  highlight = highlightOptions(weight = 5, color = "blue", 
                                               bringToFront = T, fillOpacity = polyBorderOpacity),
                  # add smoke pm values
                  label = pm_label,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"), 
                                              textsize = "12px", direction = "auto")) %>% 
      
      # add HIA polygon
      addPolygons(data = county_hia, group = "HIA", color = "transparent",
                  fillColor = ~hia_pal(hia_vals()), weight=1, smoothFactor=1, fillOpacity=polyOpacity,
                  # add highlight option
                  highlight = highlightOptions(weight = 5, color = "red", 
                                               bringToFront = T, fillOpacity = polyBorderOpacity),
                  # add hia resp est values
                  label = hia_label,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"), 
                                              textsize = "12px", direction = "auto"))  %>% 
      
      # add fire locations 
      addMarkers(
        lng=fire_locations$longitude,
        lat=fire_locations$latitude,
        # clusterOptions = markerClusterOptions(
        #   spiderLegPolylineOptions = list(weight = 0, color = "#222", opacity =0.5)
        #   ),
        icon = fireIcons,
        label= paste(fire_locations$type),
        popup=paste("<b>", "Area:","</b>", fire_locations$area,"<br>",
                    "<b>", "Type:", "</b>", fire_locations$type,"<br>"),
        group="Fires"
      ) %>%
      
      # add layer control
      addLayersControl(
        baseGroups = c("Smoke"),
        overlayGroups = c("Smoke", "HIA", "Fires"),
        options = layersControlOptions(collapsed = F)
      )
    
  }) # end reactive layer
  
}) # end server function

# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


