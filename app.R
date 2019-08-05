# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan 
# Date Created: 2017-06-17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

#### THIS VERSION IS MEANT ONLY FOR RUNNING THE SHINY APP ON A LOCAL COMPUTER.
#### DELETE THESE LINES OF CODE AND SECTIONS THAT SAY "## uncomment this later: "
#### BEFORE RUNNING THIS ON THE SERVER. ALSO UNDO THE "## changed in local" changes.


#------------------------------------------#
#-----------------SETUP--------------------#
#------------------------------------------#

# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal) # for read shapefile
library(stringr)
library(RColorBrewer) # to get pretty colors that are colorblind friendly


#------------------------------------------#
#-------SYMBOLOGY FOR ALL LAYERS-----------#
#------------------------------------------#

# Polygon color options 
polyOpacity <- 0.5
polyBorderOpacity <- .7

fire_color <- function(fires) {
  sapply(fires$type, function(fire) {
    if(fire =="RX") {
      "orange"
    } else if(fire == "WF") {
      "red"
    } else {
      "red"
    } })
}

fire_pal <- colorFactor(
  palette = c("red", "pink"),
  levels = c("WF", "RX")
)

# define color bin for health impact assessment estimates
# Ryan comment: I do not think these values will exceed 300... but it could happen
# TODO: Make a log of when this does happen. 
hia_bin <- c(1, 10, 25, 50, 100, 150, 200, 250)

# hia pallet
hia_pal <- colorBin(brewer.pal(n=length(hia_bin-2), "YlOrRd"), 
                    domain = c(1, max(hia_bin)),
                    bins = hia_bin, 
                    na.color="transparent")
#hia_pal <- brewer.pal(n=length(hia_bin-2), "PuRd")

# define color bin for smoke layer ----
# going with a bin since it will be easier to handle extreme colors
smoke_bin <- c(2, 25, 50, 200, 1000)
smoke_pal <- colorBin(c("#feb14c", "#fd8c3c", "#f03b20", "#bd0026"), 
                      domain = c(1, 1000), 
                      bins = smoke_bin,
                      na.color = "black") # "#F0F2F0", "#000c40"


#------------------------------------------#
#-----SET PATHS AND LOAD CRITICAL DATA-----#
#------------------------------------------#

# read in smoke forecast shapefile ----
# define relative path to polygon file
#poly_path <- "./data/smk_poly"
poly_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/smk_poly" ## changed in local

# # read bluesky forecast polygon for the two forecasted dates 
smk_forecast_1 <- readOGR(dsn = poly_path, layer = "smk_poly_1")
smk_forecast_2 <- readOGR(dsn = poly_path, layer = "smk_poly_2")

# read in hia estimate ----
#hia_path <- "./data/hia_poly"  # original
hia_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/hia_poly"
hia_layer <- "hia_poly"

# hia polygon
county_hia <- readOGR(dsn = hia_path, layer = hia_layer)

# Current smoke conditions
#latest_smoke <- readOGR(dsn="./data/HMS", layer="latest_smoke_display") # Original path
latest_smoke <- readOGR(
  dsn="C:/Users/apddsouth/Documents/Smoke_Predictor/data/HMS", 
  layer="latest_smoke_display")  ## modified path

# Note 2017-12-29: Decided not to cap county population-wted pm, but I will need
# to reconcile cap of grid values polygon with this

# read in saved R dates ----
#load("./data/date_label.RData") # original
#date_labels[1] <- paste(date_labels[1], "(today)") # original
#date_labels[2] <- paste(date_labels[2], "(tomorrow)") # original
date_labels <- c('today', 'tomorrow')

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# read in fire_locations ----
#load(here::here("Smoke_Predictor/data/", "fire_locations.RData"))
load("C:/Users/apddsouth/Documents/Smoke_Predictor/data/fire_locations.RData")


#------------------------------------------#
#--------SETUP SHINY DASHBOARD UI----------#
#------------------------------------------#

head <- dashboardHeader(
  tags$li(class = "dropdown", tags$a(href = "https://github.com/smartenies/smoke_forecaster/blob/development/README.md", "About")),
  tags$li(class = "dropdown", tags$a(href = "mailto:Sheena.Martenies@colostate.edu", "Contact Us")),
  tags$li(class = "dropdown", tags$a(href = "https://github.com/smartenies/smoke_forecaster/issues", "Report Bug")),
  tags$li(class = "dropdown", tags$a(href = "", "More Info")),
  title = "Smoke Health Impact Assessment (HIA) Forecaster (beta)",
  titleWidth = 550
)

# side bar
side <- dashboardSidebar(
  #sidebarPanel("<p>Testtesttest </p>"),
  # reactive sidebar
  selectInput(inputId="date_smoke", 
              label = h3("Date to forecast:"),
              choices = date_list, 
              selected = "layer_1"),
  ## uncomment this later: ,
  ## Show the forecast hour for the smoke data being displayed
  ## uncomment this later: fluidRow(
  ## uncomment this later:   column(align="center", width=12,
           #p(paste0("Model Run: ", forecast_date, " ",forecast_hour, "Z"))
  ## uncomment this later:          p(tags$a(href = forecast_url, 
  ## uncomment this later:                   paste0("Model Run Used: ", forecast_date, " ", forecast_hour, "Z")))
  ## uncomment this later:          )
  ## uncomment this later:   )
  h3(textOutput("caption"))
) # end side bar

# body
body <- dashboardBody(
  # set tag style
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")
) # end dashboard body


# ui function with dashboard header, side, and body
ui <- dashboardPage(head, side, body, skin = "black")

#------------------------------------------#
#----------SETUP SHINY SERVER--------------#
#------------------------------------------#

server <- (function(input, output){
  # add base leaflet map
  # TODO: Set users LOCATION as the default center of view
  # TODO: https://github.com/AugustT/shiny_geolocation
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      
      # call map layer
      #addTiles() %>%  # original basemap
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # dark grey basemap
      
      # set bounds of map
      fitBounds(lng1=-123.925,lng2=-74.425, lat1=48.225, lat2=33.975) %>% 
      
      # This buttom allows the user to find their location. 
      # TODO: Fix, it does not appear to always work. Security issue? 
      addEasyButton(
        easyButton(
          position = "topleft",
          icon = "fa-crosshairs",
          title = "My location",
          onClick = JS(
            c(
              "function(btn,  map){map.locate({setView:true,enableHighAccuracy:true})}"
            )
          )
        )
      ) %>%
      
      # add legend for smoke values
      addLegend(pal=smoke_pal, 
                values=c(0, 1000),
                title = htmltools::HTML("Smoke Particulate Matter (PM<sub>2.5</sub>, <span>&#181;</span>g/m<sup>3</sup>)"),
                position = "bottomleft",
                group="Forecasted Smoke") %>%
    
      # add legend for Emergency Dept. Visits
      addLegend(pal=hia_pal,
                values=hia_bin,
                title = htmltools::HTML("<strong>Emergency Dept. Visits</strong>"),
                position = "bottomleft",
                group="Emergency Dept. Visits")
    
      # add legend for Fire Locations
      #addLegend(pal=fire_pal,
      #        values=c("WF", "RX"),
      #        title = htmltools::HTML("<strong>Fire Locations</strong>"),
      #        position = "bottomright",
      #        group="Fire Locations")

  })# end base leaflet
  
  # add interactive polygon layers -----
  observeEvent(input$date_smoke,{
    
    # set index as 1 or 2 for easier index
    layer_name <- as.character(input$date_smoke)
    
    # define reactive label values of smoke concentrations and relative risks
    # TODO: Give vals a more descriptive name 
    if(layer_name == "layer_1"){
      vals <- reactive({getElement(smk_forecast_1@data, layer_name)})
      smk_forecast_display <- smk_forecast_1
    } else{
      vals <- reactive({getElement(smk_forecast_2@data, layer_name)})
      smk_forecast_display <- smk_forecast_2
    }

    # Smoke Concentration: value ug/m^3 \return
    # Relative Increase in Risk: value %
    pm_label <- sprintf(paste0(
      "<strong>24-hr smoke concentration: %g <span>&#181;</span>g/m<sup>3</sup></strong>",
      "<br><strong>Respiratory Relative Risk: %g</strong>",
      "<br><strong>Asthma Relative Risk: %g</strong>"), 
      # number for smoke concentration
      round(vals(),1),
      # number for relative risk respiratory
      round(exp(vals()/10*0.0507),2),
      # number for relative risk asthma
      round(exp(vals()/10*0.0733),2)) %>% 
      lapply(htmltools::HTML)
    
    # Health Impact Assessment labels
    hia_vals <- reactive({getElement(county_hia@data, layer_name)})
    hia_county_name <- reactive({getElement(county_hia@data, "NAME")}, quoted=T)
    hia_county_pop <- reactive({getElement(county_hia@data, "Pop")})
  
    # call proxy map
    leafletProxy(mapId="map") %>%
      clearShapes() %>% 
      
      
      # add smoke polygons 
      addPolygons(data = smk_forecast_display, 
                  group = "Forecasted Smoke", 
                  color = "transparent", 
                  fillColor = ~smoke_pal(vals()), 
                  weight=1, 
                  smoothFactor=1, 
                  fillOpacity=polyOpacity, 
                  # add highlight option
                  highlight = highlightOptions(weight = 5, 
                                               color = "blue", 
                                               bringToFront = T, 
                                               fillOpacity = polyBorderOpacity),
                  # add smoke pm values
                  label = pm_label,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"), 
                                              textsize = "12px", direction = "auto")
                  ) %>% 
      
      # add HIA polygon
      # TODO: Make these labels nice and more informative following example of link below. 
      # http://rpubs.com/bhaskarvk/electoral-Map-2016
      addPolygons(data = county_hia, 
                  group = "Emergency Dept. Visits", 
                  fillColor = ~hia_pal(hia_vals()), 
                  stroke=FALSE,
                  weight=1, 
                  smoothFactor=1, 
                  fillOpacity=polyOpacity,
                  # add highlight option
                  highlight = highlightOptions(weight = 3, 
                                               color = "red", 
                                               bringToFront = T, 
                                               fillOpacity = polyBorderOpacity),
                  popup=paste("<strong>County:</strong>", hia_county_name(),
                              "<br><strong>Population:</strong>", hia_county_pop(),
                              "<br><strong>Emergency Department vists:</strong>", hia_vals()),
                  label=paste("Click for", hia_county_name()," county Health Impact Assessment"),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "12px",
                                              direction = "auto")
                  )  %>% 
      
      {if (layer_name=="layer_1")
        addPolygons(., data = latest_smoke,
                    group = "Analyzed Plumes",
                    popup = paste("<b>Analyzed:</b>", latest_smoke$X1,
                                  "<br><b>Satellite:</b>", latest_smoke$Satellite,
                                  "<br><b>Density:</b>", latest_smoke$Density, "~&#181;</span>g/m<sup>3</sup>",
                                  "<br><b>Details:</b> www.ospo.noaa.gov/Products/land/hms.html"),
                    color = "Gray",
                    label = "Smoke plume drawn by HMS analyst"
                    ) else .} %>%
      
      
      addCircleMarkers(data = fire_locations, 
                       lat = fire_locations$latitude,
                       lng = fire_locations$longitude, 
                       color = ~pal_fire(type),
                       radius = ~sqrt(area/100), 
                       fill=F, 
                       weight = 0.5,
                       group="Fire Locations",
                       label=paste0("Type: ",fire_locations$type, " | Area: ", round(fire_locations$area))) %>% 
      
      # add layer control, but ommit Analyzed Plumes if the "tomorrow" input is chosen by user
      {if (layer_name=="layer_1") addLayersControl(
        .,
        overlayGroups = c("Emergency Dept. Visits", 
                          "Fire Locations", 
                          "Forecasted Smoke",
                          "Analyzed Plumes"),
        options = layersControlOptions(collapsed = F))
        else addLayersControl(.,
          overlayGroups = c("Emergency Dept. Visits", 
                            "Fire Locations", 
                            "Forecasted Smoke"),
          options = layersControlOptions(collapsed = F))
        }%>%
      
      # Set default hidden groups 
      hideGroup(group=c("Forecasted Smoke","Analyzed Plumes"))
    
    
  }) # end reactive layer
  
}) # end server function

# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


