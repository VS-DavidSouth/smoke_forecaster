# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan 
# Date Created: 2017-06-17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.

# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal) # for read shapefile
library(stringr)

# Get information on when smoke data were last downloaded. This code is ugly as sin. 
forecast_date <- stringr::str_sub(readLines("bluesky_download_log.txt")[2], 16, 23)
forecast_hour <- stringr::str_sub(readLines("bluesky_download_log.txt")[3], 16, 17)

# Polygon color options 
polyOpacity <- 0.7
polyBorderOpacity <- 1

# fireIcons <- icons(
#   iconUrl = "http://thediscipleproject.net/wp-content/uploads/2013/07/fire-vector.png",
#   iconWidth = 17, 
#   iconHeight = 17
# )

pal_fire <- colorFactor(
  palette = c("red", "green"),
  levels = c("WF", "RX")
)

# Set paths and load required data ---------------------------------------------

# read in smoke forecast shapefile ----
# define relative path to polygon file
poly_path <- "./data/smk_poly"

# # read bluesky forecast polygon for the two forecasted dates 
smk_forecast_1 <- readOGR(dsn = poly_path, layer = "smk_poly_1")
smk_forecast_2 <- readOGR(dsn = poly_path, layer = "smk_poly_2")

# read in hia estimate ----
hia_path <- "./data/hia_poly"
hia_layer <- "hia_poly"

# hia polygon
county_hia <- readOGR(dsn = hia_path, layer = hia_layer)

# Current smoke conditions
latest_smoke <- readOGR(dsn="./data/HMS", layer="latest_smoke_display")

# Note 2017-12-29: Decided not to cap county population-wted pm, but I will need
# to reconcile cap of grid values polygon with this

# define color bin for smoke layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(2, 10, 20, 30, 40, 50, 100, 250, 1000)
pal <- colorBin(palette=c("gray", "red", "purple"), 
                domain = c(0,1000), 
                bins = bin,
                na.color = "black") # "#F0F2F0", "#000c40"

################################################################################
# Ryan, what is going on here in this section. Where do these numbers come from?
# add another legend for relative risk resp
resp_bin <- round(exp((bin/10)*0.0507), 2)

# resp pal
resp_pal <- colorBin(c("#F0F2F0", "#000c40"), 
                     domain = c(1,max(resp_bin)), 
                     bins = resp_bin, 
                     na.color = "red") # "transparent" to hide, if desired
# asthma
asthma_bin <- round(exp((bin/10)*0.0733), 2)

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
hia_pal <- colorBin(c("blue", "orange"), 
                    domain = c(1, max(hia_bin)),
                    bins = hia_bin, 
                    na.color="transparent")

# read in saved R dates ----
load("./data/date_label.RData")
date_labels[1] <- paste(date_labels[1], "(today)")
date_labels[2] <- paste(date_labels[2], "(tomorrow)")

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# read in fire_locations ----
load("./data/fire_locations.RData")

# TODO: Current conditions

# shiny dash board ui ----
head <- dashboardHeader(
  tags$li(class = "dropdown", tags$a(href = "https://github.com/RyanGan/smoke_forecaster/blob/development/README.md", "About")),
  tags$li(class = "dropdown", tags$a(href = "mailto:sjbrey@rams.colostate.edu", "Contact us")),
  tags$li(class = "dropdown", tags$a(href = "https://github.com/RyanGan/smoke_forecaster/issues", "Report Bug")),
  title = "Smoke Health Impact Assessment (HIA) Forecaster (beta)",
  titleWidth = 550
)

# side bar
side <- dashboardSidebar(
  # reactive sidebar
  selectInput(inputId="date_smoke", 
              label = h3("Date to forecast:"),
              choices = date_list, 
              selected = "layer_1"),
  
  # Show the forecast hour for the smoke data being displayed
  fluidRow(
    column(align="center", width=12,
           p(paste0("Model Run: ", forecast_date, " ",forecast_hour, "Z"))
    )
  )
) # end side bar


# body
body <- dashboardBody(
  # set tag style
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")
) # end dashboard body


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
      fitBounds(lng1=-123.925,lng2=-74.425, lat1=48.225, lat2=33.975) %>% 
      
      # This buttom allows the user to find thier location. 
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
      addLegend(pal=pal, 
                values=c(0, 1000),
                title = htmltools::HTML("24-hr Smoke PM<sub>2.5</sub> [<span>&#181;</span>g/m<sup>3</sup>]"),
                position = "bottomleft",
                group="Forecasted Smoke") %>%
    
      addLegend(pal=hia_pal,
                values=hia_bin,
                title = htmltools::HTML("<strong>Emergency Dept. Visits</strong>"),
                position = "bottomleft",
                group="HIA")

  })# end base leaflet
  
  # add interactive polygon layers -----
  observeEvent(input$date_smoke,{
    
    # set index as 1 or 2 for easier index
    layer_name <- as.character(input$date_smoke)
    
    print("layer_name")
    print(layer_name)
    
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
    
    # HIA labels
    hia_vals <- reactive({getElement(county_hia@data, layer_name)})
    hia_county_name <- reactive({getElement(county_hia@data, "NAME")}, quoted=T)
    hia_county_pop <- reactive({getElement(county_hia@data, "Pop")})
    
    hia_label <- sprintf(paste0(
      "<strong>Estimated Respiratory Emergency Department Visits: %g </strong>",
      #"<br> County Name: %g",
      "<br> County Population: %g"), 
      # number for smoke ED visits
      hia_vals(),
      # # County Name
      # hia_county_name(),
      # number for relative risk asthma
      hia_county_pop())%>% 
      lapply(htmltools::HTML)
    
    # call proxy map
    leafletProxy(mapId="map") %>%
      clearShapes() %>% 
      
      # set a box that defines the dimensions of bluesky forecast
      addRectangles(lng1=-130.0,
                    lng2= -59.95, 
                    lat1=22.5, 
                    lat2=52.5,
                    fillColor = "transparent", 
                    color = "black", 
                    weight = 2,
                    group = "Forecasted Smoke") %>%
      
      # add smoke polygons 
      addPolygons(data = smk_forecast_display, 
                  group = "Forecasted Smoke", 
                  color = "transparent", 
                  fillColor = ~pal(vals()), 
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
                                              textsize = "12px", direction = "auto")) %>% 
      
      # add HIA polygon
      # TODO: Make these labels nice and more informative following example of link below. 
      # http://rpubs.com/bhaskarvk/electoral-Map-2016
      addPolygons(data = county_hia, 
                  group = "HIA", 
                  color = "transparent",
                  fillColor = ~hia_pal(hia_vals()), 
                  weight=1, 
                  smoothFactor=1, 
                  fillOpacity=polyOpacity,
                  # add highlight option
                  highlight = highlightOptions(weight = 5, 
                                               color = "red", 
                                               bringToFront = T, 
                                               fillOpacity = polyBorderOpacity),
                  # add hia resp est values
                  label = hia_label,
                  # label=paste0(county_hia@data$NAME, " county ",
                  #              "(population ", county_hia@data$Pop,"), ",
                  #              "Emergency Department vists: ", hia_vals()),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"), 
                                              textsize = "12px", 
                                              direction = "auto")
      )  %>% 
      # TODO: Give these a colorbar! Make different concentrations different
      # TODO: colors! 
      # TODO: Do not allow this to display when the date is set for tomorrow. 
      addPolygons(data = latest_smoke,
                  group = "Analysed Plumes",
                  popup = paste("<b>Analysed:</b>", latest_smoke$Start, "Z",
                                "<br><b>Satellite:</b>", latest_smoke$Satellite,
                                "<br><b>Density:</b>", latest_smoke$Density, "~&#181;</span>g/m<sup>3</sup>",
                                "<br><b>Detials:</b> www.ospo.noaa.gov/Products/land/hms.html"),
                  color = pal(latest_smoke$Density),
                  label = "Smoke plume drawn by HMS analyst"
      ) %>%
      
      
      addCircleMarkers(data = fire_locations, 
                       lat = fire_locations$latitude,
                       lng = fire_locations$longitude, 
                       color = ~pal_fire(type),
                       radius = ~sqrt(area/100), 
                       fill=F, 
                       weight = 0.5,
                       group="Fire Locations",
                       label=paste0("Type: ",fire_locations$type, " | Area: ", round(fire_locations$area))) %>% 
      
      # add layer control
      addLayersControl(
        overlayGroups = c("Forecasted Smoke", 
                          "HIA", 
                          "Fire Locations", 
                          "Analysed Plumes"),
        options = layersControlOptions(collapsed = F)
      ) %>%
      
      # Set defualt hidden groups 
      hideGroup(group=c("HIA","Analysed Plumes"))
    
    
  }) # end reactive layer
  
}) # end server function

# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


