# ------------------------------------------------------------------------------
# Title: App script for smoke forecaster shiny app
# Author: Ryan Gan 
# Date Created: 2017-06-17
# R Version 3.4.0 
# ------------------------------------------------------------------------------

# note this script contains both ui and server function. I modified to make this 
# version lighter as the server can't handle the raster brick. That code still
# exists in the ui and server code.


#------------------------------------------#
#-----------------SETUP--------------------#
#------------------------------------------#

# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(shinyBS)
library(leaflet)
library(rgdal) # for read shapefile
library(stringr)
library(RColorBrewer) # to get pretty colors that are colorblind friendly


#------------------------------------------#
#-------SYMBOLOGY FOR ALL LAYERS-----------#
#------------------------------------------#

fire_pal <- colorFactor(
  palette = c("red", "orange"),
  levels = c("WF", "RX")
)

# define color bin for health impact assessment estimates
# Ryan comment: I do not think these values will exceed 300... but it could happen
# TODO: Make a log of when this does happen. 
hia_bin <- c(1, 10, 50, 100, 200, 300)

# hia pallet
#hia_pal <- colorBin(brewer.pal(n=length(hia_bin-2), "Purples"),
hia_pal <- colorBin(palette=c("#bfd3e6", "#8c96c6", "#8856a7", "#810f7c"),
                    
                    bins = hia_bin, 
                    na.color="transparent")
#hia_pal <- brewer.pal(n=length(hia_bin-2), "PuRd")

# define color bin for predictged smoke layer
smoke_bin <- c(1, 10, 25, 50, 200, 1000)
smoke_pal <- colorBin(brewer.pal(length(smoke_bin), "YlOrBr"), 
                      domain = c(1, 1000), 
                      bins = smoke_bin,
                      na.color = "black") # "#F0F2F0", "#000c40"

analyzed_plumes_color <- "grey"
analyzed_plumes_outline_color <- "transparent"


#------------------------------------------#
#-----SET PATHS AND LOAD CRITICAL DATA-----#
#------------------------------------------#

# read in smoke forecast shapefile ----
# define relative path to polygon file
poly_path <- "./data/smk_poly"
#poly_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/smk_poly" ## changed in local

# # read bluesky forecast polygon for the two forecasted dates 
smk_forecast_1 <- readOGR(dsn = poly_path, layer = "smk_poly_1")
smk_forecast_2 <- readOGR(dsn = poly_path, layer = "smk_poly_2")

# read in hia estimate ----
hia_path <- "./data/hia_poly"  # original
#hia_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/hia_poly"
hia_layer <- "hia_poly"

# hia polygon
county_hia <- readOGR(dsn = hia_path, layer = hia_layer)

# Current smoke conditions
analyzed_plumes <- readOGR(dsn="./data/HMS", layer="latest_smoke_display") # Original path
#analyzed_plumes <- readOGR(
   #dsn="C:/Users/apddsouth/Documents/Smoke_Predictor/data/HMS", 
   #layer="latest_smoke_display")  ## modified path

# Note 2017-12-29: Decided not to cap county population-wted pm, but I will need
# to reconcile cap of grid values polygon with this

# read in saved R dates ----
load("./data/date_label.RData") # original
date_labels[1] <- paste(date_labels[1], "(today)") # original
date_labels[2] <- paste(date_labels[2], "(tomorrow)") # original
#date_labels <- c('today', 'tomorrow') # changed in local

# create date names list to use with the radio button
date_list <- list("layer_1", "layer_2")
names(date_list) <- date_labels

# read in fire_locations ----
load(here::here("Smoke_Predictor/data/", "fire_locations.RData"))
#load("C:/Users/apddsouth/Documents/Smoke_Predictor/data/fire_locations.RData")

# read when the HMS smoke plumes were updated. This creates the updated_date value, which is used later.
# When testing this app, just set updated_date <-  [today's date] in format "YYYY-MM-DD"
load(here::here("Smoke_Predictor/data/HMS/", "plume_update_date.Rdata"))
#load("C:/Users/apddsouth/Documents/Smoke_Predictor/data/HMS/plume_update_date.RData") # changed in local

#------------------------------------------#
#--------SETUP SHINY DASHBOARD UI----------#
#------------------------------------------#

head <- dashboardHeader(
  tags$li(class = "dropdown", tags$a(href = "https://github.com/smartenies/smoke_forecaster/blob/development/README.md", "About")),
  tags$li(class = "dropdown", tags$a(href = "mailto:Sheena.Martenies@colostate.edu", "Contact Us")),
  tags$li(class = "dropdown", tags$a(href = "https://github.com/smartenies/smoke_forecaster/issues", "Report Bug")),
  tags$li(class = "dropdown", tags$a(href = "https://github.com/smartenies/smoke_forecaster/blob/sm_local/general_audience_information.Rmd", "More Info")),
  title = "Smoke Health Impact Assessment (HIA) Forecaster (beta)",
  titleWidth = 550
)

# side bar
side <- dashboardSidebar(
  width = 300,
  # create a drop-down menu for the user to decide the day
  selectInput(inputId="date_smoke", 
              label = h3("Date to forecast:"),
              choices = date_list, 
              selected = "layer_1"),
  # Show the forecast hour for the smoke data being displayed
  fluidRow(
    column(align="center", width=12,
           #p(paste0("Model Run: ", forecast_date, " ",forecast_hour, "Z"))
           p(tags$a(href = forecast_url, 
                    paste0("Model Run Used: ", forecast_date, " ", forecast_hour, "Z")))
           )
    ),
  
  # Add some descriptive text
  tags$div(class="header", checked=NA,
           tags$p("Click below to learn about the map layers and how these calculations were made. 
                  For a more detailed explanation, click the ", strong("More Info"), "tab.")),
  
  # Create a series of  collapsable panels that give useful information. Uses the ShinyBS library.  
  # More info on collapsing panels here: https://ebailey78.github.io/shinyBS/docs/Collapses.html#bsCollapsePanel
  bsCollapse(id = "collapseExample", open = "Fire Locations",
             
             # Info for Fire Locations panel
             bsCollapsePanel(HTML('<font size="3" color="black">Fire Locations</font>'), 
                             tags$div(style="color:black", 
                                      HTML(paste("The <b>Fire Locations</b> layer shows the sites of fires burning across the United States and 
                                      Canada. <span style=\"color: red\">Wildfires (WF)</span> are shown in red and 
                                      <span style=\"color: orange\">prescribed burns (RX)</span> are shown in yellow.
                                      RX burns are generally smaller and are continuously monitored by fire crews.",
                                      "The locations come from from BlueSky, a model developed by the
                                      United States Forest Service. View BlueSky data 
                                      <a href=\"https://tools.airfire.org/websky/v1/#status\" style=\"color:#0C81A9\">here</a>,
                                      more information about the BlueSky model 
                                      <a href=\"https://www.fs.fed.us/bluesky/about/\" style=\"color:#0C81A9\">here</a>.",
                                      sep="<br><br/>"))), 
                             style = "info"),
             
             # Info for forecasted smoke panel
             bsCollapsePanel(HTML('<font size="3" color="black">Forecasted Smoke Concentration</font>'), 
                             tags$div(style="color:black", 
                                      HTML(paste("The <b>Forecasted Smoke Concentration</b> layer uses data from the BlueSky model (see <b>Fire Locations</b> tab)
                                      to estimate the ground-level concentrations of smoke due to wildfires.", 
                                      "Hover your mouse over an area with wildfire smoke to see smoke concentration and
                                      the Relative Risk values. Respiratory and Asthma Relative Risk levels above 1
                                      indicate that people exposed to the smoke have a higher chance of going to the emergency department.",
                                      "The Smoke Forecaster uses the values modeled in this layer to estimate the number
                                      of emergency department visits that might occur due to wildfire smoke exposure, shown in 
                                      the <b>Emergency Dept. Visits</b> layer.",
                                      sep="<br><br/>"))), 
                             style = "info"),
             
             # Info for smoke plumes panel
             bsCollapsePanel(HTML('<font size="3" color="black">Visible Smoke Plumes</font>'), 
                             tags$div(style="color:black", 
                                      HTML(paste("The <b>Visible Smoke Plumes</b> layer shows wildfire smoke plumes visible from satellite imagery. 
                                      The data are generated each day by analysts for the Hazard Mapping System (HMS) which 
                                      is run by the National Oceanic and Atmospheric Association (NOAA).", 
                                      "Occasionally the daily HMS data will not have been released
                                      by the time the Smoke Forecaster is updated. In this case, the Visible Smoke Plumes 
                                      layer will not be displayed. The layer is also disabled when the Smoke Forecaster date
                                      is set to tomorrow.",
                                      sep="<br><br/>"))), 
                             style = "info"),
             # Info for Emergency Dept. Visits panel
             bsCollapsePanel(HTML('<font size="3" color="black">Emergency Dept. Visits</font>'), 
                             tags$div(style="color:black", 
                                      HTML(paste("You can click on the counties of the <b>Emergnecy Dept. Visits</b> layer to show the expected number of 
                                      emergency department visits due to wildfire smoke exposure. 
                                      The Smoke Forecaster uses a health impact function to generate an estimate of how many
                                      additional people might visit the emergency department for any respiratory health condition or for asthma
                                      in particular.",
                                      "This estimate is based on the concentration of wildfire smoke in the air, the typical
                                      number of emergency department visits per day for county, the number of people 
                                      who live in the county, and the relationship between wildfire smoke and emergency department visits.",
                                      sep="<br><br/>"))), 
                             style = "info"))
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
                title = htmltools::HTML("Forecasted Smoke Concentration (PM<sub>2.5</sub>, <span>&#181;</span>g/m<sup>3</sup>)"),
                position = "bottomleft",
                group="Forecasted Smoke") %>%
    
      # add legend for Emergency Dept. Visits
      addLegend(pal=hia_pal,
                values=hia_bin,
                title = htmltools::HTML("<strong>Emergency Dept. Visits</strong>"),
                position = "bottomleft",
                group="Emergency Dept. Visits") %>% 
    
      # add legend for Fire Locations
      addLegend(pal=fire_pal,
              values=c("WF", "RX"),
              title = htmltools::HTML("<strong>Fire Locations</strong>"),
              position = "bottomright",
              group="Fire Locations")

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
      
      
      # add smoke forecast polygons 
      addPolygons(data = smk_forecast_display, 
                  group = "Forecasted Smoke Concentration", 
                  stroke=FALSE, 
                  fillColor = ~smoke_pal(vals()), 
                  weight=1, 
                  smoothFactor=1, 
                  fillOpacity=.9, 
                  # add highlight option
                  highlight = highlightOptions(weight = 5, 
                                               bringToFront = TRUE, 
                                               fillOpacity = 1),
                  # add smoke pm values
                  label = pm_label,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"), 
                                              textsize = "12px", direction = "auto")
                  ) %>% 
      
      # add HIA polygon
      addPolygons(data = county_hia, 
                  group = "Emergency Dept. Visits", 
                  fillColor = ~hia_pal(hia_vals()), 
                  stroke=TRUE,
                  color="black",
                  weight=1, 
                  smoothFactor=1, 
                  fillOpacity=.9,
                  # add highlight option
                  highlight = highlightOptions(weight = 3, 
                                               color = "red", 
                                               bringToFront = T, 
                                               fillOpacity =0.5),
                  popup=paste("<strong>County:</strong>", hia_county_name(),
                              "<br><strong>Population:</strong>", hia_county_pop(),
                              "<br><strong>Emergency Department vists:</strong>", hia_vals()),
                  label=paste("Click for", hia_county_name()," county Health Impact Assessment"),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "12px",
                                              direction = "auto")
                  )  %>% 
      
      {if (layer_name=="layer_1" & updated_date==Sys.Date())
        addPolygons(., data = analyzed_plumes,
                    group = "Visible Smoke Plumes",
                    popup = paste("<b>Analyzed:</b>", analyzed_plumes$X1,
                                  "<br><b>Satellite:</b>", analyzed_plumes$Satellite,
                                  "<br><b>Density:</b>", analyzed_plumes$Density, "~&#181;</span>g/m<sup>3</sup>",
                                  "<br><b>Details:</b> www.ospo.noaa.gov/Products/land/hms.html"),
                    fillColor = "Gray",
                    stroke = FALSE,
                    label = "Smoke plume drawn by HMS analyst"
                    ) else .} %>%
      
      
      addCircleMarkers(data = fire_locations, 
                       lat = fire_locations$latitude,
                       lng = fire_locations$longitude, 
                       color = ~fire_pal(type),
                       radius = ~sqrt(area/100), 
                       fill=F, 
                       weight = 0.5,
                       group="Fire Locations",
                       label=paste0("Type: ",fire_locations$type, " | Area: ", round(fire_locations$area))) %>% 
      
      # add layer control, but omit Analyzed Plumes if the "tomorrow" input is chosen by user
      {if (layer_name=="layer_1" & updated_date==Sys.Date()) addLayersControl(
        .,
        overlayGroups = c("Fire Locations", 
                          "Forecasted Smoke Concentration",
                          "Visible Smoke Plumes",
                          "Emergency Dept. Visits"),
        options = layersControlOptions(collapsed = F))
        else addLayersControl(.,
          overlayGroups = c("Fire Locations",
                            "Forecasted Smoke Concentration",
                            "Emergency Dept. Visits"),
          options = layersControlOptions(collapsed = F))
        }%>%
      
      # Set default hidden groups 
      hideGroup(group=c("Forecasted Smoke Concentration", "Visible Smoke Plumes", "Emergency Dept. Visits"))
    
    
  }) # end reactive layer
}) # end server function



# launch shiny app (this is necessary for running on server)
shinyApp(ui = ui, server = server)


