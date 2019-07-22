# load libraries ---------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal) # for read shapefile
library(stringr)

# Polygon color options 
polyOpacity <- 0.5
polyBorderOpacity <- .7

# fireIcons <- icons(
#   iconUrl = "http://thediscipleproject.net/wp-content/uploads/2013/07/fire-vector.png",
#   iconWidth = 17, 
#   iconHeight = 17
# )

pal_fire <- colorFactor(
  palette = c("red", "orange"),
  levels = c("WF", "RX")
)

# Set paths and load required data ---------------------------------------------

# read in smoke forecast shapefile ----
# define relative path to polygon file
##poly_path <- "./data/smk_poly"    ## Original path
poly_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/smk_poly" ## modified path

# # read bluesky forecast polygon for the two forecasted dates 
smk_forecast_1 <- readOGR(dsn = poly_path, layer = "smk_poly_1")
smk_forecast_2 <- readOGR(dsn = poly_path, layer = "smk_poly_2")

# read in hia estimate ----
##hia_path <- "./data/hia_poly"  # modified
hia_path <- "C:/Users/apddsouth/Documents/Smoke_Predictor/data/hia_poly"
hia_layer <- "hia_poly"

# hia polygon
county_hia <- readOGR(dsn = hia_path, layer = hia_layer)

# Current smoke conditions
latest_smoke <- readOGR(
  dsn="C:/Users/apddsouth/Documents/Smoke_Predictor/data/HMS", 
  layer="latest_smoke_display")

# Note 2017-12-29: Decided not to cap county population-wted pm, but I will need
# to reconcile cap of grid values polygon with this

# define color bin for smoke layer ----
# going with a bin since it will be easier to handle extreme colors
bin <- c(2, 10, 20, 30, 40, 50, 100, 250, 1000)
pal <- colorBin(palette=c("gray", "red", "purple"), 
                domain = c(0,1000), 
                bins = bin,
                na.color = "black") # "#F0F2F0", "#000c40"

map <- leaflet() %>% 
    
    # call map layer
    #addTiles() %>%  # DS: original basemap
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # DS: dark grey basemap
    
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
