# ------------------------------------------------------------------------------
# Title: UI script for smoke forecaster shiny app
# Author: Ryan Gan and Steve Brey
# Date Created: 6/17/17
# R Version 3.3.3 
# ------------------------------------------------------------------------------

# load libraries ----
library(shiny)
library(leaflet)
library(raster)

# read net cdf file of forecasts from 06/08/2017 ----
# brick or stack works
smk_brick <- brick("./smoke_dispersion_v2.nc")
# find the max range
max_pm <- max(summary(smk_brick)[5,])

smk_brick <- calc(smk_brick, fun=function(x){
  x[x > 200] <- 200; return(x)
})

# define color gradient for layer ----
pal2 <- colorBin(c("#F0F2F0", "#000c40"), domain = c(0,max_pm), bin = c(0:10),
  pretty=T, na.color = "transparent")

pal2

pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,200),
                    na.color = "transparent")

# I need to convert date and time
min_date

# set up shiny layout
ui <- fluidPage(
  leafletOutput("map"),
  sliderInput(inputId = "time", 
              label = "Date:Time", 
              min =1, max = 192,
              value = 0, step = 1)
)


# server section that will eventually go in it's own script
server <- function(input, output, session) {
  
  # add base leaflet map
  output$map <- renderLeaflet({
   map <-  leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) %>% 
      addLegend(pal=pal, values=c(0,max_pm), title = "Smoke ug/m^3")
  map  
    
  }) #
  
  # add interactive raster layer
  observeEvent(input$time,{
    
    time_index_string <- paste0("X", input$time)
    index <- as.numeric(input$time)
    r <- reactive({smk_brick[[index]]})
    
    leafletProxy(mapId="map") %>%
      clearImages() %>%
      addRasterImage(r(), colors = pal, opacity = 0.3, project = F) 
    
    })
}

shinyApp(ui, server)



