library(shiny)
library(leaflet)
library(raster)

max(smk_brick)
# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,600), 
                    na.color = "transparent")

# read net cdf file of forecasts from 06/08/2017
smk_brick <- brick("./smoke_dispersion_v2.nc")

r <- smk_brick[[1]]
length(smk_brick@data@names)


ui <- fluidPage(
  leafletOutput("map"),
  sliderInput("range", "Date", min =0, max = length(smk_brick@data@names),
              value = 0, step = 1)
)

server <- function(input, output, session) {
  
  # add base leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
       fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) 
  }) #
  
  # add interactive raster brick
  observe({
    # reactive raster brick
    r <- smk_brick[[input$range]]
    
    proxy <- leafletProxy("map") %>% 
      addRasterImage(r, colors = pal, opacity = 0.5, project = F) # %>%  
      #addLegend(pal=pal, values=r, title = "Smoke ug/m^3")
  
    })
}

shinyApp(ui, server)



