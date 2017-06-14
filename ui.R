library(shiny)
library(leaflet)
library(raster)

max(smk_brick)
# define color gradient for layer ----
pal <- colorNumeric(c("#F0F2F0", "#000c40"), domain = c(0,600), 
                    na.color = "transparent")

# read net cdf file of forecasts from 06/08/2017
smk_brick <- stack("./smoke_dispersion_v2.nc")

r_test <- smk_brick[[180]]

# smaller brick
small_brick <- smk_brick[[1:10]]


ui <- fluidPage(
  leafletOutput("map"),
  sliderInput(inputId = "time", 
              label = "Date", 
              min =1, max = 10,
              value = 1, step = 1)
)

server <- function(input, output, session) {
  
  # add base leaflet map
  output$map <- renderLeaflet({
   map <-  leaflet() %>% 
      # call map layer
      addTiles() %>% 
      # set bounds of map
      fitBounds(lng1=-100, lat1=50, lng2=-90, lat2=25) 
  map  
    
  }) #
  
  # add interactive raster brick
  observeEvent(input$time,{
    
    # reactive raster brick
    print(input$time %in% names(small_brick))
    
    time_index_string <- paste0("X", input$time)
    index <- as.numeric(input$time)
    r <- reactive({small_brick[[index]]})
    #r <- raster(xmn=-2.8, xmx=-2.79, ymn=54.04, ymx=54.05, nrows=30, ncols=30) 
    
    leafletProxy(mapId="map") %>%
      clearImages() %>%
     
      addRasterImage(r_test, colors = pal, opacity = 0.5, project = T) #%>%
      #addLegend(pal=pal, values=r, title = "Smoke ug/m^3")
    
    })
}

shinyApp(ui, server)



