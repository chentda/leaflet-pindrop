library(shiny)   # Web Framework
library(leaflet) # Interactive Maps

ui <- fluidPage(
  leafletOutput("map", width = "100%", height = "700px"),
  br(),
  br(),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles(options = providerTileOptions(noWrap = TRUE))
  })
  
  # Null reactive object for the coordinates of the marker pindrop
  clicked_marker <- reactiveValues(clickedMarker = NULL)
  
  observeEvent(input$map_click, {
    click <- input$map_click
    click_lat <- click$lat
    click_lng <- click$lng
    
    # Adding clicked on coordinates of marker to reactive object
    clicked_marker$clickedMarker <- c(click)
    
    leafletProxy("map") %>% 
      clearMarkers() %>% 
      addMarkers(lng = click_lng, lat = click_lat, options = markerOptions(draggable = TRUE))
  })  
  
  observeEvent(input$map_marker_mouseout, {
    
    # Replaces reactive coordinates of marker after being dragged
    clicked_marker$clickedMarker <- c(input$map_marker_mouseout)
    
    
  })
  
  output$out <- renderPrint({
    
    validate(need(clicked_marker$clickedMarker, FALSE))
    
    paste('Latitude: ', clicked_marker$clickedMarker$lat, '| Longitude: ', clicked_marker$clickedMarker$lng)
    
  })
  
}

shinyApp(ui, server)