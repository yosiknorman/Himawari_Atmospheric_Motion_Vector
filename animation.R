library(shiny)

#User interface (UI) settings
ui <- fluidPage(leafletOutput("m.dynamic"),
                absolutePanel(top = 10,
                              right = 10,
                              draggable = TRUE,
                              sliderInput("range",
                                          "Time of data collection:",
                                          min = min(sp.lines.df@data$w.date),
                                          max = max(sp.lines.df@data$w.date),
                                          value = min(sp.lines.df@data$w.date),
                                          step = 4,
                                          animate=TRUE)))

#Name @coords slot of SpatialLinesDataFrame: 'lng' and 'lat'
#task necessary for 'observer' within 'server' function
for (i in c(1:max(sp.lines.df@data$id))) {
  colnames(sp.lines.df@lines[[i]]@Lines[[1]]@coords) <- c("lng","lat")
}

#Server logic
server <- function(input, output){
  filteredData <- reactive({
    sp.lines.df[sp.lines.df@data$w.date == input$range[1],]
  })
  output$m.dynamic <- renderLeaflet({
    leaflet(sp.lines.df) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addLegend("bottomright",pal = pal, values = ~w.speed, title = "Wind speed <br> (km/h)", opacity = 0.9) %>%
      fitBounds(sp.lines.df@bbox[1,1], sp.lines.df@bbox[2,1], sp.lines.df@bbox[1,2], sp.lines.df@bbox[2,2])
  })
  observe({
    leafletProxy("m.dynamic", data = filteredData()) %>%
      clearShapes() %>%
      addPolylines(color = ~pal(w.speed), opacity=1, weigh = 3, popup = labels)
  })
}

# Complete app with UI and server components
shinyApp(ui, server)