library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  observe({
    map$clearShapes()
    cities <- topCitiesInBounds()
    
    if (nrow(cities) == 0)
      return()
    
    map$addCircle(
      cities$Lat,
      cities$Long,
      sqrt(cities[[popCol()]]) * radiusFactor / max(5, input$map_zoom)^2,
      row.names(cities),
      list(
        weight=1.2,
        fill=TRUE,
        color='#4A9'
      )
    )
  })
  output$cityRelationship <- renderPlot({
    p <- ggplot
  })
})