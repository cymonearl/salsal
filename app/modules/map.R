library(shiny)
library(leaflet)
library(dplyr)

mapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("price_map"), height = 600)
  )
}

mapServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    output$price_map <- renderLeaflet({
      req(filtered_data())
      
      df <- filtered_data() %>%
        group_by(admin1, market, latitude, longitude) %>%
        summarise(Avg_Price = mean(price, na.rm = TRUE), .groups = "drop")
      
      leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(
          lat = ~latitude,
          lng = ~longitude,
          radius = ~sqrt(Avg_Price)/2,
          color = "#E69F00",
          fillOpacity = 0.8,
          label = ~paste(market, "-", round(Avg_Price, 2), "PHP"),
          popup = ~paste("<b>", market, "</b><br>",
                        "Region:", admin1, "<br>",
                        "Avg Price:", round(Avg_Price, 2), "PHP")
        )
    })
  })
} 