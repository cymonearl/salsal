library(shiny)
library(ggplot2)

analysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(plotOutput(ns("price_trend")), width = 12),
      box(plotOutput(ns("price_boxplot")), width = 12)
    )
  )
}

analysisServer <- function(id, filtered_data, commodity) {
  moduleServer(id, function(input, output, session) {
    
    # Price trend plot
    output$price_trend <- renderPlot({
      req(filtered_data())
      
      ggplot(filtered_data(), aes(x = date, y = price, color = admin1)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(title = paste("Price Trend for", commodity()),
             x = "Date", y = "Price (PHP)") +
        theme_minimal() +
        scale_color_viridis_d() +
        theme(plot.background = element_rect(fill = "transparent", color = NA))
    })
    
    # Price distribution boxplot
    output$price_boxplot <- renderPlot({
      req(filtered_data())
      
      ggplot(filtered_data(), aes(x = admin1, y = price, fill = admin1)) +
        geom_boxplot() +
        labs(title = paste("Price Distribution for", commodity()),
             x = "Region", y = "Price (PHP)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d() +
        theme(plot.background = element_rect(fill = "transparent", color = NA))
    })
  })
} 