library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

heatmapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Price Heatmap Controls",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        selectInput(ns("heatmap_type"), "Heatmap Type:",
                   choices = c("Absolute Prices", "Price Deviation")),
        sliderInput(ns("heatmap_resolution"), "Time Resolution:",
                   min = 1, max = 12, value = 3, step = 1,
                   post = " months")
      ),
      box(
        plotOutput(ns("price_heatmap")), 
        width = 12
      )
    )
  )
}

heatmapServer <- function(id, filtered_data, commodity) {
  moduleServer(id, function(input, output, session) {
    
    output$price_heatmap <- renderPlot({
      req(filtered_data(), input$heatmap_type, input$heatmap_resolution)
      
      df <- filtered_data() %>%
        mutate(time_group = floor_date(date, paste(input$heatmap_resolution, "months"))) %>%
        group_by(admin1, time_group) %>%
        summarise(
          avg_price = mean(price, na.rm = TRUE),
          price_deviation = (mean(price) - mean(price)) / sd(price),
          .groups = "drop"
        )
      
      gg <- ggplot(df, aes(x = time_group, y = admin1)) +
        geom_tile(aes(fill = if(input$heatmap_type == "Absolute Prices") avg_price else price_deviation), 
                 color = "white") +
        scale_fill_gradient2(
          low = "#2166ac",
          mid = "#f7f7f7",
          high = "#b2182b",
          midpoint = 0,
          name = if(input$heatmap_type == "Absolute Prices") "Average Price (PHP)" else "Price Deviation (Z-score)"
        ) +
        labs(title = paste(input$heatmap_type, "Heatmap for", commodity()),
             x = "Time Period", y = "Region") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.background = element_rect(fill = "transparent", color = NA))
      
      gg + scale_x_date(date_labels = "%b %Y")
    })
  })
} 