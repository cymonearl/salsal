library(shiny)
library(DT)

dataTableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(ns("raw_data"))
  )
}

dataTableServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    output$raw_data <- DT::renderDataTable({
      req(filtered_data())
      
      filtered_data() %>%
        select(date, admin1, market, category_group, commodity, pricetype, price, usdprice)
    })
  })
} 