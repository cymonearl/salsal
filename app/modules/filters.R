library(shiny)

filtersUI <- function(id, data) {
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("region"), "Select Region:", 
                  choices = sort(unique(data$admin1)), 
                  multiple = TRUE,
                  options = list(placeholder = 'Select regions',
                                closeAfterSelect = TRUE)),
    actionButton(ns("clear_regions"), "Clear Selection"),
    selectInput(ns("category"), "Food Category:", 
               choices = sort(unique(data$category_group))),
    selectInput(ns("commodity"), "Select Commodity:", 
               choices = sort(unique(data$commodity))),
    dateRangeInput(ns("dates"), "Date Range:", 
                  start = min(data$date), 
                  end = max(data$date)),
    selectInput(ns("pricetype"), "Price Type:", 
               choices = c("Retail", "Wholesale"))
  )
}

filtersServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update commodity choices based on category
    observe({
      updateSelectInput(session, "commodity",
                       choices = sort(unique(data$commodity[data$category_group == input$category])))
    })
    
    # Region selection clearing
    observeEvent(input$clear_regions, {
      updateSelectizeInput(session, "region", selected = character(0))
    })
    
    # Return filtered data
    filtered_data <- reactive({
      req(input$commodity, input$dates, input$pricetype)
      
      df <- data %>%
        filter(date >= input$dates[1],
               date <= input$dates[2],
               commodity == input$commodity,
               pricetype == input$pricetype)
      
      if(!is.null(input$region)) {
        df <- df %>% filter(admin1 %in% input$region)
      }
      df
    })
    
    # Return all inputs as a list
    return(list(
      filtered_data = filtered_data,
      region = reactive(input$region),
      category = reactive(input$category),
      commodity = reactive(input$commodity),
      dates = reactive(input$dates),
      pricetype = reactive(input$pricetype)
    ))
  })
} 