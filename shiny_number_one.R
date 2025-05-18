library(shiny)
library(shinydashboard)
library(shinyjs)  # For dark mode toggle
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)

# Read and clean data (from previous solution)
data <- read.csv("wfp_food_prices_phl.csv", 
                 comment.char = "#",
                 strip.white = TRUE,
                 stringsAsFactors = FALSE) %>%
  rename_with(~ gsub("[.#+]", "", .x)) %>% 
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label = TRUE),
    across(c(latitude, longitude, price, usdprice), as.numeric)
  )

# CSS for dark mode
css <- "
.body {
  background-color: white;
  color: black;
}

.dark-mode {
  background-color: #2d2d2d;
  color: #dfdfdf;
}

.dark-mode .box {
  background-color: #404040;
  border-color: #606060;
}

.dark-mode .content-wrapper {
  background-color: #2d2d2d;
}
"

ui <- dashboardPage(
  dashboardHeader(
    title = "PH Food Prices",
    tags$li(class = "dropdown", actionLink("dark_mode", "🌓 Toggle Theme"))
  ),
  
  dashboardSidebar(
    useShinyjs(),
    tags$style(css),
    
    sidebarMenu(
      menuItem("Price Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Geographic View", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    
    selectizeInput("region", "Select Region:", 
                   choices = sort(unique(data$admin1)), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select regions',
                                  closeAfterSelect = TRUE)),
    actionButton("clear_regions", "Clear Selection"),
    selectInput("commodity", "Select Commodity:", choices = sort(unique(data$commodity))),
    dateRangeInput("dates", "Date Range:", 
                   start = min(data$date), 
                   end = max(data$date)),
    selectInput("pricetype", "Price Type:", 
                choices = c("Retail", "Wholesale"))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                box(plotOutput("price_trend"), width = 12)
              ),
              fluidRow(
                box(plotOutput("price_boxplot"), width = 12)
              )
      ),
      
      tabItem(tabName = "map",
              leafletOutput("price_map", height = 600)
      ),
      
      tabItem(tabName = "data",
              DT::dataTableOutput("raw_data")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dark mode toggle
  observeEvent(input$dark_mode, {
    shinyjs::toggleClass(selector = "body", class = "dark-mode")
  })
  
  # Region selection clearing
  observeEvent(input$clear_regions, {
    updateSelectizeInput(session, "region", selected = character(0))
  })
  
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
  
  output$price_trend <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = date, y = price, color = admin1)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Price Trend for", input$commodity),
           x = "Date", y = "Price (PHP)") +
      theme_minimal() +
      scale_color_viridis_d() +
      theme(plot.background = element_rect(fill = "transparent", color = NA))
  })
  
  output$price_boxplot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = admin1, y = price, fill = admin1)) +
      geom_boxplot() +
      labs(title = paste("Price Distribution for", input$commodity),
           x = "Region", y = "Price (PHP)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d() +
      theme(plot.background = element_rect(fill = "transparent", color = NA))
  })
  
  output$price_map <- renderLeaflet({
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
  
  output$raw_data <- DT::renderDataTable({
    filtered_data() %>%
      select(date, admin1, market, commodity, pricetype, price, usdprice)
  })
}

shinyApp(ui, server)