library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(DT)

# Read and clean data
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
  ) %>%
  # Add commodity categories
  mutate(
    category_group = case_when(
      grepl("Rice|Maize", commodity) ~ "Grains & Cereals",
      grepl("Meat|Chicken", commodity) ~ "Meat & Protein",
      grepl("Eggs", commodity) ~ "Eggs",
      grepl("Fish|Crab|Shrimp", commodity) ~ "Seafood",
      grepl("Banana|Bottle|Bitter|Mandarin|Pineapple|Papaya|Tomato|Coconut|Choko|Calamansi|Mango", commodity) ~ "Fruits",
      grepl("Bean|Vegetable|Cabbage|Carrot|Choko|Eggplant|Anchovies|Garlic|Ginger|Onion|Potato|Squash|Tomato|Water", commodity, ignore.case = TRUE) ~ "Vegetables & Legumes",
      TRUE ~ "Other Food Items"
    )
  )

# CSS for dark mode and heatmap
css <- HTML("
  /* Dark mode styles */
  body.dark-mode { background-color: #2d2d2d; color: #dfdfdf; }
  body.dark-mode .content-wrapper,
  body.dark-mode .main-sidebar,
  body.dark-mode .main-header .navbar,
  body.dark-mode .main-header .logo {
    background-color: #2d2d2d !important;
    color: #dfdfdf !important;
  }
  body.dark-mode .box {
    background-color: #404040;
    border-color: #606060;
    color: #dfdfdf;
  }
  body.dark-mode .selectize-input {
    background-color: #555 !important;
    color: #dfdfdf !important;
    border-color: #666 !important;
  }
  body.dark-mode .selectize-dropdown-content {
     background-color: #555 !important;
     color: #dfdfdf !important;
  }
   body.dark-mode .selectize-dropdown .active {
    background-color: #6a6a6a !important;
  }
  body.dark-mode .sidebar-menu > li > a { color: #dfdfdf; }
  body.dark-mode .sidebar-menu > li.active > a,
  body.dark-mode .sidebar-menu > li > a:hover {
    background: #404040;
    color: #FFF;
    border-left-color: #FFF;
  }
  body.dark-mode .shiny-input-container label { color: #dfdfdf; }
  body.dark-mode .form-control:not(.selectize-input) {
    background-color: #555;
    color: #dfdfdf;
    border-color: #666;
  }
   body.dark-mode .input-group-addon {
    background-color: #555;
    color: #dfdfdf;
    border-color: #666;
  }

  /* For DT table in dark mode */
  body.dark-mode .dataTables_wrapper .dataTables_length,
  body.dark-mode .dataTables_wrapper .dataTables_filter,
  body.dark-mode .dataTables_wrapper .dataTables_info,
  body.dark-mode .dataTables_wrapper .dataTables_processing,
  body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: #dfdfdf !important;
  }
  body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
    color: #888 !important;
  }
  body.dark-mode table.dataTable tbody tr {
    background-color: #404040;
  }
  body.dark-mode table.dataTable tbody tr:hover {
    background-color: #505050 !important;
  }
  body.dark-mode table.dataTable th, body.dark-mode table.dataTable td {
    color: #dfdfdf;
    border-color: #606060;
  }
   body.dark-mode table.dataTable thead th {
    border-bottom: 1px solid #606060;
  }

  /* Specific app styles */
  .datatables td {
    transition: all 0.3s ease;
  }
  .price-info {
    position: absolute;
    padding: 8px;
    background: #fff;
    border: 1px solid #ddd;
    border-radius: 5px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.2);
    pointer-events: none;
    z-index: 1000;
    color: #333;
  }
  body.dark-mode .price-info {
    background: #333;
    color: #dfdfdf;
    border-color: #555;
  }
")

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
      menuItem("Price Heatmap", tabName = "heatmap", icon = icon("fire")),
      menuItem("Geographic View", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    
    selectizeInput("region", "Select Region:", 
                   choices = sort(unique(data$admin1)), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select regions',
                                  closeAfterSelect = TRUE)),
    actionButton("clear_regions", "Clear Selection"),
    selectInput("category", "Food Category:", 
                choices = sort(unique(data$category_group))),
    selectInput("commodity", "Select Commodity:", 
                choices = sort(unique(data$commodity))),
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
                box(plotOutput("price_trend"), width = 12),
                box(plotOutput("price_boxplot"), width = 12)
              )
      ),
      
      # Replace the heatmap tab UI section with this corrected version
      tabItem(tabName = "heatmap",
              fluidRow(
                box(
                  width = 12,
                  title = "Price Heatmap Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput("heatmap_type", "Heatmap Type:",
                              choices = c("Absolute Prices", "Price Deviation")),
                  sliderInput("heatmap_resolution", "Time Resolution:",
                              min = 1, max = 12, value = 3, step = 1,
                              post = " months")
                ),
                box(
                  plotOutput("price_heatmap"), 
                  width = 12
                )
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
  
  # Update commodity choices based on category
  observe({
    updateSelectInput(session, "commodity",
                      choices = sort(unique(data$commodity[data$category_group == input$category])))
  })
  
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
  
  # Price trend plot
  output$price_trend <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = date, y = price, color = admin1)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Price Trend for", input$commodity),
           x = "Date", y = "Price (PHP)") +
      theme_minimal() +
      scale_color_viridis_d() +
      theme(plot.background = element_rect(fill = "transparent", color = NA))
  })
  
  # Price distribution boxplot
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
  
  # Price heatmap
  output$price_heatmap <- renderPlot({
    df <- filtered_data() %>%
      mutate(time_group = floor_date(date, paste(input$heatmap_resolution, "months"))) %>%
      group_by(admin1, time_group) %>%
      summarise(
        avg_price = mean(price, na.rm = TRUE),
        price_deviation = (mean(price) - mean(data$price[data$commodity == input$commodity])) / 
          sd(data$price[data$commodity == input$commodity]),
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
      labs(title = paste(input$heatmap_type, "Heatmap for", input$commodity),
           x = "Time Period", y = "Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.background = element_rect(fill = "transparent", color = NA))
    
    if(input$heatmap_type == "Absolute Prices") {
      gg + scale_x_date(date_labels = "%b %Y")
    } else {
      gg + scale_x_date(date_labels = "%b %Y")
    }
  })
  
  # Interactive map
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
  
  # Data table
  output$raw_data <- DT::renderDataTable({
    filtered_data() %>%
      select(date, admin1, market, category_group, commodity, pricetype, price, usdprice)
  })
}

shinyApp(ui, server)