# Enhanced Philippine Rice Price Analytics - R Shiny Application
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)
library(RColorBrewer)
library(forecast)
library(tseries)
library(corrplot)
library(VIM)
library(psych)
library(broom)
library(sf) # Add this for handling spatial data
library(geojsonio) # Add this for reading GeoJSON files

# Load GeoJSON as sf object
ph_regions_sf <- sf::st_read("ph.json", quiet = TRUE)

# Map GeoJSON region names to your data's region names
# KEY = Name as it appears in GeoJSON "name" field
# VALUE = Name as it appears in price data "Region" column
region_name_map <- c(
  "National Capital Region" = "Metro Manila",  # No match in data - will be NA
  "Ilocos" = "Ilocos Region",
  "Cagayan Valley" = "Cagayan Valley",        
  "Central Luzon" = "Central Luzon",          
  "Bicol" = "Bicol Region",                   # No match in data - will be NA
  "Western Visayas" = "Western Visayas",      
  "Central Visayas" = "Central Visayas",      
  "Eastern Visayas" = "Eastern Visayas",      
  "Zamboanga Peninsula" = "Zamboanga Peninsula",
  "Northern Mindanao" = "Northern Mindanao",  
  "Davao" = "Davao Region",
  "Soccsksargen" = "SOCCSKSARGEN",            
  "Caraga" = "CARAGA",                        
  "Autonomous Region in Muslim Mindanao" = "BARMM", 
  "Cordillera Administrative Region" = "CAR",       
  "Calabarzon" = "CALABARZON",                
  "Mimaropa" = "MIMAROPA"                     
)
# Add a column for joining: App_Region
# Use "name" field instead of "NAME_1"
ph_regions_sf$App_Region <- dplyr::recode(ph_regions_sf$name, !!!region_name_map)

# Read and prepare the data
data <- read.csv("combined_country_and_regional_price_data_COMPLETE.csv", stringsAsFactors = FALSE)

# Data preprocessing
data$Year <- as.numeric(data$Year)
data$Farmgate <- as.numeric(data$Farmgate)
data$Wholesale <- as.numeric(data$Wholesale)
data$Retail <- as.numeric(data$Retail)
data <- data[data$Region != "Philippines",]

# Calculate additional metrics
data$Farmgate_to_Wholesale_Margin <- data$Wholesale - data$Farmgate
data$Wholesale_to_Retail_Margin <- data$Retail - data$Wholesale
data$Total_Margin <- data$Retail - data$Farmgate

# Define color palette
colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6")

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Philippine Rice Price Analytics",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Regional Analysis", tabName = "regional", icon = icon("map-marked-alt")),
      menuItem("Choropleth Map", tabName = "choropleth", icon = icon("globe-asia")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("clock")),
      menuItem("Price Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Margin Analysis", tabName = "margins", icon = icon("coins")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("search")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-area")),
      menuItem("Statistical Tests", tabName = "statistics", icon = icon("calculator")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    br(),
    
    # Filters
    div(style = "padding: 15px;",
        h4("Filters", style = "color: white; margin-bottom: 15px;"),
        
        pickerInput(
          inputId = "region_filter",
          label = "Select Regions:",
          choices = unique(data$Region),
          selected = unique(data$Region),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Select All",
            deselectAllText = "Deselect All",
            liveSearch = TRUE
          )
        ),
        
        sliderInput(
          inputId = "year_range",
          label = "Year Range:",
          min = min(data$Year, na.rm = TRUE),
          max = max(data$Year, na.rm = TRUE),
          value = c(2010, max(data$Year, na.rm = TRUE)),
          step = 1,
          sep = ""
        ),
        
        radioGroupButtons(
          inputId = "price_type",
          label = "Price Type:",
          choices = c("Farmgate", "Wholesale", "Retail"),
          selected = "Retail",
          status = "primary",
          size = "sm"
        )
    )
  ),

  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .value-box-icon {
          font-size: 60px !important;
        }
        .small-box h3 {
          font-size: 2.2em !important;
        }
        .leaflet-container {
          height: 400px !important;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("avg_farmgate", width = 3),
          valueBoxOutput("avg_wholesale", width = 3),
          valueBoxOutput("avg_retail", width = 3),
          valueBoxOutput("total_regions", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Price Trends Over Time", status = "primary", solidHeader = TRUE,
            width = 8, height = 500,
            withSpinner(plotlyOutput("overview_trend_plot"))
          ),
          box(
            title = "Regional Price Distribution", status = "info", solidHeader = TRUE,
            width = 4, height = 500,
            withSpinner(plotlyOutput("price_distribution"))
          )
        ),
        
        fluidRow(
          box(
            title = "Top 5 Highest Price Regions (Latest Year)", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("top_regions_table"))
          ),
          box(
            title = "Price Growth Rate by Region", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("growth_rate_plot"))
          )
        )
      ),
      
      # Regional Analysis Tab
      tabItem(
        tabName = "regional",
        fluidRow(
          box(
            title = "Regional Price Comparison", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            withSpinner(plotlyOutput("regional_comparison_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Regional Statistics Summary", status = "info", solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("regional_stats_table"))
          ),
          box(
            title = "Price Volatility by Region", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("volatility_plot"))
          )
        )
      ),
      
      # Choropleth Map Tab
      tabItem(
        tabName = "choropleth",
        fluidRow(
          box(
            title = "Interactive Choropleth Map", status = "primary", solidHeader = TRUE,
            width = 8, height = 500,
            withSpinner(leafletOutput("choropleth_map"))
          ),
          box(
            title = "Map Controls", status = "info", solidHeader = TRUE,
            width = 4, height = 500,
            sliderInput("map_year", "Select Year:", 
                       min = min(data$Year, na.rm = TRUE), 
                       max = max(data$Year, na.rm = TRUE),
                       value = max(data$Year, na.rm = TRUE),
                       step = 1, sep = ""),
            radioButtons("map_price_type", "Price Type:",
                        choices = c("Farmgate", "Wholesale", "Retail"),
                        selected = "Retail"),
            hr(),
            h4("Legend"),
            p("Darker colors represent higher prices"),
            verbatimTextOutput("map_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Regional Ranking", status = "success", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("map_ranking_table"))
          )
        )
      ),
      
      # Time Series Tab
      tabItem(
        tabName = "timeseries",
        fluidRow(
          box(
            title = "Interactive Time Series Analysis", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            withSpinner(plotlyOutput("timeseries_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Year-over-Year Growth", status = "info", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("yoy_growth_plot"))
          ),
          box(
            title = "Seasonal Patterns", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("seasonal_plot"))
          )
        )
      ),
      
      # Price Comparison Tab
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = "Price Level Comparison (All Types)", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            withSpinner(plotlyOutput("price_comparison_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Price Correlation Matrix", status = "info", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("correlation_plot"))
          ),
          box(
            title = "Price Spread Analysis", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("spread_analysis_plot"))
          )
        )
      ),
      
      # Margin Analysis Tab
      tabItem(
        tabName = "margins",
        fluidRow(
          box(
            title = "Margin Evolution Over Time", status = "primary", solidHeader = TRUE,
            width = 12, height = 500,
            withSpinner(plotlyOutput("margin_evolution_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Average Margins by Region", status = "info", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("margin_by_region_plot"))
          ),
          box(
            title = "Margin Statistics", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("margin_stats_table"))
          )
        )
      ),
      
      # Data Exploration Tab
      tabItem(
        tabName = "exploration",
        fluidRow(
          box(
            title = "Data Summary", status = "primary", solidHeader = TRUE,
            width = 6,
            withSpinner(verbatimTextOutput("data_summary"))
          ),
          box(
            title = "Missing Data Analysis", status = "info", solidHeader = TRUE,
            width = 6,
            withSpinner(plotOutput("missing_data_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution Analysis", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("distribution_plot"))
          ),
          box(
            title = "Outlier Detection", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("outlier_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Correlation Heatmap", status = "primary", solidHeader = TRUE,
            width = 12,
            withSpinner(plotOutput("correlation_heatmap"))
          )
        )
      ),
      
      # Forecasting Tab
      tabItem(
        tabName = "forecasting",
        fluidRow(
          box(
            title = "Forecasting Controls", status = "info", solidHeader = TRUE,
            width = 3,
            selectInput("forecast_region", "Select Region:",
                       choices = unique(data$Region),
                       selected = "Philippines"),
            radioButtons("forecast_price_type", "Price Type:",
                        choices = c("Farmgate", "Wholesale", "Retail"),
                        selected = "Retail"),
            numericInput("forecast_periods", "Forecast Periods:",
                        value = 5, min = 1, max = 10),
            actionButton("run_forecast", "Run Forecast", 
                        class = "btn-primary")
          ),
          box(
            title = "Price Forecast", status = "primary", solidHeader = TRUE,
            width = 9,
            withSpinner(plotlyOutput("forecast_plot"))
          )
        ),
        
        fluidRow(
          box(
            title = "Forecast Accuracy Metrics", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("forecast_accuracy"))
          ),
          box(
            title = "Model Diagnostics", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(plotOutput("forecast_diagnostics"))
          )
        )
      ),
      
      # Statistical Tests Tab
      tabItem(
        tabName = "statistics",
        fluidRow(
          box(
            title = "Test Configuration", status = "info", solidHeader = TRUE,
            width = 3,
            selectInput("test_type", "Select Test:",
                       choices = c("Normality Test" = "normality",
                                  "Stationarity Test" = "stationarity",
                                  "Price Difference Test" = "price_diff",
                                  "Regional Comparison" = "regional_comp")),
            conditionalPanel(
              condition = "input.test_type == 'price_diff' || input.test_type == 'regional_comp'",
              selectInput("test_region1", "Region 1:",
                         choices = unique(data$Region),
                         selected = "Philippines"),
              selectInput("test_region2", "Region 2:",
                         choices = unique(data$Region),
                         selected = "Central Luzon")
            ),
            actionButton("run_test", "Run Statistical Test",
                        class = "btn-primary")
          ),
          box(
            title = "Test Results", status = "primary", solidHeader = TRUE,
            width = 9,
            withSpinner(verbatimTextOutput("test_results"))
          )
        ),
        
        fluidRow(
          box(
            title = "Descriptive Statistics", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("descriptive_stats"))
          ),
          box(
            title = "Statistical Visualization", status = "warning", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("statistical_plot"))
          )
        )
      ),
      
      # Data Table Tab
      tabItem(
        tabName = "datatable",
        fluidRow(
          box(
            title = "Complete Dataset", status = "primary", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("complete_data_table"))
          )
        )
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Application", status = "primary", solidHeader = TRUE,
            width = 8,
            h3("Philippine Rice Price Analytics Dashboard"),
            p("This comprehensive dashboard provides in-depth analysis of rice price data across different regions in the Philippines. 
              The application offers multiple analytical perspectives including time series analysis, regional comparisons, 
              statistical testing, and forecasting capabilities."),
            
            h4("Key Features:"),
            tags$ul(
              tags$li("Interactive visualizations with real-time filtering"),
              tags$li("Choropleth mapping for geographical price analysis"),
              tags$li("Statistical testing and hypothesis validation"),
              tags$li("Time series forecasting with accuracy metrics"),
              tags$li("Comprehensive data exploration tools"),
              tags$li("Margin analysis across the supply chain")
            ),
            
            h4("Data Sources:"),
            p("The data includes farmgate, wholesale, and retail prices across various Philippine regions, 
              spanning multiple years to enable comprehensive trend analysis."),
            
            h4("Methodology:"),
            p("The analysis employs various statistical methods including:"),
            tags$ul(
              tags$li("Time series decomposition and forecasting (ARIMA, ETS)"),
              tags$li("Statistical hypothesis testing (t-tests, normality tests)"),
              tags$li("Correlation and regression analysis"),
              tags$li("Outlier detection and data quality assessment")
            )
          ),
          box(
            title = "Technical Information", status = "info", solidHeader = TRUE,
            width = 4,
            h4("Built With:"),
            tags$ul(
              tags$li("R Shiny Framework"),
              tags$li("Plotly for Interactive Visualization"),
              tags$li("Leaflet for Mapping"),
              tags$li("Forecast Package for Time Series"),
              tags$li("Various Statistical Packages")
            ),
            
            h4("Version Information:"),
            p("Version: 2.0"),
            p("Last Updated: ", Sys.Date()),
            
            h4("Contact:"),
            p(
              a("Cymon Earl A. Galzote", href = "mailto:ceagalzote01323@usep.edu.ph",
                target = "_blank"), br(),
              a("Christian John Ed Rosal", href = "mailto:cjejrosal00566@usep.edu.ph",
                target = "_blank")
            ),
            
            h4("Disclaimer:"),
            p("This tool is for analytical purposes. Users should validate 
              findings with official sources before making policy decisions.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data based on filters
  filtered_data <- reactive({
    data %>%
      filter(
        Region %in% input$region_filter,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # Value boxes for overview
  output$avg_farmgate <- renderValueBox({
    avg_val <- round(mean(filtered_data()$Farmgate, na.rm = TRUE), 2)
    valueBox(
      value = paste("₱", avg_val),
      subtitle = "Average Farmgate Price",
      icon = icon("seedling"),
      color = "green"
    )
  })
  
  output$avg_wholesale <- renderValueBox({
    avg_val <- round(mean(filtered_data()$Wholesale, na.rm = TRUE), 2)
    valueBox(
      value = paste("₱", avg_val),
      subtitle = "Average Wholesale Price",
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  output$avg_retail <- renderValueBox({
    avg_val <- round(mean(filtered_data()$Retail, na.rm = TRUE), 2)
    valueBox(
      value = paste("₱", avg_val),
      subtitle = "Average Retail Price",
      icon = icon("shopping-cart"),
      color = "red"
    )
  })
  
  output$total_regions <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$Region)),
      subtitle = "Regions Selected",
      icon = icon("map"),
      color = "yellow"
    )
  })
  
  # Overview trend plot
  output$overview_trend_plot <- renderPlotly({
    price_col <- input$price_type
    
    trend_data <- filtered_data() %>%
      group_by(Year) %>%
      summarise(
        avg_price = mean(get(price_col), na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(trend_data, aes(x = Year, y = avg_price)) +
      geom_line(color = colors[1], size = 1.2) +
      geom_point(color = colors[1], size = 3) +
      labs(
        title = paste("Average", price_col, "Price Trend"),
        x = "Year",
        y = paste(price_col, "Price (₱)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # Price distribution plot
  output$price_distribution <- renderPlotly({
    price_col <- input$price_type
    latest_year <- max(filtered_data()$Year, na.rm = TRUE)
    
    dist_data <- filtered_data() %>%
      filter(Year == latest_year) %>%
      arrange(desc(get(price_col)))
    
    p <- ggplot(dist_data, aes(x = reorder(Region, get(price_col)), y = get(price_col))) +
      geom_col(fill = colors[2], alpha = 0.8) +
      coord_flip() +
      labs(
        title = paste(price_col, "Prices by Region (", latest_year, ")"),
        x = "Region",
        y = paste(price_col, "Price (₱)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Top regions table
  output$top_regions_table <- DT::renderDataTable({
    price_col <- input$price_type
    latest_year <- max(filtered_data()$Year, na.rm = TRUE)
    
    top_regions <- filtered_data() %>%
      filter(Year == latest_year) %>%
      arrange(desc(get(price_col))) %>%
      select(Region, !!price_col) %>%
      head(5)
    
    colnames(top_regions) <- c("Region", paste(price_col, "Price (₱)"))
    
    DT::datatable(
      top_regions,
      options = list(
        dom = 't',
        pageLength = 5,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2, digits = 2)
  })
  
  # Growth rate plot
  output$growth_rate_plot <- renderPlotly({
    price_col <- input$price_type
    
    growth_data <- filtered_data() %>%
      group_by(Region) %>%
      arrange(Year) %>%
      summarise(
        start_price = first(get(price_col)),
        end_price = last(get(price_col)),
        years = n(),
        .groups = 'drop'
      ) %>%
      filter(years > 1) %>%
      mutate(
        growth_rate = ((end_price / start_price) ^ (1 / (years - 1)) - 1) * 100
      ) %>%
      arrange(desc(growth_rate))
    
    p <- ggplot(growth_data, aes(x = reorder(Region, growth_rate), y = growth_rate)) +
      geom_col(fill = colors[3], alpha = 0.8) +
      coord_flip() +
      labs(
        title = paste("Annual Growth Rate -", price_col, "Price"),
        x = "Region",
        y = "Annual Growth Rate (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Regional comparison plot
  output$regional_comparison_plot <- renderPlotly({
    price_col <- input$price_type
    
    p <- ggplot(filtered_data(), aes(x = Year, y = get(price_col), color = Region)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Regional", price_col, "Price Comparison"),
        x = "Year",
        y = paste(price_col, "Price (₱)"),
        color = "Region"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("colour", "x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # Regional statistics table
  output$regional_stats_table <- DT::renderDataTable({
    price_col <- input$price_type
    
    stats_data <- filtered_data() %>%
      group_by(Region) %>%
      summarise(
        Mean = round(mean(get(price_col), na.rm = TRUE), 2),
        Median = round(median(get(price_col), na.rm = TRUE), 2),
        SD = round(sd(get(price_col), na.rm = TRUE), 2),
        Min = round(min(get(price_col), na.rm = TRUE), 2),
        Max = round(max(get(price_col), na.rm = TRUE), 2),
        .groups = 'drop'
      )
    
    DT::datatable(
      stats_data,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2:6, digits = 2)
  })
  
  # Volatility plot
  output$volatility_plot <- renderPlotly({
    price_col <- input$price_type
    
    volatility_data <- filtered_data() %>%
      group_by(Region) %>%
      summarise(
        cv = sd(get(price_col), na.rm = TRUE) / mean(get(price_col), na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>%
      arrange(desc(cv))
    
    p <- ggplot(volatility_data, aes(x = reorder(Region, cv), y = cv)) +
      geom_col(fill = colors[4], alpha = 0.8) +
      coord_flip() +
      labs(
        title = paste("Price Volatility -", price_col),
        x = "Region",
        y = "Coefficient of Variation (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 9)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Choropleth Map
  output$choropleth_map <- renderLeaflet({
    req(ph_regions_sf, input$map_year, input$map_price_type)
    
    selected_year <- input$map_year
    selected_price_type <- input$map_price_type
    
    # Aggregate price data for the selected year and price type
    map_price_data <- data %>%
      filter(Year == selected_year) %>%
      group_by(Region) %>%
      summarise(Price_Value = mean(get(selected_price_type), na.rm = TRUE), .groups = 'drop')
    
    # Join spatial data with price data
    map_sf_joined <- ph_regions_sf %>%
      left_join(map_price_data, by = c("App_Region" = "Region"))
    
    # Create color palette
    price_domain <- map_sf_joined$Price_Value[!is.na(map_sf_joined$Price_Value)]
    
    if(length(price_domain) == 0) {
      # If no price data available, show basic map
      leaflet(data = ph_regions_sf) %>%
        addTiles() %>%
        addPolygons(fillColor = "lightgrey", weight = 1, color = "white", fillOpacity = 0.5,
                    label = ~sprintf("<strong>%s</strong><br/>No data", name)) %>%
        setView(lng = 121.7740, lat = 12.8797, zoom = 6)
    } else {
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = price_domain,
        na.color = "#bdbdbd"
      )
      
      leaflet(data = map_sf_joined) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(Price_Value),
          weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = ~sprintf(
            "<strong>%s</strong><br/>%s: ₱%s<br/>Year: %s",
            name,
            selected_price_type,
            ifelse(is.na(Price_Value), "No Data", formatC(Price_Value, format = "f", digits = 2)),
            selected_year
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~Price_Value,
          title = paste(selected_price_type, "Price (₱)"),
          position = "bottomright",
          labFormat = labelFormat(prefix = "₱")
        ) %>%
        setView(lng = 121.7740, lat = 12.8797, zoom = 6)
    }
  })
  
  output$map_info <- renderText({
    paste("Showing", input$map_price_type, "prices for", input$map_year)
  })
  
  output$map_ranking_table <- DT::renderDataTable({
    ranking_data <- data %>%
      filter(Year == input$map_year) %>%
      arrange(desc(get(input$map_price_type))) %>%
      select(Region, !!input$map_price_type) %>%
      mutate(Rank = row_number())
    
    colnames(ranking_data) <- c("Region", paste(input$map_price_type, "Price (₱)"), "Rank")
    
    DT::datatable(
      ranking_data,
      options = list(pageLength = 15),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2, digits = 2)
  })
  
  # Time series plot
  output$timeseries_plot <- renderPlotly({
    price_col <- input$price_type
    
    p <- ggplot(filtered_data(), aes(x = Year, y = get(price_col))) +
      geom_line(aes(color = Region), size = 1) +
      geom_point(aes(color = Region), size = 2, alpha = 0.7) +
      labs(
        title = paste("Time Series Analysis -", price_col, "Price"),
        x = "Year",
        y = paste(price_col, "Price (₱)"),
        color = "Region"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("colour", "x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  # Year-over-year growth plot
  output$yoy_growth_plot <- renderPlotly({
    price_col <- input$price_type
    
    yoy_data <- filtered_data() %>%
      group_by(Region) %>%
      arrange(Year) %>%
      mutate(
        yoy_growth = (get(price_col) / lag(get(price_col)) - 1) * 100
      ) %>%
      filter(!is.na(yoy_growth))
    
    avg_yoy <- yoy_data %>%
      group_by(Year) %>%
      summarise(avg_growth = mean(yoy_growth, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(avg_yoy, aes(x = Year, y = avg_growth)) +
      geom_line(color = colors[1], size = 1.2) +
      geom_point(color = colors[1], size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      labs(
        title = paste("Average Year-over-Year Growth -", price_col),
        x = "Year",
        y = "YoY Growth (%)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Price comparison plot (all types)
  output$price_comparison_plot <- renderPlotly({
    comparison_data <- filtered_data() %>%
      select(Year, Region, Farmgate, Wholesale, Retail) %>%
      tidyr::gather(key = "Price_Type", value = "Price", -Year, -Region)
    
    p <- ggplot(comparison_data, aes(x = Year, y = Price, color = Price_Type)) +
      geom_line(size = 1) +
      facet_wrap(~Region, scales = "free_y") +
      labs(
        title = "Price Comparison: Farmgate vs Wholesale vs Retail",
        x = "Year",
        y = "Price (₱)",
        color = "Price Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = c("colour", "x", "y"))
  })
  
  # Margin evolution plot
  output$margin_evolution_plot <- renderPlotly({
    margin_data <- filtered_data() %>%
      select(Year, Region, Farmgate_to_Wholesale_Margin, Wholesale_to_Retail_Margin, Total_Margin) %>%
      tidyr::gather(key = "Margin_Type", value = "Margin", -Year, -Region)
    
    avg_margins <- margin_data %>%
      group_by(Year, Margin_Type) %>%
      summarise(avg_margin = mean(Margin, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(avg_margins, aes(x = Year, y = avg_margin, color = Margin_Type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Evolution of Price Margins Over Time",
        x = "Year",
        y = "Average Margin (₱)",
        color = "Margin Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = c("colour", "x", "y"))
  })
  
  # Margin by region plot
  output$margin_by_region_plot <- renderPlotly({
    margin_by_region <- filtered_data() %>%
      group_by(Region) %>%
      summarise(
        Farmgate_to_Wholesale = mean(Farmgate_to_Wholesale_Margin, na.rm = TRUE),
        Wholesale_to_Retail = mean(Wholesale_to_Retail_Margin, na.rm = TRUE),
        Total_Margin = mean(Total_Margin, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      tidyr::gather(key = "Margin_Type", value = "Margin", -Region)
    
    p <- ggplot(margin_by_region, aes(x = reorder(Region, Margin), y = Margin, fill = Margin_Type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Average Margins by Region",
        x = "Region",
        y = "Average Margin (₱)",
        fill = "Margin Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 9)
      )
    
    ggplotly(p, tooltip = c("fill", "x", "y"))
  })
  
  # Margin statistics table
  output$margin_stats_table <- DT::renderDataTable({
    margin_stats <- filtered_data() %>%
      summarise(
        `Farmgate to Wholesale - Mean` = round(mean(Farmgate_to_Wholesale_Margin, na.rm = TRUE), 2),
        `Farmgate to Wholesale - SD` = round(sd(Farmgate_to_Wholesale_Margin, na.rm = TRUE), 2),
        `Wholesale to Retail - Mean` = round(mean(Wholesale_to_Retail_Margin, na.rm = TRUE), 2),
        `Wholesale to Retail - SD` = round(sd(Wholesale_to_Retail_Margin, na.rm = TRUE), 2),
        `Total Margin - Mean` = round(mean(Total_Margin, na.rm = TRUE), 2),
        `Total Margin - SD` = round(sd(Total_Margin, na.rm = TRUE), 2)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value")
    
    DT::datatable(
      margin_stats,
      options = list(
        dom = 't',
        pageLength = 10,
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = 2, digits = 2)
  })
  
  # Data Exploration Section
  output$data_summary <- renderPrint({
    summary_data <- filtered_data() %>%
      select(Year, Farmgate, Wholesale, Retail, Farmgate_to_Wholesale_Margin, 
             Wholesale_to_Retail_Margin, Total_Margin)
    
    summary(summary_data)
  })
  
  output$missing_data_plot <- renderPlot({
    VIM::aggr(filtered_data() %>% select(Farmgate, Wholesale, Retail), 
              col = c('navyblue', 'red'), 
              numbers = TRUE, 
              sortVars = TRUE)
  })
  
  output$distribution_plot <- renderPlotly({
    dist_data <- filtered_data() %>%
      select(Farmgate, Wholesale, Retail) %>%
      tidyr::gather(key = "Price_Type", value = "Price")
    
    p <- ggplot(dist_data, aes(x = Price, fill = Price_Type)) +
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      facet_wrap(~Price_Type, scales = "free") +
      labs(
        title = "Price Distribution by Type",
        x = "Price (₱)",
        y = "Frequency"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$outlier_plot <- renderPlotly({
    outlier_data <- filtered_data() %>%
      select(Region, Farmgate, Wholesale, Retail) %>%
      tidyr::gather(key = "Price_Type", value = "Price", -Region)
    
    p <- ggplot(outlier_data, aes(x = Price_Type, y = Price)) +
      geom_boxplot(aes(fill = Price_Type), alpha = 0.7) +
      labs(
        title = "Outlier Detection - Price Distribution",
        x = "Price Type",
        y = "Price (₱)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$correlation_heatmap <- renderPlot({
    cor_data <- filtered_data() %>%
      select(Farmgate, Wholesale, Retail, Farmgate_to_Wholesale_Margin, 
             Wholesale_to_Retail_Margin, Total_Margin) %>%
      cor(use = "complete.obs")
    
    corrplot(cor_data, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black")
  })
  
  # Forecasting Section
  forecast_model <- eventReactive(input$run_forecast, {
    req(input$forecast_region, input$forecast_price_type)
    
    forecast_data <- data %>%
      filter(Region == input$forecast_region) %>%
      arrange(Year) %>%
      select(Year, !!input$forecast_price_type)
    
    if(nrow(forecast_data) < 3) return(NULL)
    
    ts_data <- ts(forecast_data[[input$forecast_price_type]], 
                  start = min(forecast_data$Year), 
                  frequency = 1)
    
    # Try different models and select best
    models <- list()
    try({
      models$auto_arima <- auto.arima(ts_data)
    }, silent = TRUE)
    
    try({
      models$ets <- ets(ts_data)
    }, silent = TRUE)
    
    # Select best model based on AIC
    if(length(models) > 0) {
      best_model <- models[[which.min(sapply(models, AIC))]]
      forecast_result <- forecast(best_model, h = input$forecast_periods)
      
      list(
        model = best_model,
        forecast = forecast_result,
        original_data = ts_data
      )
    } else {
      NULL
    }
  })
  
  output$forecast_plot <- renderPlotly({
    forecast_result <- forecast_model()
    if(is.null(forecast_result)) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "No forecast available"), 
                  size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Prepare data for plotting
    historical_years <- time(forecast_result$original_data)
    historical_values <- as.numeric(forecast_result$original_data)
    
    forecast_years <- time(forecast_result$forecast$mean)
    forecast_values <- as.numeric(forecast_result$forecast$mean)
    forecast_lower <- as.numeric(forecast_result$forecast$lower[,2])
    forecast_upper <- as.numeric(forecast_result$forecast$upper[,2])
    
    plot_data <- data.frame(
      Year = c(historical_years, forecast_years),
      Price = c(historical_values, forecast_values),
      Type = c(rep("Historical", length(historical_values)), 
               rep("Forecast", length(forecast_values)))
    )
    
    forecast_ribbon <- data.frame(
      Year = forecast_years,
      Lower = forecast_lower,
      Upper = forecast_upper
    )
    
    p <- ggplot(plot_data, aes(x = Year, y = Price)) +
      geom_line(aes(color = Type), size = 1.2) +
      geom_point(aes(color = Type), size = 2) +
      geom_ribbon(data = forecast_ribbon, 
                  aes(x = Year, ymin = Lower, ymax = Upper), 
                  alpha = 0.3, fill = "blue") +
      labs(
        title = paste("Price Forecast -", input$forecast_region, "-", input$forecast_price_type),
        x = "Year",
        y = "Price (₱)",
        color = "Data Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_accuracy <- DT::renderDataTable({
    forecast_result <- forecast_model()
    if(is.null(forecast_result)) {
      return(DT::datatable(data.frame(Metric = "No data", Value = "N/A")))
    }
    
    accuracy_metrics <- accuracy(forecast_result$model)
    accuracy_df <- data.frame(
      Metric = rownames(accuracy_metrics),
      Value = round(accuracy_metrics[,1], 4)
    )
    
    DT::datatable(accuracy_df, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$forecast_diagnostics <- renderPlot({
    forecast_result <- forecast_model()
    if(is.null(forecast_result)) {
      plot.new()
      text(0.5, 0.5, "No diagnostics available", cex = 1.5)
      return()
    }
    
    checkresiduals(forecast_result$model)
  })
  
  # Statistical Tests Section
  test_results <- eventReactive(input$run_test, {
    req(input$test_type)
    
    results <- list()
    
    if(input$test_type == "normality") {
      price_data <- filtered_data()[[input$price_type]]
      price_data <- price_data[!is.na(price_data)]
      
      if(length(price_data) > 3) {
        shapiro_test <- shapiro.test(price_data)
        results$shapiro <- shapiro_test
        
        if(length(price_data) > 50) {
          ks_test <- ks.test(price_data, "pnorm", mean(price_data), sd(price_data))
          results$ks <- ks_test
        }
      }
      
    } else if(input$test_type == "stationarity") {
      ts_data <- filtered_data() %>%
        filter(Region == input$region_filter[1]) %>%
        arrange(Year) %>%
        pull(!!input$price_type)
      
      if(length(ts_data) > 3) {
        adf_test <- adf.test(ts_data)
        results$adf <- adf_test
        
        kpss_test <- kpss.test(ts_data)
        results$kpss <- kpss_test
      }
      
    } else if(input$test_type == "price_diff") {
      req(input$test_region1, input$test_region2)
      
      region1_data <- data %>%
        filter(Region == input$test_region1) %>%
        pull(!!input$price_type)
      
      region2_data <- data %>%
        filter(Region == input$test_region2) %>%
        pull(!!input$price_type)
      
      if(length(region1_data) > 3 && length(region2_data) > 3) {
        t_test <- t.test(region1_data, region2_data)
        results$t_test <- t_test
        
        var_test <- var.test(region1_data, region2_data)
        results$var_test <- var_test
      }
      
    } else if(input$test_type == "regional_comp") {
      req(input$test_region1, input$test_region2)
      
      region1_data <- data %>%
        filter(Region == input$test_region1) %>%
        select(Year, !!input$price_type) %>%
        arrange(Year)
      
      region2_data <- data %>%
        filter(Region == input$test_region2) %>%
        select(Year, !!input$price_type) %>%
        arrange(Year)
      
      # Merge by year for paired comparison
      merged_data <- merge(region1_data, region2_data, by = "Year", suffixes = c("_1", "_2"))
      
      if(nrow(merged_data) > 3) {
        paired_t_test <- t.test(merged_data[[paste0(input$price_type, "_1")]], 
                               merged_data[[paste0(input$price_type, "_2")]], 
                               paired = TRUE)
        results$paired_t_test <- paired_t_test
      }
    }
    
    results
  })
  
  output$test_results <- renderPrint({
    results <- test_results()
    
    if(length(results) == 0) {
      cat("No test results available. Please run a test.")
      return()
    }
    
    for(test_name in names(results)) {
      cat("=== ", toupper(gsub("_", " ", test_name)), " ===\n")
      print(results[[test_name]])
      cat("\n")
    }
  })
  
  output$descriptive_stats <- DT::renderDataTable({
    desc_stats <- filtered_data() %>%
      group_by(Region) %>%
      summarise(
        across(c(Farmgate, Wholesale, Retail), 
               list(mean = ~mean(.x, na.rm = TRUE),
                    median = ~median(.x, na.rm = TRUE),
                    sd = ~sd(.x, na.rm = TRUE),
                    min = ~min(.x, na.rm = TRUE),
                    max = ~max(.x, na.rm = TRUE)), 
               .names = "{.col}_{.fn}"),
        .groups = 'drop'
      )
    
    DT::datatable(desc_stats, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatRound(columns = 2:ncol(desc_stats), digits = 2)
  })
  
  output$statistical_plot <- renderPlotly({
    if(input$test_type == "normality") {
      price_data <- filtered_data()[[input$price_type]]
      price_data <- price_data[!is.na(price_data)]
      
      p <- ggplot(data.frame(x = price_data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, fill = "skyblue") +
        geom_density(color = "red", size = 1) +
        labs(title = "Distribution and Normality Check", 
             x = paste(input$price_type, "Price"), y = "Density") +
        theme_minimal()
      
    } else if(input$test_type == "price_diff" && !is.null(input$test_region1) && !is.null(input$test_region2)) {
      comp_data <- data %>%
        filter(Region %in% c(input$test_region1, input$test_region2)) %>%
        select(Region, !!input$price_type)
      
      p <- ggplot(comp_data, aes(x = Region, y = get(input$price_type), fill = Region)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Price Comparison Between Regions",
             x = "Region", y = paste(input$price_type, "Price")) +
        theme_minimal()
      
    } else {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Select appropriate test for visualization"), 
                  size = 6) +
        theme_void()
    }
    
    ggplotly(p)
  })
  
  # Complete data table
  output$complete_data_table <- DT::renderDataTable({
    display_data <- filtered_data() %>%
      arrange(desc(Year), Region)
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      DT::formatRound(columns = c("Farmgate", "Wholesale", "Retail", "Farmgate_to_Wholesale_Margin", 
                                  "Wholesale_to_Retail_Margin", "Total_Margin"), digits = 2)
  })
  
  # Additional plots for seasonal and correlation analysis
  output$seasonal_plot <- renderPlotly({
    # Since we don't have monthly data, we'll show price patterns by year ranges
    price_col <- input$price_type
    
    seasonal_data <- filtered_data() %>%
      mutate(
        Period = case_when(
          Year >= 1998 & Year <= 2002 ~ "1998-2002",
          Year >= 2003 & Year <= 2007 ~ "2003-2007",
          Year >= 2008 & Year <= 2012 ~ "2008-2012",
          Year >= 2013 & Year <= 2017 ~ "2013-2017",
          Year >= 2018 ~ "2018-2023",
          TRUE ~ "Other"
        )
      ) %>%
      group_by(Period, Region) %>%
      summarise(avg_price = mean(get(price_col), na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(seasonal_data, aes(x = Period, y = avg_price, fill = Period)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = paste("Price Patterns by Period -", price_col),
        x = "Period",
        y = paste("Average", price_col, "Price (₱)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$correlation_plot <- renderPlotly({
    cor_data <- filtered_data() %>%
      select(Farmgate, Wholesale, Retail) %>%
      cor(use = "complete.obs")
    
    # Convert correlation matrix to long format
    cor_long <- expand.grid(Var1 = rownames(cor_data), Var2 = colnames(cor_data))
    cor_long$value <- as.vector(cor_data)
    
    p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(
        title = "Price Correlation Matrix",
        x = "",
        y = "",
        fill = "Correlation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$spread_analysis_plot <- renderPlotly({
    spread_data <- filtered_data() %>%
      group_by(Year) %>%
      summarise(
        min_retail = min(Retail, na.rm = TRUE),
        max_retail = max(Retail, na.rm = TRUE),
        spread = max_retail - min_retail,
        .groups = 'drop'
      )
    
    p <- ggplot(spread_data, aes(x = Year, y = spread)) +
      geom_line(color = colors[5], size = 1.2) +
      geom_point(color = colors[5], size = 3) +
      labs(
        title = "Price Spread Analysis (Max - Min Retail Price)",
        x = "Year",
        y = "Price Spread (₱)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)