ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "litera"), 
  titlePanel("Philippine Commodity Price Analysis Dashboard"),

  tags$style(HTML("
    /* Custom CSS Styles */
    body {
      min-height: 100vh;
      background-image: url('bg.jpg');
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
      background-attachment: fixed;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      position: relative;
      z-index: 1;
      margin: 0;
      padding: 0;
    }

    /* Semi-transparent white containers */
    .sidebarPanel, .mainPanel, .nav-tabs, .tab-content, .dataTables_wrapper {
      background-color: rgba(255, 255, 255, 0.65) !important;
      backdrop-filter: blur(1px);
      border-radius: 12px !important;
      box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1) !important;
      border: 1px solid rgba(255, 255, 255, 0.3) !important;
      position: relative;
      z-index: 2;
    }

    /* Improve text contrast */
    h1, h2, h3, h4, .dataTables_title {
      color: #2c3e50 !important;
      text-shadow: 1px 1px 2px rgba(255, 255, 255, 0.5);
    }

    /* Transparent table styling */
    .table {
      background-color: rgba(255, 255, 255, 0.8) !important;
    }

    /* Glassmorphism effect for tabs */
    .nav-tabs {
      background: rgba(255, 255, 255, 0.25) !important;d
      padding: 8px;
      border-radius: 8px;
    }

    /* Download button styling */
    .btn-default {
      background: linear-gradient(45deg, #7f7fd5, #86a8e7) !important;
      color: white !important;
      border: none !important;
      transition: transform 0.3s ease !important;
    }

    /* Sidebar styling */
    .sidebarPanel {
      background-color: rgba(255, 255, 255, 0.65) !important;
      border-right: 2px solid rgba(222, 226, 230, 0.8) !important;
      height: 90vh;
      overflow-y: auto;
    }
    
    /* Main panel header spacing */
    .mainPanel h3 {
      margin-top: 1.5rem;
      color: #2c3e50;
      border-bottom: 2px solid #3498db;
      padding-bottom: 0.5rem;
      margin: 10px;
    }
    
    /* Tab styling */
    .nav-tabs {
      margin-bottom: 1.5rem;
    }
    
    .nav-tabs .nav-item .nav-link.active {
      background-color: #3498db !important;
      color: white !important;
      border-color: #3498db;
    }
    
    /* Download button styling */
    .btn-default {
      background-color: #FFFBDE !important;
      color: white !important;
      margin: 5px 0;
      width: 100%;
    }
    
    .btn-default:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 15px rgba(127, 127, 213, 0.3) !important;
    }
    
    /* Plot containers */
    .leaflet-container, .plotly.html-widget {
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Data table styling */
    .dataTables_wrapper {
      border: 1px solid #ddd !important;
      border-radius: 6px;
      padding: 10px;
    }
    
    /* Warning messages */
    .shiny-output-error-validation {
      color: #c0392b;
      font-weight: bold;
      padding: 10px;
    }

    .row {
      margin: 20px;
    }

    /* Ensure the main container doesn't block the background */
    .container-fluid {
      background: transparent !important;
    }

    /* Make sure the fluid page doesn't add unwanted background */
    .fluid-page {
      background: transparent !important;
    }
  ")),

  sidebarLayout(
    sidebarPanel(
      width = 3, 
      h4("Data Filters"),
      selectInput("selected_regions", "Select Region(s):",
                  choices = if(exists("available_regions_from_data") && length(available_regions_from_data) > 0) available_regions_from_data else "Loading...",
                  selected = if (exists("available_regions_from_data") && "Philippines" %in% available_regions_from_data) "Philippines" else if(exists("available_regions_from_data") && length(available_regions_from_data) > 0) available_regions_from_data[1] else NULL,
                  multiple = TRUE),
      selectInput("selected_price_level", "Select Price Level:",
                  choices = if(exists("available_price_levels") && length(available_price_levels) > 0) available_price_levels else "Loading...",
                  selected = if(exists("available_price_levels") && length(available_price_levels) > 0) available_price_levels[1] else NULL),
      sliderInput("selected_year_range", "Select Year Range:",
                  min = if(exists("min_year_from_data")) min_year_from_data else 2000, 
                  max = if(exists("max_year_from_data")) max_year_from_data else 2023,
                  value = c(if(exists("max_year_from_data")) max(min_year_from_data, max_year_from_data - 10) else 2013, if(exists("max_year_from_data")) max_year_from_data else 2023), 
                  step = 1,
                  sep = ""),
      
      hr(),
      h4("Map Options"),
      selectInput("map_year", "Select Year for Map:",
                  choices = if(exists("min_year_from_data") && exists("max_year_from_data")) seq(min_year_from_data, max_year_from_data, by = 1) else 2023,
                  selected = if(exists("max_year_from_data")) max_year_from_data else 2023),
      
      hr(),
      h4("Forecast Options"),
      selectInput("forecast_region", "Select Region for Forecast:",
                  choices = if(exists("available_regions_from_data") && length(available_regions_from_data) > 0) available_regions_from_data else "Loading...",
                  selected = if (exists("available_regions_from_data") && "Philippines" %in% available_regions_from_data) "Philippines" else if(exists("available_regions_from_data") && length(available_regions_from_data) > 0) available_regions_from_data[1] else NULL),
      sliderInput("forecast_horizon", "Forecast Horizon (Years):",
                  min = 1, max = 5, value = 2, step = 1),
      
      hr(),
      h4("Downloads"),
      downloadButton("downloadPlot", "Download Current Plot (PNG)"),
      downloadButton("downloadData", "Download Filtered Data (CSV)")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Overview & Map",
          icon = icon("map"),
                 fluidRow(
                   column(12, h3("Choropleth Map of Prices")),
                   column(12, leafletOutput("choroplethMap", height = "500px")),
                   column(12, uiOutput("mapWarningUI")) 
                 ),
                 fluidRow(
                   column(12, h3("Time Series of Prices by Region")),
                   column(12, plotlyOutput("timeSeriesPlot", height = "450px"))
                 )
        ),
        tabPanel("Data Exploration",
          icon = icon("chart-simple"),
                 fluidRow(
                   column(12, h3("Summary Statistics")),
                   column(12, DTOutput("summaryTable"))
                 ),
                 fluidRow(
                   column(6, h3("Price Distribution (Selected Level)"),
                          plotlyOutput("distributionPlot")), 
                   column(6, h3("Correlation Matrix (Placeholder)"),
                          plotOutput("correlationMatrix")) 
                 )
        ),
        tabPanel("Forecasting",
          icon = icon("chart-line"),
                 fluidRow(
                   column(12, h3(textOutput("forecastTitle"))),
                   column(12, plotlyOutput("forecastPlot", height = "450px")), 
                   column(12, h4("Forecast Accuracy Metrics"), verbatimTextOutput("accuracyMetrics")),
                   column(12, h4("Residual Diagnostics (Plot)"), plotOutput("residualDiagnosticsPlot"))
                 )
        ),
        tabPanel("Statistical Tests",
          icon = icon("chart-area"),
                 fluidRow(
                   column(12, h3("ANOVA: Price Differences Across Regions")),
                   column(12, verbatimTextOutput("anovaResults"))
                 )
        ),
        tabPanel("About",
          icon = icon("circle-info"),
          style = "margin: 20px;",
                 h3("About This Application"),
                 p("This dashboard provides tools for analyzing commodity prices in the Philippines."),
                 p("Features from roadmap:"),
                 tags$ul(
                   tags$li("Map choropleth + tooltips (Implemented with Leaflet)"),
                   tags$li("Time series & comparison plots (Implemented with Plotly)"),
                   tags$li("Forecasting (ARIMA - Implemented)"),
                   tags$li("Download handlers (Implemented for plot and data)"),
                   tags$li("Error handling in outputs (Basic implementation with validate/need)"),
                   tags$li("Shiny modules (Structure allows for future refactoring)"),
                   tags$li("Custom visualization settings (Basic settings via inputs)"),
                   tags$li("Seasonal/Distribution plots (Histogram implemented)"),
                   tags$li("Styling (Using bslib for theming)"),
                   tags$li("Automated ARIMA parameter selection (auto.arima used)"),
                   tags$li("Interactive time series and forecast visualization with confidence intervals (Implemented)"),
                   tags$li("Accuracy metrics dashboard (Implemented for ARIMA)"),
                   tags$li("Residual diagnostics (Implemented for ARIMA)"),
                   tags$li("Dynamic Filtering (Implemented for all main plots/tables)")
                 ),
                 p("Data Source: User-provided `price_data.csv`"),
                 p("GeoJSON Source: User-provided `ph.json` (e.g., from simplemaps.com)"),
                 p("Developed by: AI Assistant")
        )
      )
    )
  )
)

shinyApp(ui, server)
