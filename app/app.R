# Source global variables and functions
source("app/global.R")

# Source all modules
source("app/modules/filters.R")
source("app/modules/analysis.R")
source("app/modules/heatmap.R")
source("app/modules/map.R")
source("app/modules/data_table.R")

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
    
    # Add filters module UI
    filtersUI("filters", data)
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              analysisUI("analysis")
      ),
      
      tabItem(tabName = "heatmap",
              heatmapUI("heatmap")
      ),
      
      tabItem(tabName = "map",
              mapUI("map")
      ),
      
      tabItem(tabName = "data",
              dataTableUI("data_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dark mode toggle
  observeEvent(input$dark_mode, {
    shinyjs::toggleClass(selector = "body", class = "dark-mode")
  })
  
  # Initialize filters module and get filtered data
  filters <- filtersServer("filters", data)
  
  # Initialize other modules with filtered data
  analysisServer("analysis", filters$filtered_data, filters$commodity)
  heatmapServer("heatmap", filters$filtered_data, filters$commodity)
  mapServer("map", filters$filtered_data)
  dataTableServer("data_table", filters$filtered_data)
}

# Run the application
shinyApp(ui = ui, server = server) 
