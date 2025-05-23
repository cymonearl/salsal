library(shiny)
library(bslib)  # For Bootstrap theming
library(fontawesome)  # Optional: ensures FA icons render correctly

# Define a list of Bootswatch themes
themes <- c("cosmo", "darkly", "flatly", "cyborg")

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cosmo"),  # Initial theme

  navbarPage(
    title = "My Shiny App with Theme Switcher",
    theme = bs_theme(bootswatch = "cosmo"),

    # Map Tab
    tabPanel(tagList(icon("map"), "Map"),
      h2("Map"),
      sidebarPanel(
        selectInput("commodity", "Commodity:",
                    choices = c("All", "NCR", "Luzon", "Visayas", "Mindanao"),
                    selected = "All"
        ),
        sliderInput("date", "Select a Year:",
                    min = as.Date("1999-01-01"),
                    max = as.Date("2025-12-31"),
                    value = c(as.Date("1999-01-01"), as.Date("2025-12-31")),
                    timeFormat = "%Y"
        ),
        p("Select a commodity and a year to view the",
          "price trends for that commodity in that year.")
      )
    ),

    # Price Trends Tab
    tabPanel(tagList(icon("chart-line"), "Analysis"),
      sidebarPanel(
        selectInput("commodity", "Commodity:",
                    choices = c("All"),
                    selected = "All"
        ),
        selectInput("region", "Region:",
                    choices = c("All", "NCR", "Luzon", "Visayas", "Mindanao"),
                    selected = "All"
        ),
        sliderInput("date", "Select a Year:",
                    min = as.Date("1999-01-01"),
                    max = as.Date("2025-12-31"),
                    value = c(as.Date("1999-01-01"), as.Date("2025-12-31")),
                    timeFormat = "%Y"
        )
      )
    ),

    # Predictive Analysis
    tabPanel(tagList(icon("chart-line"), "Predictive Analysis"),
      h2("Predictive Analysis")
    ),

    # Reports
    tabPanel(tagList(icon("file-alt"), "Reports"),
      h2("Reports")
    ),

    # About
    tabPanel(tagList(icon("info-circle"), "About"),
      h2("About")
    ),

    # Theme Switcher Tab
    tabPanel(tagList(icon("palette"), "Theme"),
      fluidRow(
        column(12, align = "center",
          h4("Change Theme"),
          selectInput("theme", "Select Theme:",
                      choices = themes,
                      selected = themes[1]
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Observe theme selection and apply it dynamically
  observeEvent(input$theme, {
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
}

# Run app
shinyApp(ui, server)