install.packages("bs4Dash")
library(bs4Dash)

  lt <- as.POSIXlt(Sys.Date())
  lt$year <- lt$year - 30
  minAllowableDate <- as.Date(lt)

ui <- dashboardPage(
help = NULL,
fullscreen = TRUE,

dashboardHeader( title = dashboardBrand(
    "Philippine Food Trends", color = "primary"
    )
),
  
dashboardSidebar( 
    sidebarMenu(
        id = "tabs",
        menuItem("Home", 
        tabName = "home", 
        icon = icon("house")
        ),

        menuItem("Price Analysis", 
        tabName = "reports", 
        icon = icon("chart-line")
        ),

        menuItem("Price Heatmap",
        tabName = "heatmap",
        icon = icon("fire")
        ),

        menuItem("Geographic View",
        tabName = "geographic",
        icon = icon("map")
        ),

        menuItem("About",
        tabName = "about",
        icon = icon("circle-info")
        ),

        selectInput("category", "Food Category:", choices = c("what", "is", "this")),
        dateRangeInput("dates", "Date Range:", start = minAllowableDate , end =  Sys.Date())
    )
),
  
dashboardBody(
    tabItems(
      tabItem(tabName = "home", 
                box(
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    h2("Welcome!"),
                    p("Tracking trends, inflation, and affordability", 
                    "of key food items across the Philippines."),
                    collapsible = FALSE,
                    elevation = 1    # Box shadow (0-5)
                ),

                fluidRow(

                    userBox(
                        title = userDescription(
                            title = "Cymon Earl A. Galzote",
                            subtitle = "Data Scientist",
                            image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",  # User image
                            type = 2,
                        ),
                        p("Greetings, I'm Cymon you can call me Mon. My partner and I made this dashing",
                        "dashboard as our Learning Evidence."),
                        p("I hope you like it!"),
                        status = "purple",
                        collapsible = FALSE
                    ),
                    userBox(
                        title = userDescription(
                            title = "Christian John Ed Rosal",
                            subtitle = "Data Scientist",
                            image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",  # User image
                            type = 2,
                        ),

                        p("Maayong Adlaw, My name is Christian, Me and my partner crafted this delightful",
                        "dashboard for our Learning Evidence."),
                        p("Thank you so much!"),
                        status = "purple",
                        collapsible = FALSE
                    )
                )
            ),
            
        tabItem(tabName = "reports", 
            h2("Price Analysis")
            ),

        tabItem(tabName = "heatmap",
            h2("Heatmap Price")
        ),

        tabItem(tabName = "geographic",
            h2("Geographic View")
        ),

        tabItem(tabName = "about",
            h2("About")
        )
    )
),
  
  title = "My BS4 Dashboard"
)
server <- function(input, output, session) {

}

shinyApp(ui, server)
