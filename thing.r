library(shiny)

ui <- fluidPage(
  tags$style(
    HTML("
      body {
        background-image: url('bg.jpg') !important;
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }
    ")
  ),
  h1("Test Background")
)

server <- function(input, output) {}

shinyApp(ui, server)
