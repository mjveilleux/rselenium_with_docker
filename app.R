
#Shiny app for forecasting oil prices




library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = 'Forecasting Oil Prices'),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
        column(width = 4,
               box(
                   title = "Model Specification", width = NULL, solidHeader = TRUE, status = "warning",
                   "Box content"
               )
               )
        
    )
))

server <- function(input, output) { }

shinyApp(ui, server)