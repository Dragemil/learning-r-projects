#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("plotStartStops.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Jersey City bike rental analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
                sliderInput("hour",
                            "Choose hours:",
                            min = 0,
                            max = 23,
                            value = c(0, 23)),
                sliderInput("month",
                            "Choose month:",
                            min = 1,
                            max = 12,
                            value = 6)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Map", 
                                 plotlyOutput("mapPlot", height = "500%")),
                        tabPanel("Top Stations", 
                                 plotlyOutput("topPlot")),
                        tabPanel("Freeriders", 
                                 plotlyOutput("freeriders")))
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderPlotly({
        plotlyStart(input$hour[1], input$hour[2], input$month)
    })
    output$topPlot <- renderPlotly({
        plotlyTopStations(input$hour[1], input$hour[2], input$month)
    })
    output$freeriders <- renderPlotly({
        plotlyFreeriders(input$hour[1], input$hour[2], input$month)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
