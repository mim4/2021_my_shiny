#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("This is a new Shiny App"),
    includeMarkdown("references.md"),
    h3("Plots"),
    plotOutput(outputId = "plot")
    
)
data = runif(20)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot = renderPlot({
        plot(data)
    })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
