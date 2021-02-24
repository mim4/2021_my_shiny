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

list_choices <-  unique(msleep$vore)
list_choices = list_choices[!is.na(list_choices)]
names(list_choices) = paste0(list_choices,"vore")

# Define UI for application that draws a histogram
ui <- navbarPage( "ShinyApp",
    tabPanel("msleep",
    fluidPage(
    sidebarLayout(
        sidebarPanel( 
    selectInput("select", label= h3("Plot by type of alimentation"),
                choices=character(0),
                selected=1)
    ),
    mainPanel(
    h3("Plots"),
    plotOutput(outputId = "plot")
    )
    )
    
)),
tabPanel("References",
         includeMarkdown("references.md")
) #  titlePanel
) # navbarPage


col_scale <- scale_colour_discrete(limits = list_choices)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    updateSelectInput(session, "select",
                      choices = list_choices,
                      selected = tail(list_choices, 1)
    )
    
    output$plot = renderPlot({
        ggplot(msleep %>% filter(vore==input$select)
               , aes(bodywt, sleep_total, colour = vore)) +
            scale_x_log10() +
            col_scale +
            geom_point()
    })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
