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
tabPanel("Random generator",
         sidebarLayout(position = "right",
                       sidebarPanel(
                           selectInput("dist", label = h3("Select the distribution"), 
                                       choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                                       selected = 1),
                           sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                                       max = 100, value = 50),
                           sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                                       max = 50, value = 30)
                       ), # sidebarPanel
                       mainPanel(
                           plotOutput(outputId = "pulpo")
                       ) # mainPanel
         ) # sidebarLayout
), #  tabPanel
tabPanel("References",
         p(tags$button(class="btn btn-default", 
                       `data-toggle`="collapse", 
                       `data-target`="#hola",
                       "References")),
         div(class="collapse", id="hola",
             div(class="card card-body",
                 includeMarkdown("references.md")
             )),
         
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
    
    cmd = reactive(eval(parse(text=paste(input$dist,"(",input$n_sample,")"))));
    
    output$pulpo = renderPlot(hist(cmd()))

   
}

# Run the application 
shinyApp(ui = ui, server = server)
