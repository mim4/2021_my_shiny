#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tidyverse)
require("shinyjs")
require(ggplot2)

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
    plotOutput(outputId = "plot", click = "plot_click"),
    tableOutput("info")
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
                           fluidRow(
                               h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
                               column(2,
                                      div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
                               ),
                               column(10,
                                      sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
                               )
                           )
                       ), # sidebarPanel
                       mainPanel(
                           tabsetPanel(type = "tabs",
                                   tabPanel("Plot", plotOutput("histPlot")),
                                   tabPanel("Summary", verbatimTextOutput("histSummary")),
                                   tabPanel("Table", tableOutput("histTable"))
                       )
                       )
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
         
), #  tabPanel
useShinyjs()
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
    
    output$info = renderTable({
        nearPoints(msleep 
                   %>% select(name, bodywt,  sleep_total, sleep_rem, sleep_cycle ), 
                   input$plot_click, threshold = 10, maxpoints = 1,
                   addDist = TRUE)
        
        # to show just the coordinates paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
         # to show all the info: toString(input$plot_click)
    
    })
    
    samples <- reactive({
        dist <- eval(parse(text=paste(input$dist)))
        dist(input$n_sample)
    })
    
 
    
    observe(if(input$auto_bins) disable("n_bins") else enable("n_bins"))
    
    output$histPlot <- renderPlot(
        hist(samples(), main="Random Generation",col = "royalblue", 
             breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
    )
    output$histSummary <- renderPrint(summary(samples()))
    output$histTable <- renderTable(samples())

   
}

# Run the application 
shinyApp(ui = ui, server = server)
