#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggthemes)
library(tidyverse)

source(file = "simule_one_simple_pop.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Population dynamic"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("nb_pop",
                        "Number of populations:",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("r",
                        "Growth rate:",
                        min = 0.2,
                        max = 2,
                        value = 1.2),
            sliderInput("K",
                        "Carrying capacity:",
                        min = 10,
                        max = 1000,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # get parameters input$par from ui.R
        growth_rate    <- input$r
        carrying_cap <- input$K
        nb_of_population <- input$nb_pop

        # plot the dynamic for isolated populations
        plot_isolated_pop(parametre = c(growth_rate, carrying_cap), nb_of_pop=nb_of_population)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
