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
library(shinyWidgets)
library(ggthemes)
library(tidyverse)
library(knitr)

source(file = "simule_isolated_pop.R")
source(file = "simule_with_migration.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    chooseSliderSkin("Flat"),
    

    navbarPage("Gull Population Dynamic",
      ## Onglet 1 : About the model
      tabPanel("About the model", 
               # Render the Markdown file with MathJax
               withMathJax(),
               uiOutput("markdownOutput")
               ),
      
      ## Onglet 2 : graphic for isolated populations
      tabPanel("Isolated populations",
       
        
        # Sidebar with a slider input
          sidebarPanel(
            sliderInput("nb_pop",
                        "Number of populations:",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("r",
                        "Growth rate:",
                        min = -2,
                        max = 2,
                        value = 1.2),
            sliderInput("K",
                        "Carrying capacity:",
                        min = 10,
                        max = 1000,
                        value = 100),
            sliderInput("N0",
                        "Initial population size:",
                        min = 0,
                        max = 200,
                        value = 10),
            actionButton("reset", "Reset")
          ),
          
          # Plot dynamic for isolated population 
          mainPanel(
            plotOutput("distPlot")
          )
      ),
      
      ## Onglet 3 : graphic for connected population
      tabPanel("Metapopulation", 
               # Sidebar with a slider input
               sidebarPanel(
                 sliderInput("nb_pop_connected",
                             "Number of populations:",
                             min = 1,
                             max = 5,
                             value = 3),
                 sliderInput("N0_metapop", "Initial sizes",
                             min = 0,
                             max = 200,
                             value = 10),
                 selectInput("graph", "Choose a graph:", 
                             choices = c("Demographic curves", "Network"))
               ),
               
               # Plot dynamic for connected population 
               mainPanel(
                 h5("Here you can see what happened when migration is added. You can choose the 
                     value of the parameters with the sliders. ri, Ki refers to the growth rate 
                     and carrying capacity of population i. \n
                     Initial population size are the same for all population for more simplicity.
                    "),
                  uiOutput("sliders"),
                  checkboxInput("show_K", "Show carrying capacity", value = TRUE),
                 conditionalPanel(
                   condition = "input.graph == 'Demographic curves'",
                   plotOutput("demographicPlot")
                 ),
                 conditionalPanel(
                   condition = "input.graph == 'Network'",
                   visNetworkOutput("networkPlot"),
                   sliderInput("simulation_time", "Time", min = 1, max = 30, value = 1, step = 1),
               )
      )
)))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Onglet 1 Markdown
  output$markdownOutput <- renderUI(
  withMathJax({
    includeMarkdown("about.md")
  }))
    # Onglet 2
    ## Generate the plot for isolated populations
    output$distPlot <- renderPlot({
        # get parameters input$par from ui.R
        growth_rate    <- input$r
        carrying_cap <- input$K
        nb_of_population <- input$nb_pop
        initial_size <- input$N0

        # plot the dynamic for isolated populations
        plot_isolated_pop(parametre = c(growth_rate, carrying_cap, initial_size), nb_of_pop=nb_of_population)
    })
    
    ## reset button
    observeEvent(input$reset, {
      updateSliderInput(inputId = "nb_pop", value = 5)
      updateSliderInput(inputId = "r", value = 1.2)
      updateSliderInput(inputId = "K", value = 100)
      updateSliderInput(inputId = "N0", value = 10)
    })
    
    # Onglet 3
    ## Generate the plot for connected populations
    output$demographicPlot <- renderPlot({
      # get parameters input$par from ui.R
      nb_of_population <- input$nb_pop_connected
      growth_rate <- c(input$r1, input$r2, input$r3, input$r4, input$r5)
      growth_rate <- growth_rate[1:nb_of_population]
      carrying_cap <- c(input$K1, input$K2, input$K3, input$K4, input$K5)
      carrying_cap <- carrying_cap[1:nb_of_population]
      initial_size <- rep(input$N0_metapop, nb_of_population)
      # initial_size <- initial_size[1:nb_of_population]
      plot_connected_pop(parametre = list(r=growth_rate, K=carrying_cap, N0=initial_size), 
                           input$show_K)
      
    })
    output$networkPlot <- renderVisNetwork({
      # get parameters input$par from ui.R
      nb_of_population <- input$nb_pop_connected
      growth_rate <- c(input$r1, input$r2, input$r3, input$r4, input$r5)
      growth_rate <- growth_rate[1:nb_of_population]
      carrying_cap <- c(input$K1, input$K2, input$K3, input$K4, input$K5)
      carrying_cap <- carrying_cap[1:nb_of_population]
      initial_size <- rep(input$N0_metapop, nb_of_population)
      # initial_size <- initial_size[1:nb_of_population]
        # Network
      res = simulate_n_metapop(parametre= list(r=growth_rate, K=carrying_cap, N0=initial_size), 
                                 input$show_K)
      plot_network(res, date =  input$simulation_time)
    })
    
    ## Adjust the number of slider to the number of pop
    observeEvent(input$nb_pop_connected, {
      output$sliders = renderUI({
        tagList(
          # Sliders for r
          lapply(1:input$nb_pop_connected, function(x) {
            column(
              width = 4,
              sliderInput(paste0("r", x), paste0("r", x),
                          min = 0,
                          max = 2,
                          value = 1,
                          step = 0.1)
            )
          }),
          # Sliders for K
          lapply(1:input$nb_pop_connected, function(x) {
            column(
              width = 4, 
              sliderInput(paste0("K", x), paste0("K", x),
                          min = 10,
                          max = 1000,
                          value = 100)
            )
          }),
          # Sliders for N0
          # lapply(1:input$nb_pop_connected, function(x) {
          #   column(
          #     width = 4, 
          #     sliderInput(paste0("N0", x), paste("Initial pop size", x),
          #                 min = 0,
          #                 max = 200,
          #                 value = 10)
          #   )
          # })
        )
      })
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
