#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(ggthemes)

#I make sure the data I need is available: 
master <- read_csv("master_file.csv", 
                   col_names = TRUE,
                   cols(
                     state_name = col_character(),
                     county_name = col_character(),
                     aco_benes = col_double(),
                     year = col_double(),
                     geometry = col_character()
                   ))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring ACO Participation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
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
     master %>%
       ggplot(aes(x = aco_benes, fill = "red")) +
       geom_histogram(bins = input$bins) +
       scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000), 
                     labels = c("10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
       labs(x = "ACO beneficiaries in county (Log base 10)",
            y = "Number of counties",
            title = "The count of ACO beneficiaires by county",
            subtitle = "There is large variation in ACO participation across counties",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist() +
       theme(legend.position = "none")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

