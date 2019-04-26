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

aco_master <- read_rds("aco_master_file.rds") %>%
  mutate(qual_score = as.numeric(qual_score))

county_master <- read_rds("county_master_file.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring ACO Participation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("year",
                    "Select Program Year",
                    choices = c(2014, 2015, 2016, 2017),
                    selected = 2017,
                    multiple = FALSE,
                    selectize = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Background", verbatimTextOutput("summary"), plotOutput("yearbars")),
                    tabPanel("County Variation", plotOutput("distPlot")),
                    tabPanel("Entrace and Exit", plotOutput("enterbars"), plotOutput("exitbars"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$yearbars <- renderPlot({
     
     aco_master %>%
       ggplot(aes(x = year, fill = "blue")) +
       geom_bar() +
       labs(x = "Year",
            y = "Number of ACOs",
            title = "MSSP Participation by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "blue")
     
   })
   
   output$distPlot <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes(x = aco_benes, fill = "red")) +
       geom_density() +
       scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000), 
                     labels = c("10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
       scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5),
                          labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5")) +
       labs(x = "ACO beneficiaries in county (Log base 10)",
            y = "Density of Counties",
            title = "The count of ACO beneficiaires by county",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "light blue")
     
   })
   
   output$enterbars <- renderPlot({
     
     aco_master %>%
     filter(entered == 1) %>%
     ggplot(aes(x = year, fill = "blue")) +
     geom_bar() +
     labs(x = "Year",
          y = "Number of ACOs that Entered",
          title = "MSSP Entrance by Year",
          caption = "Data from the Center for Medicare and Medicaid Services") +
     theme_minimal() +
     theme(legend.position = "none") +
     scale_fill_manual(values = "green")
     
   })
   
   output$exitbars <- renderPlot({
     
     aco_master %>%
       filter(exit == 1) %>%
       ggplot(aes(x = year, fill = "blue")) +
       geom_bar() +
       labs(x = "Year",
            y = "Number of ACOs that Exited",
            title = "MSSP Exit by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "red")
    
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

