#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#I begin as always by loading in the libraries I will use

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(sf)
library(tidyverse)
library(ggthemes)

#I make sure the data I need is available: 

aco_master <- read_rds("aco_master_file.rds") %>%
  mutate(qual_score = as.numeric(qual_score))

county_master <- read_rds("county_master_file.rds")

#I create a tibble with the feature choices I will use for selector inputs. 

feature_choices <- tibble(
  "Number of Beneficiaries" = "n_ab",
  "Per Capita Benchmark" = "updated_bnchmk",
  "Total Benchmark" = "a_btot_bnchmk",
  "Per Capita Expenditures" = "per_capita_exp_total_py",
  "Total Expenditures" = "a_btot_exp",
  "Shared Savings Rate" = "final_share_rate",
  "Quality Score" = "qual_score",
  "Savings/Losses Generated" = "bnchmk_min_exp",
  "Saving/Loss Rate" = "sav_rate",
  "Savings Earned" = "earn_save_loss",
  "Length of Services" = "service_length")

#I also create a way to look up the label of the variables so I can have reactive axes. 

feature_lookup <- 
  feature_choices %>% 
  gather(name, symbol)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  
   titlePanel("Exploring ACO Participation"),
   
   # Here I setup the sidebar, which will have different selector options depending on the tab.
   # Hence each selector input is preceded by a conditionalPanel argument. 
   
   sidebarLayout(
      sidebarPanel(
        
        conditionalPanel(
          condition = "input.tabs == 'Background'",
          
          selectInput("year",
                      "Select program year",
                      choices = c(2014, 2015, 2016, 2017),
                      selected = 2017,
                      multiple = FALSE,
                      selectize = TRUE)
        ),
        
        
        conditionalPanel(
          condition = "input.tabs == 'County Variation'",
          
          selectInput("year",
                    "Select program year",
                    choices = c(2014, 2015, 2016, 2017),
                    selected = 2017,
                    multiple = FALSE,
                    selectize = TRUE)
         ),
        
        conditionalPanel(
          condition = "input.tabs == 'Program Features'",
          
          selectInput('xvar', 
                    "Select x variable", 
                    choices = feature_choices,
                    selected = "service_length"),
        
          selectInput('yvar', 
                    "Select y variable", 
                    choices = feature_choices,
                    selected = "bnchmk_min_exp")
        
         ),
        
         conditionalPanel(
           condition = "input.tabs == 'Entrance and Exit'",
          
           selectInput("violinvar",
                    "Select a variable to compare on",
                    choices = feature_choices,
                    selected = "earn_save_loss")
        )),
      
      # The main panel will have several tabs.
      mainPanel(
        
        tabsetPanel(id = "tabs",
                    
      # The first tab will have background information and some basic visualizations. 
                    
                    tabPanel("Background", 
                             
                             verbatimTextOutput("summary"), 
                             
                             plotOutput("yearbars")),
      
      # The next tab will present the relationship between various organizational variables. 
      
                    tabPanel("Program Features", 
                             
                             plotOutput("scatterfeatures")), 
      
      # The next tab will focus on variation at the county level with some plots and maps. 
      # This is where selecting program year will be relevant. 
      
                    tabPanel("County Variation",
                             
                             plotOutput("distPlot"), 
                             
                             plotOutput("countymap")),
      
      # The next tab will discuss entrance and exit in the MSSP program 
      # and compare program features on that axis.
      
                    tabPanel("Entrance and Exit", 
                             plotOutput("enterbars"), 
                             
                             plotOutput("enterviolin"),
                             
                             plotOutput("exitbars"),
                             
                             plotOutput("exitviolin"))
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
   
   
   output$scatterfeatures <- renderPlot({
     
     aco_master %>%
       ggplot(aes_string(x = input$xvar, y = input$yvar), aes(color = year)) +
       geom_point(alpha = 0.25) +
       labs(caption = "Data from the Center for Medicare and Medicaid Services",
            color = "Colored by Year",
            x = paste(filter(feature_lookup, symbol==input$xvar)["name"]),
            y = paste(filter(feature_lookup, symbol==input$yvar)["name"]),
            title = "The Relationship Between Organizational Variables in the MSSP") +
       theme_minimal() 
     
   })
   
   
   output$distPlot <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes(x = aco_benes, fill = "light blue")) +
       geom_density() +
       scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000, 1000000), 
                     labels = c("10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
       scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5),
                          labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5")) +
       labs(x = "ACO beneficiaries in county (Log base 10)",
            y = "Density of Counties",
            title = "The count of MSSP beneficiaires by county",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "light blue")
     
   })
   
   output$countymap <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes(fill = log(aco_benes + 1))) +
       geom_sf() +
       labs(title = "Medicare Shared Savings Program assigned beneficiaries by county",
            subtitle = paste("Beneficiary population in ", input$year),
            caption = "Data from the Center for Medicare and Medicaid Services",
            fill = "Log Beneficiary Population") +
       theme_minimal() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank()) 
     
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
   
   output$enterviolin <- renderPlot({
     
     aco_master %>%
       ggplot(aes_string(y = input$violinvar, group = "entered", x = "entered")) +
       geom_violin(alpha = 0.25, fill = "green") + 
       scale_x_continuous(breaks = c(0, 1),
                          labels = c("Continuing", "Entering")) +
       labs(x = element_blank(),
            y = paste(filter(feature_lookup, symbol==input$violinvar)["name"]),
            title = "Organization features vary by continuing or entering status",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
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
   
   output$exitviolin <- renderPlot({
     
     aco_master %>%
       ggplot(aes_string(y = input$violinvar, group = "exit", x = "exit")) +
       geom_violin(alpha = 0.25, fill = "red")+
       scale_x_continuous(breaks = c(0, 1),
                          labels = c("Continuing", "Exiting")) +
       labs(x = element_blank(),
            y = paste(filter(feature_lookup, symbol==input$violinvar)["name"]),
            title = "Organization features vary by continuing or exiting status",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_minimal() +
       scale_fill_manual(values = "red")
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

