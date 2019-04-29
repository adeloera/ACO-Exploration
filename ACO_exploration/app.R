#
# This is a Shiny web application made to explore public data on the Medicare Shared Savings Program.
# Created by Andres de Loera-Brust for Gov 1005 in Spring 2019. 
# You can run the application by clicking the 'Run App' button above.


#I begin as always by loading in the libraries I will use

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(sf)
library(knitr)
library(sjPlot)
library(scales)
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

aco_choices <- tibble(
  "Log Number of ACO Beneficiaries" = "log_aco_benes",
  "ACO Participation Rate" = "aco_participation_rate")

#I also create a way to look up the label of the variables so I can have reactive axes. 

feature_lookup <- 
  feature_choices %>% 
  gather(name, symbol)

aco_choice_lookup <- aco_choices %>%
  gather(name, symbol)


# Define UI for application
#The first argument defines the theme for the app, which I chose to be "spacelab" 

ui <- fluidPage(theme = shinytheme("spacelab"),
  
   # I set the pplication title
  
   titlePanel("Exploring the Medicare Shared Savings Program"),
   
   # Here I setup the sidebar, which will have different selector options depending on the tab.
   # Hence each selector input is preceded by a conditionalPanel argument. 
   
   sidebarLayout(
      sidebarPanel(
        
        #On the background panel I only add a selector for year.
        
        conditionalPanel(
          condition = "input.tabs == 'Background'",
          
          selectInput("year_1",
                      "Select program year",
                      choices = c(2014, 2015, 2016, 2017),
                      selected = 2017,
                      multiple = FALSE,
                      selectize = TRUE)
        ),
        
        #On the county variation panel I again only add a selector for year. 
        
        conditionalPanel(
          condition = "input.tabs == 'County Variation'",
          
          selectInput("year",
                    "Select program year",
                    choices = c(2014, 2015, 2016, 2017),
                    selected = 2017,
                    multiple = FALSE,
                    selectize = TRUE),
          
          selectInput("acovar",
                      "Select a variable to map",
                      choices = aco_choices, 
                      selected = "log_aco_benes")
         ),
        
        #On the program features panel I give choices for the x and y variables
        #I also add a box that can be checked to add a fitted line. 
        
        conditionalPanel(
          condition = "input.tabs == 'Program Features'",
          
          selectInput('xvar', 
                    "Select x variable", 
                    choices = feature_choices,
                    selected = "service_length"),
        
          selectInput('yvar', 
                    "Select y variable", 
                    choices = feature_choices,
                    selected = "bnchmk_min_exp"),
          
          checkboxInput("fit", 
                        "Fit a linear model?", 
                        FALSE)
          
         ),
        
        #On the entrance and exit panel I give the user the choice of what variables to consider. 
        
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
                             
                             plotOutput("yearbars"),
                             
                             plotOutput("saveplot")),
      
      # The next tab will present the relationship between various organizational variables. 
      
                    tabPanel("Program Features", 
                             
                             plotOutput("scatterfeatures"),
                             
                             htmlOutput("program_feature_stats")), 
      
      # The next tab will focus on variation at the county level with some plots and maps. 
      # This is where selecting program year will be relevant. 
      
                    tabPanel("County Variation",
                             
                             plotOutput("distPlot"), 
                             
                             plotOutput("penePlot"),
                             
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


# Define server logic required for the app.

server <- function(input, output) {
  
  
  #Here I make a simple bar chart showing the number of MSSP Participating ACOs every year. 
  #The additional arguments just set the labels, theme and color. 
  
   output$yearbars <- renderPlot({
     
     aco_master %>%
       ggplot(aes(x = year, fill = "blue")) +
       geom_bar() +
       labs(x = "Year",
            y = "Number of ACOs",
            title = "MSSP Participation by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "blue")
     
   })
   
   output$saveplot <- renderPlot({
     
     aco_master %>%
       filter(year == input$year_1) %>%
       ggplot(aes(x = gen_save_loss, fill = "blue")) +
       geom_density(alpha = 0.5) +
       labs(x = "Savings/Losses Generated",
            y = "Density of ACOs",
            title = paste("Savings and Losses Generated in the MSSP in", input$year_1),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") + 
       scale_fill_manual(values = "blue") +
       scale_x_continuous(labels = dollar)
   })
   
   output$scatterfeatures <- renderPlot({
     
     #Here I create a scatterplot of the two user-selected variables. 
     #The label arguments are set such that the labels will change to respond to the users selection. 
     
    p <- aco_master %>%
       ggplot(aes_string(x = input$xvar, y = input$yvar)) +
       geom_point(alpha = 0.25) +
       labs(caption = "Data from the Center for Medicare and Medicaid Services",
            color = "Colored by Year",
            x = paste(filter(feature_lookup, symbol==input$xvar)["name"]),
            y = paste(filter(feature_lookup, symbol==input$yvar)["name"]),
            title = "The Relationship Between Organizational Variables in the MSSP") +
       theme_economist_white() 
    
    #I also add a linear fit if the user selects it. 
    
    if(input$fit == TRUE) {
      
      
      p + geom_smooth(method = "lm", se = FALSE)
      
    } else {
      
      p
      
    }
     
   })
   
   #Here I create a regression table for the linear model.
   #It's defined to only appear if the user selects the relevant box.
   
   output$program_feature_stats <- renderUI({
     
     if(input$fit == TRUE) {
       
       data <- aco_master
       
       model_input <- formula(paste(input$yvar, " ~ ", input$xvar))
       
       model <- lm(model_input, data)
       
       program_feature_table <- tab_model(model, 
                                          pred.labels = c("Intercept", paste(filter(feature_lookup, symbol==input$xvar)["name"])), 
                                          dv.labels = paste(filter(feature_lookup, symbol==input$yvar)["name"]))
       
       HTML(program_feature_table$knitr)
       
     } else {
       
       HTML("Check the box on the left to see a fitted linear model")
       
     }
     
   })
   
   #Here I make a density plot of beneficiaries per county. 
   #I set the axes deliberately to get an appealing and interpretable chart. 
   #I also set the labels, color and theme for asthetic appeal. 
   
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
            title = paste("The distribution of MSSP beneficiaires by county in", input$year),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "light blue")
     
   })
   
   #Here I make a plot showing the distribution of counties based on their ACO participation rate. 
   
   output$penePlot <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes(x = aco_participation_rate, fill = "light blue")) +
       geom_density() +
       labs(x = "ACO participation rate in county",
            y = "Density of Counties",
            title = paste("The distribution of MSSP participation rates by county in", input$year),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "dark blue")
     
   })
   
   #Here I create a map of the counties in the contiguous united states, filled by ACO beneficiaries.
   #The user gets to set the year prsented and the subtitle will respond to the users selection.
   #The other arguments set the theme and labels for clarity.  
   
   output$countymap <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes_string(fill = input$acovar)) +
       geom_sf() +
       labs(title = "Distribution of the Medicare Shared Savings Program by county",
            subtitle = paste(filter(aco_choice_lookup, symbol==input$acovar)["name"], "by county in", input$year),
            caption = "Data from the Center for Medicare and Medicaid Services",
            fill = paste(filter(aco_choice_lookup, symbol==input$acovar)["name"])) +
       theme_economist_white() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank()) 
     
   })
   
   #HereI make a bar chart of program entrances by year.
   
   output$enterbars <- renderPlot({
     
     aco_master %>%
     filter(entered == 1) %>%
     ggplot(aes(x = year, fill = "blue")) +
     geom_bar() +
     labs(x = "Year",
          y = "Number of ACOs that Entered",
          title = "MSSP Entrance by Year",
          caption = "Data from the Center for Medicare and Medicaid Services") +
     theme_economist_white() +
     theme(legend.position = "none") +
     scale_fill_manual(values = "dark green")
     
   })
   
   #Here I make a reactive violin plot where the variable of analysis is selected by the user.
   #It compares newly entered ACOs to ACOs that have been in the program longer than a year. 
   
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
       theme_economist_white() +
       scale_fill_manual(values = "green")
     
   })
   
   #Here I make a bar chart of program exits by year. 
   
   output$exitbars <- renderPlot({
     
     aco_master %>%
       filter(exit == 1) %>%
       ggplot(aes(x = year, fill = "blue")) +
       geom_bar() +
       labs(x = "Year",
            y = "Number of ACOs that Exited",
            title = "MSSP Exit by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "red")
    
   })
   
   #Here I make a reactive violin plot where the variable of analysis is selected by the user.
   #It compares ACOs that exit in a given year to those that dont.  
   
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
       theme_economist_white() +
       scale_fill_manual(values = "red")
     
   })
   
   
}

# Run the application 

shinyApp(ui = ui, server = server)

