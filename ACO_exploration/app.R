#
# This is a Shiny web application made to explore public data on the Medicare Shared Savings Program.
# Created by Andres de Loera-Brust in the Spring of 2019. 
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

# I make sure the data I need is available: 

aco_master <- read_rds("aco_master_file.rds") %>%
  mutate(qual_score = as.numeric(qual_score))

county_master <- read_rds("county_master_file.rds")

# Next I create tibbles with the choices I will pass to selectors. 
# I do this to be able to later retrieve the label and the actual variable as needed.
# That allows for flexible variable labels and titles later down the line. 

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

prominence_choices <- tibble(
  "Number of ACO Beneficiaries" = "aco_benes",
  "Log Number of ACO Beneficiaries" = "log_aco_benes",
  "ACO Participation Rate" = "aco_participation_rate")

county_choices <- tibble(
  "Fee For Service Beneficiaries" = "ffs_beneficiaries",
  "Medicare Advantage Beneficiaries" = "ma_beneficiaries",
  "Medicare Advantage Participation Rate" = "ma_participation_rate",
  "Average Medicare Beneficiary Age" = "average_age",
  "Total Medicare Costs" = "total_actual_costs",
  "Average Costs per Beneficiary" = "actual_per_capita_costs")

state_choices <- tibble(
  "Entered" = "entered",
  "Exit" = "exit")

#I also create a way to look up the label of the variables so I can have reactive labels.

feature_lookup <- 
  feature_choices %>% 
  gather(name, symbol)

aco_choice_lookup <- aco_choices %>%
  gather(name, symbol)

prominence_lookup <- prominence_choices %>%
  gather(name, symbol)

county_lookup <- county_choices %>%
  gather(name, symbol)

state_lookup <- state_choices %>%
  gather(name, symbol)


# Define UI for application
# The first argument defines the theme for the app, which I chose to be "spacelab" 

ui <- fluidPage(theme = shinytheme("spacelab"),
  
   # I set the application title
  
   titlePanel("Exploring the Medicare Shared Savings Program"),
   
   # Here I setup the sidebar, which will have different selector options depending on the tab.
   # Hence each selector input is preceded by a conditionalPanel argument. 
   
   sidebarLayout(
      sidebarPanel(
        
        #On the background panel I add a selector for year and a selector for ACO level variable. 
        
        conditionalPanel(
          condition = "input.tabs == 'Background'",
          
          selectInput("year_1",
                      "Select program year",
                      choices = c(2014, 2015, 2016, 2017),
                      selected = 2017,
                      multiple = FALSE,
                      selectize = TRUE),
          
          selectInput("orgvar",
                      "Select an ACO Variable",
                      choices = feature_choices,
                      selected = "bnchmk_min_exp")
          
        ),
        
        # On the county variation panel I add a selector for year, ACO related county variable to map,
        # ACO county variable for a scatter plot, other county variable for a scatter plot, 
        # And the option to fit a linear model. 
        
        conditionalPanel(
          condition = "input.tabs == 'County Variation'",
          
          selectInput("year",
                      "Select program year",
                      choices = c(2014, 2015, 2016, 2017),
                      selected = 2017,
                      multiple = FALSE,
                      selectize = TRUE),
          
          selectInput("acovar",
                      "Select a variable to graph and map",
                      choices = aco_choices, 
                      selected = "log_aco_benes"),
          
          selectInput("county_x",
                      "Select a measure of ACO prominence",
                      choices = prominence_choices,
                      selected = "aco_benes"),
          
          selectInput("county_y",
                      "Select a county variable",
                      choices = county_choices,
                      selected = "actual_per_capita_costs"),
          
          checkboxInput("fit2", 
                        "Fit a linear model?", 
                        FALSE)
         ),
        
        #On the program features panel I give choices for the x and y variables.
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
        
        #On the entrance and exit panel I give the user the choice of what variables to consider
        #And whether they want to look at entry or exit. 
        
         conditionalPanel(
           condition = "input.tabs == 'Entrance and Exit'",
           
           selectInput("statevar",
                       "Select entry or exit",
                       choices = state_choices,
                       selected = "entered"),
          
           selectInput("violinvar",
                    "Select a variable to compare on",
                    choices = feature_choices,
                    selected = "qual_score")
        )),
      
      # The main panel will have several tabs.
      
      mainPanel(
        
        tabsetPanel(id = "tabs",
                    
      # The first tab will have background information and some basic visualizations.  
                    
                    tabPanel("Background", 
                             
                             htmlOutput("background"), 
                             
                             #The first visualization is a bar graph of program participation by year.
                             
                             plotOutput("yearbars"),
                             
                             htmlOutput("description1"), 
                             
                             #The second visualization is a flexible density plot. 
                             
                             plotOutput("flexplot"),
                             
                             htmlOutput("description2")),
      
      # The next tab will present the relationship between various organizational variables. 
      # That will take the form of a very flexible scatterplot with the option to fit a linear model.
      # And naturally I add descriptive text. 
      
                    tabPanel("Program Features", 
                             
                             htmlOutput("features"),
                             
                             plotOutput("scatterfeatures"),
                             
                             htmlOutput("program_feature_stats")), 
          
      # The next tab will discuss entrance and exit in the MSSP program 
      # and compare program features on that axis. Ill do that will bar charts and violin plots.
      # I also add text to describe the panel. 
      
                    tabPanel("Entrance and Exit", 
                             
                             htmlOutput("entryexit"),
                             
                             plotOutput("bars"), 
                             
                             plotOutput("violin")),
      
      # The next tab will focus on variation at the county level with some plots and maps. 
      # I also add text between the plots and data.  
      
                    tabPanel("County Variation",
                             
                             htmlOutput("participation1"),
                             
                             plotOutput("distPlot"),
                             
                             plotOutput("countymap"),
                             
                             htmlOutput("participation2"),
                             
                             plotOutput("participationPlot"),
                             
                             htmlOutput("participation_stats")),
      
      #The last tab will have generic background information on the app, like links to data and code
      #as well as my contact information. 
      
                    tabPanel("About",
                             
                             htmlOutput("about"))
      

        ))))


# Here I define the server logic required for the app.

server <- function(input, output) {
  
  # I write the code for the output in order of the tabs.
  
  # This chunk begins the text description on the Background tab.
  
  output$background <- renderUI({
    
    about1 <- h3(strong("Background"))
    about2 <- p("Economists and policymakers have long debated the optimal method to pay for the delivery of healthcare. Ever since Medicare ceased to reimburse providers for their reported costs and instead decided to set prospective rates for health services, a key question in this debate has been the correct unit of analysis. For several decades the dominant models were Fee For Service (FFS), where providers are paid a fixed fee for every service provided and thus face incentives to provide as many services as possible, and Diagnosis Related Groups (DRGs), where providers are paid a fixed sum for all of the care a patient receives for a given condition and thus face incentives to code more and more severe diagnoses but potentially under provide care within each episode to increase their profit margins. However, some researchers have long believed that it would be best to reimburse providers for people rather than services and thus incentivize them to focus on value across every margin. As part of the Affordable Care Act, the Medicare program was authorized to create a program experimenting with these “capitation” models. Providers who wanted to participate in the Medicare Shared Savings Program (MSSP) would form a larger entity, known as an Accountable Care Organization (ACO), and agree to take a fixed lump sum payment (known in the program as the benchmark) for an entire population of Medicare Beneficiaries. As the graph below shows, the program has grown over the past several years.")
    
    HTML(paste(about1, 
               about2, br()))
    
  })
  
  # Here I make a simple bar chart showing the number of MSSP Participating ACOs every year. 
  # The additional arguments just set the labels, theme and color. 
  
   output$yearbars <- renderPlot({
     
     #I start with the ACO level master file.
     aco_master %>%
       
       #The x axis is naturally the year. Since it's a bar chart I need no y variable.
       
       ggplot(aes(x = year, fill = "blue")) +
       
       #I call geom_bar() to make a barchart. 
       
       geom_bar() +
       
       #I add labels to describe the plot.
       
       labs(x = "Year",
            y = "Number of ACOs",
            title = "MSSP Participation by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       
       #As a lover of The Economist magazine, I use the theme that replicates their style for my plots.
       
       theme_economist() +
       
       #I eliminate the unnecessary legend. 
       
       theme(legend.position = "none") +
       
       #I want to make the color in the app various shades of blue, so I set the fill color to blue.
       
       scale_fill_manual(values = "dark blue")
     
   })
   
   # Here I continue the descriptive text found on the Background tab. 
   
   output$description1 <- renderUI({
     
     about3 <- h3(strong("App Description"))
     about4 <- p("Naturally such a new and innovative program still has considerable uncertainty attached. This web application pulls together several publicly available datasets provided by the Center for Medicare and Medicaid Services (CMS) and allows the user to explore interesting features of the program. For example, the plot below shows the distribution of a selected variable by ACO in a given year. If you’d like to see the plot for a different year or variable, you can select as such on the left-hand panel.")

     HTML(paste(about3, 
                about4, br()))
     
   })
   
   # Here I create the density plot output on the Background tab. 
   
   output$flexplot <- renderPlot({
     
     #I start with the ACO level master dataset. 
     
     aco_master %>%
       
       #I filter by the selected year. 
       
       filter(year == input$year_1) %>%
       
       #I set the x variable to be responsive to the user's input. 
       #No y variable is needed for a density plot.
       
       ggplot(aes_string(x = input$orgvar, fill = '"blue"')) +
       
       #I call geom_density to make a density plot. 
       
       geom_density(alpha = 0.75) +
       
       #I define the labels and titles to respond to user input using the clear version of the variable name.
       
       labs(x = paste(filter(feature_lookup, symbol==input$orgvar)["name"]),
            y = "Density of ACOs",
            title = paste(filter(feature_lookup, symbol==input$orgvar)["name"], "for MSSP ACOs in", input$year_1),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       
       #I set the theme to emulate the economist.
       
       theme_economist() +
       
       #I remove an unnecessary legend.
       
       theme(legend.position = "none") + 
       
       #I set the fill color to blue for aesthetics.
       
       scale_fill_manual(values = "blue") 
   })
   
   #Here I complete the descriptive text found on the background tab. 
   #This is more complicated, in that I use more lines, because of the bullet poiting.
   
   output$description2 <- renderUI({
     
     about5 <- p("The “Program Features” tab allows the user to explore the relationship between many organizational variables with a scatterplot and (optional) linear model. The “County Variation” tab allows the user to explore how the prominence of ACOs varies across counties and how it relates to other Medicare variables at the county level. Finally, the “Exit and Entrance” tab allows the user to compare organizations that enter and exit the program to those which are continuing members on several organizational variables.  The plots are designed to be as flexible as possible to allow the user to focus on whatever relationships may be most interesting. ")
     
     about6 <- h3(strong("Insights"))
     about7 <- p("Since there are so many choices for what to explore it may be difficult to identify the most interesting insights. Below are just a few relationships revealed in the app that may be of interest to anyone curious about ACOs and the MSSP:")
     about8 <- p("-The savings/losses generated by ACOs in the program become more variable as time goes on (Note that these aren’t necessarily the “true” savings of the program because they are defined relative to the ACOs benchmark rather than the counterfactual where the providers don’t participate in the program.)")
     about9 <- p("-Longer operating ACOs have higher quality scores, shared savings rates, and generate and earn more savings. However, it is unclear if performance increases over time or if the worse performing ACOs exit.")
     about10 <- p("-Larger ACOs have lower per capita benchmark and lower per capita expenditures, which may suggest benefits of scale. ")
     about11 <- p("-There appears to be little relationship between number of ACO beneficiaries in a country and the ACO participation rate in that county. ")
     about12 <- p("-ACO participation rate seems to have little relationship with average cost per beneficiary (though perhaps ACOs are simply more appealing in high cost areas and they help lessen variation). ")
     about13 <- p("-Entering ACOs all get low quality scores but exiting ones do not. ")
     about14 <- p("-Entering and exiting ACOs have smaller beneficiary populations than continuing. ")
     
       
     HTML(paste(br(),
                about5, 
                about6, 
                about7, 
                about8, 
                about9, 
                about10, 
                about11, 
                about12, 
                about13,
                about14, br()
                ))
     
   })
   
   #Now I turn to the "Program Features" tab.
   #I begin by creating the text description, which describes each variable available for selection.
   
   output$features <- renderUI({
     
     features1 <- h3(strong("The Relationship Between Organizational Variables"))
     features2 <- p("This tab allows the user to compare how organizational variables relate to each other within the MSSP. The unit of observation is one year for one ACO. Using the tab on the left, you can select with variables appear on the X and Y axes of the scatterplot below, as well as whether to fit a linear model. The variables are defined as follows:")
     features3 <- p(strong("Number of Beneficiaries"), "refers to the number of Medicare beneficiaries assigned to the ACO in that year and used to calculate the benchmark.")
     features4 <- p(strong("Total Benchmark"), "refers to the total monetary cost Medicare has allocated for an ACO in a given year. Should the ACO spend less than that amount, it gets to keep a proportion equal to its shared savings rate. Otherwise it may (or may not) bear some losses. ")
     features5 <- p(strong("Per Capita Benchmark"), "is the Total Benchmark divided by Number of Beneficiaries, to arrive at the average cost Medicare has allocated for an ACO for each assigned person in a given year.")
     features6 <- p(strong("Total Expenditures"), "refers to the total monetary expenditures of an ACO attributed to their Medicare beneficiary population in the given year. ")
     features7 <- p(strong("Per Capita Expenditures"), "is the Total Expenditures divided by the Number of Beneficiaries, to arrive at the average cost the ACO incurred for each assigned person in a given year. ")
     features8 <- p(strong("Savings/Losses Generated"), "is the result of subtracting Total Expenditures from the Total Benchmark, to arrive at the total monetary amount the ACO saved or lost relative to their benchmark in a given year.")
     features9 <- p(strong("Savings/Loss Rate"), "is the proportion of Total Benchmark that was saved or lost in the given year.")
     features10 <- p(strong("Shared Savings Rate"), "refers to the proportion of Savings/Losses Generated that are kept (or lost) by the ACO in a given year.")
     features11 <- p(strong("Savings Earned"), "refers to the monetary amount of savings kept by the ACO in the given year. ")
     features12 <- p(strong("Quality Score"), "is a single numeric measure of quality of care received by Medicare beneficiaries at an ACO in the given year. ")
     features13 <- p(strong("Length of Service"), "refers to the number of days (as of the last day of a given year) since an ACO began its first agreement to participate in the MSSP. ")
     
     HTML(paste(features1,
                features2,
                features3,
                features4,
                features5,
                features6,
                features7,
                features8,
                features9,
                features10,
                features11,
                features12,
                features13))
     
   })
   
   #Here I create a scatterplot of the two user-selected variables.
   #I also create the possibility of a fitted linear model.
   
   output$scatterfeatures <- renderPlot({
     
     #First I define what the base scatterplot will be like, with or without the fitted line. 
     #That starts with the ACO level data.
     
    p <- aco_master %>%
      
      #I allow the user to select x and y variables. 
      
       ggplot(aes_string(x = input$xvar, y = input$yvar)) +
      
      #I call geom_point to make a scatter plot 
      #I also set the alpha low so one can see the clustering of points.
      
       geom_point(alpha = 0.25) +
      
      #I set the labels and title here. The axes labels are defined to be reactive to user input.
      
       labs(caption = "Data from the Center for Medicare and Medicaid Services",
            color = "Colored by Year",
            x = paste(filter(feature_lookup, symbol==input$xvar)["name"]),
            y = paste(filter(feature_lookup, symbol==input$yvar)["name"]),
            title = "The Relationship Between Organizational Variables in the MSSP") +
      
      #I emulate the economist's style here again. 
      
       theme_economist_white() 
    
    #I also add a linear fit if the user selects it. 
    #If the user selects to fit the line, I add a geom_smooth argument. 
    
    if(input$fit == TRUE) {
      
      p + geom_smooth(method = "lm", se = TRUE)
      
    #Otherwise,  simply display the base scatterplot. 
      
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
   
   #Here I make all the outputs for the Entry and Exit tab.
   #That begins with a text output with the description of the tab. 
   
   output$entryexit <- renderUI({
     
     entryexit1 <- h3(strong("How are entering and exiting ACOs unique?"))
     
     entryexit2 <- p("This tab allows you to explore entrance and exit in the Medicare Shared Savings Program. You can select which state you’d like to consider and then which organizational variable you would like to compare on. If you select to compare “Entered” and “Not Entered” ACOs, the left hand violin plot will show the distribution of the selected variable for ACO years which are not the first year of an ACOs participation in the program and the right hand side will show the selected distribution for ACO years which are the first year of an ACOs participation in the program. If you select to compare “Exit” and “Not Exit” ACOs, the left hand violin plot will show the distribution of the selected variable for ACO years which are not the last year of an ACOs participation in the program and the right hand side will show the selected distribution for ACO years which are the last year of an ACOs participation in the program. The organizational variables you can choose to compare are defined the same way as detailed on the “Program Features” tab.")
     
     HTML(paste(entryexit1, entryexit2))
       
   })
   
   #Here I make a bar chart of program entrances or exits by year.
   
   output$bars <- renderPlot({
     
     aco_master %>%
       filter(eval(parse(text=input$statevar)) == 1) %>%
       ggplot(aes(x = year, fill = "blue")) +
       geom_bar() +
       labs(x = "Year",
            y = paste("Number of ACOs that", filter(state_lookup, symbol==input$statevar)["name"]),
            title = "MSSP Churn by Year",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "light blue")
     
   })
   
   #Here I make a reactive violin plot where the variable of analysis is selected by the user.
   
   output$violin <- renderPlot({
     
     aco_master %>%
       ggplot(aes_string(y = input$violinvar, group = input$statevar, x = input$statevar)) +
       geom_violin(alpha = 0.25, fill = "blue") + 
       scale_x_continuous(breaks = c(0, 1),
                          labels = c(paste("Not", paste(filter(state_lookup, symbol==input$statevar)["name"])), paste(filter(state_lookup, symbol==input$statevar)["name"]))) +
       labs(x = element_blank(),
            y = paste(filter(feature_lookup, symbol==input$violinvar)["name"]),
            title = "Organization features vary by continuing, exiting or entering status",
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       scale_fill_manual(values = "dark blue")
     
   })
   
   #Now I define the outputs for the county variation tab. 
   #That begins with the descriptive text outputs. 
   
   output$participation1 <- renderUI({
     
     participation1_1 <- h3(strong("How does assignment to ACOs vary across the country?"))
     
     participation1_2 <- p("This tab allows you to explore how the prominence of the Medicare Shared Savings Program varies across the country and how it relates to other regional Medicare data. The first two plots allow you to select year and a measure of ACO prominence and see its distribution on a density plot and a map. The two measures here are the (log base 10) number of ACO beneficiaries in a county and the ACO participation rate, defines as the proportion of Medicare beneficiaries in the county assigned to an ACO. ")
     
     HTML(paste(participation1_1,
          participation1_2))
       
   })
   
   output$participation2 <- renderUI({
     
     participation2_1 <- p("The scatterplot below allows you to visualize how measures of ACO prominence relate to other county level Medicare variables, with the choice to fit a linear model. For measures of ACO prominence on the X axis you have the choice of Log Number of ACO Beneficiaries and ACO Participation Rate, as above, but also the choice to see the raw Number of ACO Beneficiaries. For the Y axis you can select from the following variables: The number of Medicare beneficiaries in the “standard” Medicare Fee for Service plan in the county, the number of Medicare beneficiaries in Medicare Advantage plans in the county, the Medicare Advantage participation rate in the county, the average age of Medicare beneficiaries in the county, the total Medicare costs in the county, and the average costs for a Medicare beneficiary in the county. ")
     
     HTML(paste(participation2_1))
     
   })
   
   #Here I make a density plot of ACO prominence measures by county. 
   #I set the axes deliberately to get an appealing and interpretable chart. 
   #I also set the labels, color and theme for asthetic appeal. 
   
   output$distPlot <- renderPlot({
     
     county_master %>%
       filter(year == input$year) %>%
       ggplot(aes_string(x = input$acovar, fill = '"light blue"')) +
       geom_density() +
       labs(x = paste(filter(aco_choice_lookup, symbol==input$acovar)["name"], "in county"),
            y = "Density of Counties",
            title = paste("The distribution of MSSP beneficiaires by county in", input$year),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white() +
       theme(legend.position = "none") +
       scale_fill_manual(values = "light blue")
     
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
   
   output$participationPlot <- renderPlot({
     
     g <- county_master %>%
       filter(year == input$year) %>%
       ggplot(aes_string(x = input$county_x, y =input$county_y)) +
       geom_point() +
       labs(title = "MSSP prominence compared with other Medicare variables",
            subtitle = paste(filter(prominence_lookup, symbol==input$county_x)["name"], "vs", filter(county_lookup, symbol==input$county_y)["name"], "in", input$year),
            x = paste(filter(prominence_lookup, symbol==input$county_x)["name"]),
            y = paste(filter(county_lookup, symbol==input$county_y)["name"]),
            caption = "Data from the Center for Medicare and Medicaid Services") +
       theme_economist_white()
     
     if(input$fit2 == TRUE) {
       g + geom_smooth(method = "lm", se = TRUE)
     } else {
       g
     }
       
   })
   
   output$participation_stats <- renderUI({
     
     if(input$fit2 == TRUE) {
       
       data <- county_master
       
       model_input <- formula(paste(input$county_y, " ~ ", input$county_x))
       
       model <- lm(model_input, data)
       
       participation_table <- tab_model(model, 
                                          pred.labels = c("Intercept", paste(filter(prominence_lookup, symbol==input$county_x)["name"])), 
                                          dv.labels = paste(filter(county_lookup, symbol==input$county_y)["name"]))
       
       HTML(participation_table$knitr)
       
     } else {
       
       HTML("Check the box on the left to see a fitted linear model")
       
     }
     
   })

   output$about <- renderUI({
     
     
     about_1 <- h3(strong("App Background"))
     about_2 <- p("This app was created by Andres de Loera-Brust in the spring of 2019. He can be reached at adeloerabrust@college.harvard.edu.")
     
     about_3 <- h3(strong("Data Sources"))
     about_4 <- p("-Detailed ACO-level data comes from Shared Savings Program Accountable Care Organizations (ACO) Public-Use Files, made available by CMS", tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html", "here."))
     about_5 <- p("-County-level ACO beneficiary data comes from the Number of ACO Assigned Beneficiaries by County Public-Use Files, available in as part of the Shared Savings Program Benchmark Rebasing PUFs found", tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/SSP_Benchmark_Rebasing.html", "here."))
     about_6 <- p("-County-level Medicare data comes from the State/County Table on All Beneficiaries in the Medicare Geographic Variation Public Use Files found", tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Geographic-Variation/GV_PUF.html", "here."))
     about_7 <- p("-The US county map shapefile comes from the Census Bureau’s Cartographic Boundary Files, available", tags$a(href="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2016.html", "here."))
     
     about_8 <- h3(strong("Source Code"))
     about_9 <- p("View the source code ", tags$a(href="https://github.com/adeloera/MSSP-Explorer", "here."))
     
     HTML(paste(about_1, 
                about_2, br(),
                about_3, 
                about_4,
                about_5,
                about_6,
                about_7, br(),
                about_8,
                about_9, br()))
     
   })
   
}

# Run the application 

shinyApp(ui = ui, server = server)

