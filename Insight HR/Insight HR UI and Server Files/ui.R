library(shiny)
library(shinyWidgets)
library(shinythemes)

shinyUI(fluidPage(
  # Set background color with radial gradient
  setBackgroundColor(
    color = c("black", "teal"),
    gradient = "radial",
    direction = "top"
  ),
  
  # Application title
  titlePanel(h1("Personnel Turnover", style = 'background-color:teal; color: white; padding-left:20px')),
  
  br(),
  
  # Subtitle explaining the application
  h4("Dynamic Web Application to predict Turnover of personnel based on various factors", style = 'background-color: black; color: white'),
  
  # Sidebar layout with input widgets
  sidebarLayout(
    position = "right",
    
    sidebarPanel(
      style = "color:black",
      
      # Slider for Work Satisfaction
      sliderInput("work_satisfaction", 
                  "Enter the Work Satisfaction level (0 to 1)", 
                  value = 0.5, 
                  min = 0, 
                  max = 1),
      
      br(),
      
      # Slider for Evaluation Score
      sliderInput("evaluation_score", 
                  "Enter the Evaluation Score (0 to 1)", 
                  value = 0.5, 
                  min = 0, 
                  max = 1),
      
      br(),
      
      # Slider for Number of Projects
      sliderInput("projects", 
                  "Enter the Number of Projects (1 to 10)", 
                  value = 3, 
                  min = 1, 
                  max = 10),
      
      br(),
      
      # Numeric input for Average Hours per Month
      numericInput("Hrs_per_mnth_avg", 
                   "Enter Average Hours per Month (100 to 300)", 
                   value = 192,
                   min = 100,
                   max = 300),
      
      br(),
      
      # Numeric input for Tenure
      numericInput("Tenure", 
                   "Enter Tenure in Years (1 to 10)", 
                   value = 4,
                   min = 1,
                   max = 10),
      
      br(),
      
      # Submit button
      actionButton("btn", "Submit")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        # Prediction tab
        tabPanel("Prediction", 
                 h3("Turnover Prediction: ", style = "color:white;"),
                 hr(),
                 h2(textOutput("class"), style = "color:red; background-color:white;")
        ),
        
        # Decision tree visualization tab
        tabPanel("Tree", plotOutput("fit_tree")),
        
        # Accuracy tab
        tabPanel("Accuracy", uiOutput("acc_output"))
      )
    )
  )
))