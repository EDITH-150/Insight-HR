library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(highcharter)
library(rpivotTable)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Dataset Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("jobrole", "Select Job role:", 
                  choices = c("admin", "technical", "marketing", "HR", "client_services",
                              "infotech", "production", "R&D")),
      selectInput("yaxis", "Y-axis:", 
                  choices = c("Work_Satisfaction", "Projects", "Tenure", 
                              "Evaluation_score", "Hrs_per_mnth_avg", "Turnover",
                              "Wages", "Domain", "Incentives", "Casualty")),
      selectInput("xaxis", "X-axis:", 
                  choices = c("Work_Satisfaction", "Projects", "Tenure", 
                              "Evaluation_score", "Hrs_per_mnth_avg", "Turnover",
                              "Wages", "Domain", "Incentives", "Casualty")),
      selectInput("colorby", "Color by:", 
                  choices = c("Turnover", "Wages", "Incentives", "Domain")),
      sliderInput("alpha", "Alpha:", 
                  min = 0, max = 1, value = 0.5)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { overflow: auto; }
        .tab-content { padding: 15px; }
        .box { margin-bottom: 20px; }
        .rpivotTable { 
          overflow: auto !important;
          max-width: 100% !important;
          height: 600px !important;
        }
        .plot-container { 
          height: 600px !important;
        }
        .dataTables_wrapper {
          overflow-x: auto;
          max-width: 100%;
        }
      "))
    ),
    
    fluidRow(
      valueBoxOutput("totalRows", width = 6),
      valueBoxOutput("totalFeatures", width = 6)
    ),
    
    fluidRow(
      column(12,
             tabBox(
               id = "tabset1",
               width = 12,
               height = "650px",
               tabPanel("Plot 1", 
                        div(class = "plot-container",
                            plotlyOutput("scatterPlot", height = "600px"))),
               tabPanel("Plot 2", 
                        div(class = "plot-container",
                            plotlyOutput("densityPlot", height = "600px"))),
               tabPanel("Plot 3", 
                        div(class = "plot-container",
                            plotlyOutput("boxPlot", height = "600px"))),
               tabPanel("Plot 4", 
                        div(class = "plot-container",
                            plotOutput("heatmap", height = "600px"))),
               tabPanel("Plot 5", 
                        div(class = "plot-container",
                            rpivotTableOutput("pivotTable"))),
               tabPanel("Dataset Representation", 
                        box(DTOutput("ptable"), width = 12, height = "650px"))
             )
      )
    )
  )
)