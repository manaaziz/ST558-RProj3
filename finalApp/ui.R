#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(tools)
library(caret)
library(plotly)
library(DT)

# Read in data and subset it
hotel <- read_csv("../H1.csv")
covs <- names(hotel)[c(1,2,8,26,20,28,22,14,27)]
hotel <- hotel[1:5000, covs]
hotel$IsCanceled <- hotel$IsCanceled %>% as.factor()

# Define UI for application that draws a histogram
shinyUI(fluidPage(useShinyjs(),
    tabsetPanel(
        tabPanel("Information", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h2("ST558 Final Project"),
                         h3("By: Mana Azizsoltani"),
                         br(),
                         h4("External Links"),
                         tags$div(
                             tags$ul(
                                 tags$li(a(href = "https://github.com/manaaziz/ST558-RProj3", "REPOSITORY")),
                                 tags$li(a(href = "https://manaaziz.github.io", "BLOG")),
                                 tags$li(a(href = "https://www.linkedin.com/in/manaazizsoltani/", "LINKEDIN"))
                             )
                         ),
                         actionButton(inputId = "posty", label = "PRESS HERE"),
                         hidden(div(id='funtext', htmlOutput("postText")))
                     ),
                     mainPanel(
                         h1("Welcome to my app!"),
                         br(),
                         tags$div(
                            h3("Introduction"),
                            tags$p("I am a ", tags$i("Sin City"), " native and the son of a professor in the hospitality department
                                   at the Univesity of Nevada, Las Vegas, so it was inevitable that I would grow up in
                                   and around the giant resorts. For the first project, I paid homage to my hometown
                                   by using data on the Vegas Golden Knights. For this project, I figured I would do
                                   the same thing by using a hotels data set. The purpose of this app is to ", 
                                   tags$i("pass ST558 :)")),
                            h3("About the Data"),
                            tags$p("The data set used comes from a hotel located in Southern Portugal. The hotel is a resort 
                                    hotel located along the coast, which is a relatively popular European tourist
                                    destination. The data set has 31 variables and 40,000 records that correspond to different
                                    booking information. Each record corresponds to a booking from the period between July 1, 
                                    2015 and August 31, 2017. The data are in .csv format. No-shows are considered cancellations.
                                    Because of limitations on my computational power, I only used the first 5,000 records of 
                                    the data set."),
                            h3("Navigating the App"),
                            tags$p("This app has a few different tabs that the user can click to navigate to:"),
                            tags$ul(
                                tags$li("Exploration: the user can create common numerical and graphical summaries"),
                                tags$li("PCA: the user can specify aspects of a principal components analysis and see a
                                        biplot"),
                                tags$li("Modeling: the user can specify various aspects of two different supervised learning 
                                        models as well as get predictions"),
                                tags$li("Data: the user can scroll through the data, subset it, and save the data as
                                        a .csv file")
                            )
                            
                         )
                     )
                 )
        ), 
        tabPanel("Exploration", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h1("Data Exploration"),
                         br(),
                         h3("Graphical Summaries"),
                         radioButtons(inputId = "plottype", label = "Plot Type",
                                      choiceValues = c("hist", "bar", "scat"),
                                      choiceNames = c("Histogram", "Bar Plot", "Scatter Plot")
                         ),
                                      conditionalPanel("input.plottype == 'hist'",
                                                       selectInput(inputId = "histvar", 
                                                                   label = "Variable for Histogram", 
                                                                   choices = c("Average Daily Rate" = "adr",
                                                                               "Days between booking and stay" = "leadtime",
                                                                               "# of weekday stays" = "weekstays")),
                                                       sliderInput("bins", "Number of Bins", 
                                                                   min = 20, max = 150, value = 30),
                                                       numericInput("maxBins", label = "Set Maximum Number of Bins",
                                                                    value = 100, min = 50, max = 150)
                                      ),
                                      conditionalPanel("input.plottype == 'bar'",
                                                      selectInput(inputId = "barvar", 
                                                                  label = "Variable for Bar Plot", 
                                                                  choices = c("Assigned Room Type" = "rmtype",
                                                                              "Customer Type" = "custtype", 
                                                                              "Market Segment" = "mrkt")),
                                                      checkboxInput(inputId = "barcolor", 
                                                                    label = "Group by Cancellations?")
                                      ),
                                      conditionalPanel("input.plottype == 'scat'",
                                                       checkboxInput(inputId = "scatcolor", 
                                                                     label = "Color by Cancellations?")
                                      ),
                         br(),
                         h4("*Numerical summaries automatically generated for selected variable(s)"),
                         br(),
                         h4("Data Set"),
                         selectInput(inputId = "exploreFunc", label = "What (numeric) summary want to see?",
                                     selected = "dat",
                                     choices = c("Means" = "avg", 
                                                 "Standard Deviations" = "sd", 
                                                 "Just the data" = "dat")),
                                     conditionalPanel("input.exploreFunc == 'dat'",
                                                      checkboxGroupInput(inputId = "xplrSub", label = "Choose Variables to See",
                                                                         selected = c("IsCanceled", "LeadTime", "StaysInWeekNights", 
                                                                                      "CustomerType", "AssignedRoomType",        
                                                                                      "MarketSegment", "ADR"),
                                                                         choices = c("IsCanceled", "LeadTime", "StaysInWeekNights", 
                                                                                     "CustomerType", "AssignedRoomType",        
                                                                                     "MarketSegment", "ADR"))
                                     ),
                                     conditionalPanel("input.exploreFunc != 'dat'",
                                                      checkboxGroupInput(inputId = "exploreNumSub", label = "Choose Variables to See",
                                                                         selected = c("LeadTime", "StaysInWeekNights", "ADR"),
                                                                         choices = c("LeadTime", "StaysInWeekNights", "ADR"))
                         ),
                         
                    ),
                    mainPanel(h1("Graphical Summary"),
                              plotlyOutput("explorePlot"),
                              br(),
                              h1("Numerical Summaries"),
                              dataTableOutput("exploreSummary"),
                              h1("Data Set"),
                              dataTableOutput("exploreData")
                    )          
                )
                    
        ),
        tabPanel("PCA", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel( )
                 )
        ),
        tabPanel("Modeling", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel( )
                 )
        ),
        tabPanel("Data", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel( )
                 )
        )
    )
))