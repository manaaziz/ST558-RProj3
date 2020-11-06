# Load necessary libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(caret)
library(plotly)
library(DT)
library(ggfortify)
library(MASS)
library(rpart.plot)
library(rpart)
library(class)

# Read in data and subset it
hotel <- read_csv("../H1.csv")
covs <- names(hotel)[c(1,2,8,26,20,28,22,14,27)]
hotel <- hotel[1:5000, covs]
hotel$IsCanceled <- hotel$IsCanceled %>% as.factor()

# Define UI for application that draws a histogram
shinyUI(fluidPage(useShinyjs(), withMathJax(),
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
                         actionButton(inputId = "posty", label = "The 'Post' Button"),
                         hidden(div(id='funtext', htmlOutput("postText")))
                     ),
                     mainPanel(
                         h2("Welcome to my app!"),
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
                                tags$li("Unsupervised: the user can specify aspects of a principal components analysis or cluster analysis
                                        and see visualizations."),
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
                         h2("Data Exploration"),
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
                                                                                      "DepositType", "CustomerType", "AssignedRoomType",
                                                                                      "RequiredCarParkingSpaces",        
                                                                                      "MarketSegment", "ADR"),
                                                                         choices = c("IsCanceled", "LeadTime", "StaysInWeekNights", 
                                                                                     "DepositType", "CustomerType", "AssignedRoomType",
                                                                                     "RequiredCarParkingSpaces",        
                                                                                     "MarketSegment", "ADR"))
                                     ),
                                     conditionalPanel("input.exploreFunc != 'dat'",
                                                      checkboxGroupInput(inputId = "exploreNumSub", label = "Choose Variables to See",
                                                                         selected = c("LeadTime", "StaysInWeekNights", "ADR",
                                                                                      "RequiredCarParkingSpaces"),
                                                                         choices = c("LeadTime", "StaysInWeekNights", "ADR",
                                                                                     "RequiredCarParkingSpaces"))
                         ),
                         
                    ),
                    mainPanel(h2("Graphical Summary"),
                              plotlyOutput("explorePlot"),
                              br(),
                              h2("Numerical Summaries"),
                              dataTableOutput("exploreSummary"),
                              h2("Data Set"),
                              dataTableOutput("exploreData")
                    )          
                )
                    
        ),
        tabPanel("Unsupervised", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(radioButtons("unsup", "Unsupervised Method", 
                                               choiceNames = c("PCA", "Cluster"),
                                               choiceValues = c("pca", "clust")),
                                  conditionalPanel("input.unsup == 'pca'",
                                                   numericInput(inputId = "pcnum", "Number of PCs to Display", 
                                                                min = 2, max = 4, value = 2)),
                                  conditionalPanel("input.unsup == 'clust'",
                                                   numericInput(inputId = "clustk", "Choose k",
                                                                min = 3, max = 6, value = 3))
                     
                     ),
                     mainPanel(uiOutput("unsuptitle"),
                               uiOutput("unsupdesc"),
                               h3("Visualization"),
                               plotlyOutput("unsupplot"),
                               h3("Analysis Output"),
                               tableOutput("ldgs")
                               
                               
                               
                     )
                 )
        ),
        tabPanel("Modeling", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(h2("Modeling"),
                                  radioButtons(inputId = "model", label = "Select Model",
                                              selected = "tree",
                                              choices = c("Classification Tree" = "tree",
                                                          "Logistic Regression" = "logit")),
                                  checkboxGroupInput(inputId = "preds", label = "Choose Predictors",
                                                     selected = c("LeadTime", "StaysInWeekNights", 
                                                                  "DepositType", "CustomerType", "AssignedRoomType",
                                                                  "RequiredCarParkingSpaces",        
                                                                  "MarketSegment", "ADR"),
                                                     choices = c("LeadTime", "StaysInWeekNights", 
                                                                 "DepositType", "CustomerType", "AssignedRoomType",
                                                                 "RequiredCarParkingSpaces",
                                                                 "MarketSegment", "ADR")),
                                  conditionalPanel("input.model == 'tree'",
                                                   sliderInput("cp", "Value of Cp",
                                                               min = .01, value = .0565, max = .065, step = .005),
                                                   checkboxInput("treeupgrade", "Upgrade Tree Plot?")),
                                  conditionalPanel("input.model =='knn'",
                                                   sliderInput("kval", "Select k",
                                                               min = 2, value = 8, max = 15)),
                                  h3("Prediction Inputs"),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("LeadTime") > -1',
                                                   numericInput("LeadTimeInput", label = "Lead Time", 
                                                                value = 0, min = 0, max = 737)),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("StaysInWeekNights") > -1',
                                                   numericInput("StaysInWeekNightsInput", label = "Weeknight Stays", 
                                                                value = 0, min = 0, max = 33)),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("DepositType") > -1',
                                                   selectInput("DepositTypeInput", label = "Deposit Type", 
                                                               choices = c("No Deposit", "Refundable", "Non Refund"))),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("CustomerType") > -1',
                                                   selectInput("CustomerTypeInput", label = "Customer Type", 
                                                               choices = c("Transient", "Contract", "Transient-Party", "Group"))),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("AssignedRoomType") > -1',
                                                   selectInput("AssignedRoomTypeInput", label = "Assigned Room Type",
                                                               choices = c("C", "A", "D", "E", "G", "F", "I", "B", "H"))),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("RequiredCarParkingSpaces") > -1',
                                                   numericInput("RequiredCarParkingSpacesInput", label = "Number of Parking Spaces", 
                                                                value = 0, min = 0, max = 2)),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("MarketSegment") > -1',
                                                   selectInput("MarketSegmentInput", label = "Market Segment", 
                                                               choices = c("Direct", "Corporate", "Online TA",
                                                                           "Offline TA/TO", "Complementary", "Groups"))),
                                  conditionalPanel(condition = 'input.preds && input.preds.indexOf("ADR") > -1',
                                                   numericInput("ADRInput", label = "Average Daily Rate", 
                                                                value = 0, min = 0, max = 332))
                    ),
                     mainPanel(h2("Model Description"),
                               uiOutput("modeldesc"),
                               plotOutput("modelplot"),
                               downloadButton("saveplot", "Save Plot"),
                               h2("Prediction Output"),
                               textOutput("pred"))
                 )
        ),
        tabPanel("Data", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(h2("Data"),
                                  sliderInput("rows", label = "Select Row Range", min = 1, 
                                              max = 5000, value = c(1, 50000)),
                                  checkboxGroupInput(inputId = "cols", label = "Select Columns",
                                                     selected = c("IsCanceled", "LeadTime", "StaysInWeekNights", 
                                                                  "DepositType", "CustomerType", "AssignedRoomType",
                                                                  "RequiredCarParkingSpaces",        
                                                                  "MarketSegment", "ADR"),
                                                     choices = c("IsCanceled", "LeadTime", "StaysInWeekNights", 
                                                                 "DepositType", "CustomerType", "AssignedRoomType",
                                                                 "RequiredCarParkingSpaces",
                                                                 "MarketSegment", "ADR")),
                                  downloadButton('download', "Download the data")
                     ),
                     mainPanel(h2("The Data Set"),
                               tags$p("This pages exists so that you, the user, can look through the data and download it."),
                               br(),
                               dataTableOutput("fulldata")
                               
                     )
                 )
        )
    )
))
