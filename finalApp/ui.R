#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tools)
library(caret)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tabsetPanel(
        tabPanel("Information", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         h2("ST558 Final Project"),
                         h3("By: Mana Azizsoltani"),
                         br(),
                         h4("Links"),
                         tags$div(
                             tags$ul(
                                 tags$li(a(href = "https://github.com/manaaziz/ST558-RProj3", "REPOSITORY")),
                                 tags$li(a(href = "https://manaaziz.github.io", "BLOG")),
                                 tags$li(a(href = "https://www.linkedin.com/in/manaazizsoltani/", "LINKEDIN"))
                             )
                         )
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
                                   the same thing by using a hotels data set. ", 
                                   tags$b("The purpose of this app is to pass ST558 :)")),
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
                     sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                     mainPanel( )
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