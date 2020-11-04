#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observe({updateSliderInput(session, "bins", max = input$maxBins)})

    output$explorePlot <- renderPlotly({
        if(input$plottype == "hist"){
            # Create histogram
            if(input$histvar == "adr"){
                plot_ly(x = ~hotel$ADR,  type = "histogram", nbinsx = input$bins) %>%
                    layout(xaxis = list(title = list(text='<b> Average Daily Room Rate </b>')),
                           yaxis = list(title = list(text='<b> Frequency </b>')))
            } else if(input$histvar == "leadtime"){
                plot_ly(x = ~hotel$LeadTime, type = "histogram", nbinsx = input$bins) %>%
                    layout(xaxis = list(title = list(text='<b> Days Between Booking and Stay </b>')),
                           yaxis = list(title = list(text='<b> Frequency </b>')))
            } else {
                plot_ly(x = ~hotel$StaysInWeekNights, type = "histogram", nbinsx = input$bins) %>%
                    layout(xaxis = list(title = list(text='<b> Number of Stays on Week Nights </b>')),
                           yaxis = list(title = list(text='<b> Frequency </b>')))
            }
        } else if(input$plottype == "bar"){
            # Create bar plot (no grouping)
            if(input$barcolor == FALSE){   
                if(input$barvar == "rmtype"){
                    plot_ly(data = hotel, x = ~AssignedRoomType, type = "histogram") %>%
                        layout(xaxis = list(title = list(text='<b> Assigned Room Type </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')))
                } else if(input$barvar == "custtype"){
                    plot_ly(data = hotel, x = ~CustomerType, type = "histogram") %>%
                        layout(xaxis = list(title = list(text='<b> Customer Type </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')))
                } else {
                    plot_ly(data = hotel, x = ~MarketSegment, type = "histogram") %>%
                        layout(xaxis = list(title = list(text='<b> Market Segment </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')))
                }
            } else {
                # Group bars by IsCancelled
                if(input$barvar == "rmtype"){
                    plot_ly(data = hotel, x = ~AssignedRoomType, type = "histogram", 
                            color = ~as.factor(IsCanceled), colors = c("#5ab4ac", "#d8b365")) %>%
                        layout(xaxis = list(title = list(text='<b> Assigned Room Type </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')),
                               legend = list(x = 0.8, y = 0.9, title=list(text='<b> Canceled? </b>'))) 
                } else if(input$barvar == "custtype"){
                    plot_ly(data = hotel, x = ~CustomerType, type = "histogram", 
                            color = ~as.factor(IsCanceled), colors = c("#5ab4ac", "#d8b365")) %>%
                        layout(xaxis = list(title = list(text='<b> Customer Type </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')),
                               legend = list(x = 0.1, y = 0.9, title=list(text='<b> Canceled? </b>')))
                } else {
                    plot_ly(data = hotel, x = ~MarketSegment, type = "histogram",
                            color = ~as.factor(IsCanceled), colors = c("#5ab4ac", "#d8b365")) %>%
                        layout(xaxis = list(title = list(text='<b> Market Segment </b>')),
                               yaxis = list(title = list(text='<b> Frequency </b>')),
                               legend = list(x = 0.1, y = 0.9, title=list(text='<b> Canceled? </b>')))
                }
            }
        } else {
            # Create scatter plot
            if(input$scatcolor == FALSE){
                plot_ly(data = hotel, x = ~ADR, y = ~LeadTime, type = "scatter",
                        mode = "markers", marker = list(size = 4)) %>%
                    layout(xaxis = list(title = list(text='<b> Average Daily Room Rate </b>')),
                           yaxis = list(title = list(text='<b> Days Between Booking and Stay </b>')))
            } else {
                plot_ly(data = hotel, x = ~ADR, y = ~LeadTime, type = "scatter",
                        mode = "markers", marker = list(size = 4),
                        color = ~as.factor(IsCanceled), colors = c("#5ab4ac", "#d8b365")) %>%
                    layout(xaxis = list(title = list(text='<b> Average Daily Room Rate </b>')),
                           yaxis = list(title = list(text='<b> Days Between Booking and Stay </b>')),
                           legend = list(x = 0.8, y = 0.9, title=list(text='<b> Canceled? </b>')))  
            }
        }
    })
    
    output$exploreSummary <- renderDataTable({
        if(input$plottype == "hist"){
            if(input$histvar == "adr"){
                hotel %>% dplyr::select(ADR, IsCanceled) %>% group_by(IsCanceled) %>%
                  summarize(Min = min(ADR), Q1 = quantile(ADR, .25),
                            Median = median(ADR), Mean = round(mean(ADR), 2), 
                            Q3 = quantile(ADR, .75), Max = max(ADR)) %>% 
                    datatable(rownames = FALSE, class = "compact")
            } else if(input$histvar == "leadtime"){
                hotel %>% dplyr::select(LeadTime, IsCanceled) %>% group_by(IsCanceled) %>%
                    summarize(Min = min(LeadTime), Q1 = quantile(LeadTime, .25),
                              Median = median(LeadTime), Mean = round(mean(LeadTime), 2), 
                              Q3 = quantile(LeadTime, .75), Max = max(LeadTime)) %>% 
                    datatable(rownames = FALSE, class = "compact")
            } else {
                hotel %>% dplyr::select(StaysInWeekNights, IsCanceled) %>% group_by(IsCanceled) %>%
                    summarize(Min = min(StaysInWeekNights), Q1 = quantile(StaysInWeekNights, .25),
                              Median = median(StaysInWeekNights), Mean = round(mean(StaysInWeekNights), 2), 
                              Q3 = quantile(StaysInWeekNights, .75), Max = max(StaysInWeekNights)) %>% 
                    datatable(rownames = FALSE, class = "compact")
            }
        } else if(input$plottype == "bar"){
            if(input$barvar == "rmtype"){
                table(hotel$AssignedRoomType, hotel$IsCanceled) %>% 
                    datatable(rownames = FALSE, class = "compact", 
                              colnames = c("Assigned Room Type", "Is Canceled", "Count"))
            } else if(input$barvar == "custtype"){
                table(hotel$CustomerType, hotel$IsCanceled) %>% 
                    datatable(rownames = FALSE, class = "compact",
                              colnames = c("Customer Type", "Is Canceled", "Count"))
            } else {
                table(hotel$MarketSegment, hotel$IsCanceled) %>%
                    datatable(rownames = FALSE, class = "compact",
                              colnames = c("Market Segment", "Is Canceled", "Count"))
            }
        } else {
            hotel %>% select(ADR, LeadTime, IsCanceled) %>% group_by(IsCanceled) %>%
                summarize(Covariance = round(cov(ADR, LeadTime), 4), 
                          Correlation = round(cor(ADR, LeadTime), 4)) %>% 
                datatable(rownames = FALSE, class = "compact")
        }
    })
})

summary(hotel$ADR)
