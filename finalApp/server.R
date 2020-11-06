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
hotel <- hotel[1:5000, covs] %>% dplyr::filter(AssignedRoomType != "P")
hotel$IsCanceled <- hotel$IsCanceled %>% as.factor()

# Define train control for knn model
trctrl <- trainControl(method = "cv", number = 5)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
# DATA EXPLORATION
    # Reactive Stuff
    observe({updateSliderInput(session, "bins", max = input$maxBins)})
    
    # Dr. Post's Button
    observeEvent(input$posty, {
        toggle('funtext')
        output$postText <- renderUI({
            HTML(paste("Thanks for the McFLEURY :)", "#GoKnightsGo", 
                       "Re-click button to hide me!",
                       sep = "<br/>"))
                 })
    })
    
    # Graphical Summary Plots
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
    # Numeric Summary Data Table
    output$exploreSummary <- renderDataTable({
        if(input$plottype == "hist"){
            if(input$histvar == "adr"){
                hotel %>% dplyr::select(ADR, IsCanceled) %>% dplyr::group_by(IsCanceled) %>%
                  dplyr::summarize(Min = min(ADR), Q1 = quantile(ADR, .25),
                                   Median = median(ADR), Mean = round(mean(ADR), 2), 
                                   Q3 = quantile(ADR, .75), Max = max(ADR)) %>% 
                    datatable(rownames = FALSE, class = "compact")
            } else if(input$histvar == "leadtime"){
                hotel %>% dplyr::select(LeadTime, IsCanceled) %>% dplyr::group_by(IsCanceled) %>%
                    dplyr::summarize(Min = min(LeadTime), Q1 = quantile(LeadTime, .25),
                                     Median = median(LeadTime), Mean = round(mean(LeadTime), 2), 
                                     Q3 = quantile(LeadTime, .75), Max = max(LeadTime)) %>% 
                    datatable(rownames = FALSE, class = "compact")
            } else {
                hotel %>% dplyr::select(StaysInWeekNights, IsCanceled) %>% dplyr::group_by(IsCanceled) %>%
                    dplyr::summarize(Min = min(StaysInWeekNights), Q1 = quantile(StaysInWeekNights, .25),
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
            hotel %>% dplyr::select(ADR, LeadTime, IsCanceled) %>% dplyr::group_by(IsCanceled) %>%
                dplyr::summarize(Covariance = round(cov(ADR, LeadTime), 4), 
                                 Correlation = round(cor(ADR, LeadTime), 4)) %>% 
                datatable(rownames = FALSE, class = "compact")
        }
    })
    # Dataset Data Table
    output$exploreData <- renderDataTable({
        if(input$exploreFunc == "dat"){
            hotel[,input$xplrSub] %>% datatable(rownames = FALSE, class = "compact")
        } else if(input$exploreFunc == "avg"){
            hotel2 <- hotel[,c("ADR", "LeadTime", "StaysInWeekNights", "RequiredCarParkingSpaces")]
            colMeans(hotel2[,input$exploreNumSub]) %>% as.matrix() %>%
                round(4) %>% datatable(class = "compact", colnames = c("Mean"))
        } else {
            hotel2 <- hotel[,c("ADR", "LeadTime", "StaysInWeekNights", "RequiredCarParkingSpaces")]
            apply(hotel2[,input$exploreNumSub], 2, sd) %>% as.matrix() %>%
                round(4) %>% datatable(class = "compact", colnames = c("Standard Deviation"))
        }

    })
    
# UNSUPERVISED LEARNING
    # Title for unsupervised learning page
    output$unsuptitle <- renderUI({
        if(input$unsup == "pca"){
            HTML("<h2> Principal Components Analysis </h2>")
        } else {
            HTML("<h2> Cluster Analysis </h2>")
        }
    })
    # Description for the unsupervised method
    output$unsupdesc <- renderUI({
        if(input$unsup == "pca"){
            HTML("<p>Principal Components Analysis (PCA) is an unsupervised learning technique used for dimension reduction and
                 de-correlating the data. In PCA, we are looking to find linear combinations of the original variables that 
                 still retains as much of the total variation as possible. These linear combinations are called principal
                 components and are by constuction orthogonal (uncorrelated). We find the principal components using the 
                                      <a href='https://www.cse.iitk.ac.in/users/rmittal/prev_course/s14/notes/lec10.pdf'>
                                        <i>Spectral Decomposition,</i> </a> which is just as spooky as it sounds.</p>")
        } else {
            HTML("<p>Cluster Analysis is an unsupervised learning technique used for dimension reduction. Clustering tried to
                 find subgroups in the data. These groups in theory have membbers that are 'similar' to one another. For this 
                 project I will be using K-means clustering. </p>")
        }
    })
    # Visualization
    output$unsupplot <- renderPlotly({
        # Subset data for the plot
        hotel.pca <- hotel[,c("ADR", "LeadTime", "StaysInWeekNights", "RequiredCarParkingSpaces")]
        if(input$unsup == "pca"){
            # Fit PCA
            pca <- princomp(hotel.pca, scores=T, cor=T)
        
            # Scores
            scores <- pca$scores
            x <- scores[,1]
            y <- scores[,2]
            z <- scores[,3]
        
            # Loadings
            loads <- pca$loadings
        
            # Scale factor for loadings
            scale.loads <- 5
        
            # 3D plot
            p <- plot_ly() %>%
                 add_trace(x=x, y=y, z=z,
                           type="scatter3d", mode="markers",
                           marker = list(color=y, 
                                         colorscale = c("#FFE1A1", "#683531"), 
                                         opacity = 0.7)) 
            # Add the different traces
            for (k in 1:input$pcnum) {
                x <- c(0, loads[k,1])*scale.loads
                y <- c(0, loads[k,2])*scale.loads
                z <- c(0, loads[k,3])*scale.loads
                p <- p %>% add_trace(x=x, y=y, z=z,
                                     name = paste("PC", k),
                                     type="scatter3d", mode="lines",
                                     line = list(width=8),
                                     opacity = 1) 
            }
            print(p)
        } else {
            # 3D Cluster PLot
            df <- hotel.pca[,1:3]
            df$cluster <- factor(kmeans(df, input$clustk)$cluster)
            
            plot_ly(df, x =~ ADR, y =~ LeadTime, 
                    z =~ StaysInWeekNights, color =~ cluster) %>%
                    add_markers(size = 1.5)
        }
    })     
    # Table of model information
    output$ldgs <- renderTable({
        hotel.pca <- hotel[,c("ADR", "LeadTime", "StaysInWeekNights", "RequiredCarParkingSpaces")]
        if(input$unsup == "pca"){
            pca <- princomp(hotel.pca, scores=T, cor=T)
            l <- varimax(pca$loadings[, 1:input$pcnum])$loadings
            data.frame(matrix(as.numeric(l), attributes(l)$dim,
                              dimnames=attributes(l)$dimnames))
        } else {
            clstr <- kmeans(hotel.pca[,1:3], input$clustk)
            clstr$centers
        }
    })
# MODELING
    # Fit models reactively
    form <- reactive({
        paste("IsCanceled", "~", paste(input$preds, collapse = "+")) %>% as.formula()
    })
    treefit <- reactive({
        treefit <- rpart(form(), data = hotel, cp = input$cp)
    })
    logitfit <- reactive({
        glm(form(), data = hotel, family = binomial(link = "logit"))
    })
    
    # Model Description
    output$modeldesc <- renderUI({
        if(input$model == "tree"){
            HTML("<p>The basic classification tree is based on partitioning the data into subgroups using 
                 simple binary splitting. Initially, all objects are considered a single group. Then, the 
                 group is binarily split into two subgroups based on the criteria of a certain variable. We 
                 then classify the observations in a specific region with majority vote. We want to grow the 
                 tree as big as we can, and then prune it back using cost-complexity pruning. This is done to 
                 not overfit the data, but pruning increases the bias. The pruning parameter needs to get 
                 tuned, which is done automatically in the `caret` package.</p> <p> The advantage 
                 of using a basic classification is that it is easy to understand and has a good interpretability.
                 Additionally, the basic classification tree is not very computationally expensive, unlike 
                 the random forest and the support vector machine. We used the rpart package in R to 
                 fit our classification trees.</p>")
        } else {
            withMathJax(
                helpText("The logistic regression model uses the 'logit' function, which links the 
                mean to the linear form of the regression model. Using it for binary classification,
                we round the fitted values either up or down to 1 or 0. The logistic regression function is defined as  
                $$\\ln \\left( \\frac{p(x)}{1-p(x)} \\right) = x'\\beta, \\qquad \\text{where }p(x) = (1+e^{-x'\\beta})$$
                The logistic regression model assumes that each observation is independent, that there is little or no 
                multicollinearity among the predictors, and that there is a linear relationship between the predictor 
                variables and log odds. This differs from the typical regression model, where the residuals must be normally 
                distributed and have constant variance.")
            )
        }
    })
    # Create plots reactively for saving
    tree.base <- reactive({
        plot(treefit(), main = "Tree Visualization")
        text(treefit())
    })
    tree.upgrade <- reactive({
        rpart.plot(treefit(), main = "Tree Visualization")
    })
    res.plot <- reactive({
        plot(residuals(logitfit()), main = "Residual Plot",
             ylab = "Residuals")
    })
    pp.out <- reactive({
        if(input$model == "tree"){
            if(input$treeupgrade == FALSE){
                pp <- tree.base()
            } else {
                pp <- tree.upgrade()
            }
        } else if(input$model == "logit"){
            pp <- res.plot()
        }
        print(pp)
    })
    # Model Plots
    output$modelplot <- renderPlot({
        if(input$model == "tree"){
            if(input$treeupgrade == FALSE){
                plot(treefit(), main = "Tree Visualization")
                text(treefit())
            } else {
                rpart.plot(treefit(), main = "Tree Visualization")
            }
        } else if(input$model == "logit"){
            plot(residuals(logitfit()), main = "Residual Plot",
                 ylab = "Residuals")
        }
    })
    # Save button for plots
    output$saveplot <- downloadHandler(
        file = "plot.png",
        content = function(file){
            png(file = file)
            pp.out()
            dev.off()
        })
    
    # Predictions
    output$pred <- renderText({
        df <- data.frame(LeadTime = input$LeadTimeInput, 
                         StaysInWeekNights = input$StaysInWeekNightsInput,
                         CustomerType = input$CustomerTypeInput, 
                         AssignedRoomType = input$AssignedRoomTypeInput,
                         RequiredCarParkingSpaces = input$RequiredCarParkingSpacesInput,
                         DepositType = input$DepositTypeInput,
                         MarketSegment = input$MarketSegmentInput,
                         ADR = input$ADRInput)
        if(input$model == "tree"){
            pr <- predict(treefit(), newdata = df, type = "vector")
            if (pr[1] == 1){pr[2] <- "CANCELLATION"} else {pr[2] <- "NO CANCELLATION"}
        } else {
            pr <- predict(logitfit(), newdata = df, type = "response") %>% round()
            if (pr[1] == 0){pr[2] <- "CANCELLATION"} else {pr[2] <- "NO CANCELLATION"}
        }
        paste0("The prediction for the set of inputs is: ", pr[2])
    })
# DATA SET
    # Set rows and columns reactively
    data.out <- reactive({
        hotel[input$rows[1]:input$rows[2], input$cols]
    })
    output$fulldata <- renderDataTable({
        data.out()
    })
    output$download <- downloadHandler(
        filename = function(){"hotel-data.csv"}, 
        content = function(fname){
            write.csv(data.out(), fname)
        })
})
