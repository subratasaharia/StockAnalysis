#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Input DowJones data 
source("./StockAnalysis_v3.R")

# Shiny Server
shinyServer(function(input, output) {
  
  output$text <- renderText(input$stock)
  output$text1 <- renderText(input$parameter)
  
# Reactive functions  
  getyparameter <- reactive({
    yparameter<-input$parameter
    yparameter
     })
  
  getstock <- reactive({
    stock<-input$stock
    stock
  })

# Output functions
  
  output$TwentydayMovingAverage <- renderText({
    stock=getstock()
    round(NewTrainDatabase[NewTrainDatabase$Stock==stock,]$TwentyDayMovingAverage,2)
    
  })
  
  output$LongTermMovingAverage <- renderText({
    stock=getstock()
    round(NewTrainDatabase[NewTrainDatabase$Stock==stock,]$LongTermMovingAverage,2)
  })
  
  output$Volatility <- renderText({
    stock=getstock()
    NewTrainDatabase[NewTrainDatabase$Stock==stock,]$volatility
  })
  
  output$Liquidity <- renderText({
    stock=getstock()
    if(NewTrainDatabase[NewTrainDatabase$Stock==stock,]$ShortTermLiquidity)
      print("Liquidity is High")
    else
      print("Liquidity is Low")
  })
  
  output$Reversal <- renderText({
    stock=getstock()
    if(NewTrainDatabase[NewTrainDatabase$Stock==stock,]$ShortTermReversal)
      print("Stock displaying Higher Highs and Higher Lows in Short Term")
    else
      print("Stock not displaying Higher Highs and Higher Lows")
    
  })
  
  output$Recommendation <- renderText({
    stock=getstock()
    NewTrainDatabase[NewTrainDatabase$Stock==stock,]$Recommend
  })
  
  output$plot1 <- renderPlotly({
    
    stock=getstock()
    database<-database[database$stock==stock,]
    parameter<-database[,getyparameter()]
    plot_ly(database, x = ~weeklyindex, y = ~parameter,type = "scatter",mode="lines")
    
    })
})
  
  
