#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Input DowJones data 
source("./StockAnalysis_v3.R")
namesdatabase<-names(database)[-c(1,5)]

# Shiny UI

shinyUI(fluidPage(
  titlePanel("Market Analysis: Top 30 DowJones Stock"),
  sidebarLayout(
    sidebarPanel(
        selectInput('stock', 'Stock', unique(database$stock)),
        selectInput('parameter', 'Parameter', namesdatabase),
        strong(h4("Technical Metrics for the Stock:")),
        br(),
        strong(("20 day Moving Average:")),
        textOutput("TwentydayMovingAverage"),
        strong(("Long Term Moving Average:")),
        (textOutput("LongTermMovingAverage")),
        strong(("Volatility:")),
        textOutput("Volatility"),
        strong(("Short Term Liquidity:")),
        textOutput("Liquidity"),
        strong(("Short Term Reversal:")),
        textOutput("Reversal"),
        h4("Recommendation (Buy/Sell):"),
        strong(em(textOutput("Recommendation")))
    ),
    mainPanel(
      h4("Trend Analysis for the Stock:"),
      textOutput("text"),
      h4("Showing trends for selected parameter:"),
      textOutput("text1"),
      plotlyOutput("plot1"),
      br(),
      br(),
      h4("Instructions"),
      
      strong(("Introduction")),
      p("The StockAnalysis App display key parameters/metrics on top 30 DowJones Stocks"),
      p("The data for the project is taken from UCI database:"),
      a("https://archive.ics.uci.edu/ml/datasets/Dow+Jones+Index"),
      br(""),
      
      strong("What the App does?"),
      p("The App display trends on key parameters/metrics, selected from the 
         parameter drop down in the side panel, for any stock selected from the 30 stocks
        available in the drop down in the side panel."),
      p("Basis the two selections, the App autocalculates key technical metrics
        such as 20 day moving average, long term moving average, volatility, 
        Short term liquidity and Short term reversal indicators. The definitions for the same
        are available in the below link."),
      br(),
      
      a("http://www.thertrader.com/2014/02/28/using-cart-for-stock-market-forecasting/"),
      br(),
      
      p("And lastly, there is a buy/sell recommendation on the stock basis the 
        technical parameters that has been listed. If the current price of the stock
        selected is higher than the 20 day moving average as well as the long term 
        moving average, and the volatility index is less than .5, the app will 
        recommend buy on the stock on purely technical terms. There is no
        application of fundamental analysis on the stock."),
      br(),
    
      strong("Conclusion"),
      p("The analysis shows buy recommendation on three stocks:",
        span("IBM, MCD",style="color:blue"), "and",span ("UTX", style="color:blue"),"basis the technical analysis done"),
      p(" The next step on this project would be to link the application to 
        real time data and develop a more evolved recommendation scheme to identify
        high gain potential stocks on real time basis."),
      br()
      )
  )
))