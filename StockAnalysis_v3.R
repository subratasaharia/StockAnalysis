library(plotly)
library(dplyr)
library(randomForest)
library(lubridate)

#dowjones<-read.csv("./Coursera/dow_jones_index/dow_jones_index.data",stringsAsFactors = FALSE)
dowjones<-read.csv("./dow_jones_index/dow_jones_index.data",stringsAsFactors = FALSE)

good<-complete.cases(dowjones)
dowjonessubgood<-dowjones[good,]

dowjonessubgoodsub<-dowjonessubgood[,c(2,3,4,5,6,7,8,10,14)]

stocks<-unique(dowjonessubgoodsub$stock)

conversion<-function(x){
  x<-substr(x,2,nchar(x))
  x
}

i<-1

database<-as.data.frame(NULL)
while(i<27){
  
  dowjonessubgoodsub1<-subset(dowjonessubgoodsub,stock==stocks[i]|stock==stocks[i+1]|stock==stocks[i+2]|stock==stocks[i+3]|stock==stocks[i+4])
  
  weeklyindex<-strptime(dowjonessubgoodsub1$date,"%m/%d/%Y")
  ClosingStockPrice<-as.numeric(conversion(dowjonessubgoodsub1$close))
  OpeningStockPrice<-as.numeric(conversion(dowjonessubgoodsub1$open))  
  HighStockPrice<-as.numeric(conversion(dowjonessubgoodsub1$high))
  LowStockPrice<-as.numeric(conversion(dowjonessubgoodsub1$low))
  
  dowjonessubgoodsub1<-cbind(dowjonessubgoodsub1,weeklyindex=weeklyindex,
                             ClosingStockPrice=ClosingStockPrice,
                             OpeningStockPrice=OpeningStockPrice,
                             HighStockPrice=HighStockPrice,
                             LowStockPrice=LowStockPrice)
  
  database<-rbind(database,dowjonessubgoodsub1)
  
  i<-i+5
}

#Trim the database , removing unnecessary columns
database<-database[,-c(2,3,4,5,6)]

#Divide the data into train and test set

#databasetrain<-database[weeklyindex<"2011-06-03",]

#databasetest<-database[weeklyindex>"2011-05-27",]

# Defining new variables to predict stock movement

TwentyDayMovingAverage<-function(dataset){
  max<-max(dataset$weeklyindex)
  min<-min(dataset$weeklyindex)
  diff<-as.numeric(max-min)
  
  if(diff>20)
    daterange<-max-60*60*24*22
  else
    daterange<-max-60*60*24*round(diff/3,0)
  
  mean(dataset[dataset$weeklyindex>daterange,]$ClosingStockPrice)
}

LongTermMovingAverage<-function(dataset){
  mean(dataset$ClosingStockPrice)
}

volatility<-function(dataset){
  max<-max(dataset$weeklyindex)
  min<-min(dataset$weeklyindex)
  diff<-as.numeric(max-min)
  
  if(diff>20)
    daterange<-max-60*60*24*22
  else
    daterange<-max-60*60*24*round(diff/3,0)
  
  dataset<-dataset[dataset$weeklyindex>daterange,]
  
  MAvg<-mean(dataset$ClosingStockPrice)
  AvgSpread<-mean(dataset$HighStockPrice-
                    dataset$LowStockPrice)
  round(AvgSpread/MAvg,2)
}


ShortTermReversal<-function(dataset){
  max<-max(dataset$weeklyindex)
  min<-min(dataset$weeklyindex)
  diff<-as.numeric(max-min)
  
  if(diff>20)
    daterange<-max-60*60*24*22
  else
    daterange<-max-60*60*24*round(diff/3,0)
  
  dataset<-dataset[dataset$weeklyindex>daterange,]
  rows<-nrow(dataset)
  
  higherhighlow<-as.logical(TRUE)
  
  for( i in 2:rows){
    if(dataset$HighStockPrice[i]-dataset$HighStockPrice[i-1]<0)
      higherhighlow<-FALSE
    if(dataset$LowStockPrice[i]-dataset$LowStockPrice[i-1]<0)
      higherhighlow<-FALSE
    
  }     
  higherhighlow
  
}

ShortTermLiquidity<-function(dataset){
  max<-max(dataset$weeklyindex)
  min<-min(dataset$weeklyindex)
  diff<-as.numeric(max-min)
  
  if(diff>20)
    daterange<-max-60*60*24*22
  else
    daterange<-max-60*60*24*round(diff/3,0)
  
  dataset<-dataset[dataset$weeklyindex>daterange,]
  rows<-nrow(dataset)
  
  highervolume<-as.logical(TRUE)
  for( i in 1:rows){
    
    if(dataset$percent_change_volume_over_last_wk[i]<0)
      highervolume<-FALSE
  }
  
  highervolume
}

LastWeekPrice<-function(dataset){
  max<-max(dataset$weeklyindex)
  ClosePrice<-dataset[dataset$weeklyindex==max,]$ClosingStockPrice
  ClosePrice
  
}

NextWeekPricechange<-function(dataset){
  NextWeekPricechangeInd<-as.logical(FALSE)
  max<-max(dataset$weeklyindex)
    if(dataset[dataset$weeklyindex==max,]$percent_change_next_weeks_price>0)
      NextWeekPricechangeInd<-TRUE
    NextWeekPricechangeInd
}

# Createnew Database for prediction algorithm

createdatabase<-function(dataset){
  SplitTrain<-split(dataset,dataset$stock)
  TransformTrain<-data.frame(TwentyDayMovingAverage=sapply(SplitTrain,TwentyDayMovingAverage),
                             LongTermMovingAverage=sapply(SplitTrain,LongTermMovingAverage),
                             LastWeekPrice=sapply(SplitTrain,LastWeekPrice),
                             ShortTermReversal=sapply(SplitTrain,ShortTermReversal),
                             ShortTermLiquidity=sapply(SplitTrain,ShortTermLiquidity),
                             volatility=sapply(SplitTrain,volatility)
                             #NextWeekPricechangeInd=sapply(SplitTrain,NextWeekPricechange
                            )
  StockNames<-row.names(TransformTrain)
  TransformTrain<-TransformTrain%>%mutate(#TwentyDayMovingAverageInd=
                                            #ifelse(LastWeekPrice>TwentyDayMovingAverage,TRUE,FALSE),
                                          Stock=StockNames)
  TransformTrain
}

#databasetrain<-databasetrain%>%mutate(Month=month(weeklyindex))

#SplitMonth<-split(databasetrain,databasetrain$Month)
#NumberofMonths<-max(databasetrain$Month)
#NewTrainDatabase<-data.frame(NULL)

#for( i in 1:NumberofMonths){
  NewTrainDatabase<-createdatabase(database)
  NewTrainDatabase<-NewTrainDatabase%>%
                              mutate(Recommend=
                                ifelse(LastWeekPrice>TwentyDayMovingAverage, 
                                ifelse(LastWeekPrice>LongTermMovingAverage,
                                ifelse(volatility<.05, "Buy","Sell"),"Sell"),"Sell"))
#}

#SplitStock<-split(NewTrainDatabase,NewTrainDatabase$Stock)
#NewTrainDatabase<-data.frame(NewTrainDatabase,LongTermMovingAverage=sapply(SplitStock,LongTermMovingAverage))

#NewTrainDatabase<-NewTrainDatabase[,-c(1,2,3)]  

# Rearranging for training
#NewTrainDatabase<-cbind(NextWeekPricechangeInd=NewTrainDatabase[,c(4)],NewTrainDatabase[,-c(4)])


#model<-randomForest(as.factor(NextWeekPricechangeInd)~.,data=NewTrainDatabase[,-c(7)],
#             importance=TRUE)

#plot_ly(database[1:120,], x = ~weeklyindex, y = ~ClosingStockPrice,type = "scatter",mode="lines",color=~stock)
