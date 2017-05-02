install.packages("zoo")
library(zoo)
install.packages("dplyr")
library(dplyr)
install.packages("sqldf")
library(sqldf)
install.packages("tcltk")
library(tcltk)
install.packages("forecast")
library(forecast)


carsales <- read.csv(file.choose(), header = TRUE)
#exploring the dataset
summary(carsales)
str(carsales)
which(is.na(carsales$Make))
#since the quantity for NA is very less omiting those rows 
carsales<- na.omit(carsales)
nrow(carsales)

#for the unique combinations of Make
unique(carsales$Make)

#orderby for carsales
carsales <- carsales[order(carsales$Make),]
#adding date in the dataset for grouping it
carsales$date <- paste(carsales$Year,carsales$Month)
carsales$date<- as.Date(paste(as.character(carsales$date), '01'), format='%Y%m%d')


df<- carsales
#making a list of unique car Make
Make_list <-as.data.frame (sort(unique(df$Make)))
df$date<- as.factor(df$date)
dataf<- NULL
output<- NULL 
df_total <- NULL
 

for(k in 1:nrow(Make_list)) {
#dividing the dataset into test and train sets
  trainquery <-  paste("select Make,Quantity,Year,Month from df where date between '2007-01-01' and '2016-01-01'  and make = '", Make_list[k,], "'" , sep = '')
  testquery <- paste("select Make,Quantity,Year,Month  from df where date between '2016-02-01' and '2017-01-01' and make = '", Make_list[k,], "'" , sep = '')
  train <- sqldf(trainquery,stringsAsFactors = FALSE)
  test<- sqldf (testquery,stringsAsFactors = FALSE)
 
  #if number of datapoints are less then jump to next level
   if(nrow(train)<12) {next}
  if(nrow(test)<12){next}
  
  #converting it into timeseries set
  training <- ts(train$Quantity,frequency=12)
  #removing the outliers 
  training<- tsclean(training)
 
###################################################################################
  #Time series Models 
  #ETS MODEL 
  ets.fit=ets(training)
  fc.ets = forecast(ets.fit,h=12)
  ets.fitted <- data.frame(as.numeric(ets.fit$fitted))
  ets.predict <- data.frame(as.numeric(fc.ets$mean))
  accuracy.ets <- data.frame(accuracy(fc.ets, test$Quantity))
  
  #Seasonal Naive
  snaive.fit = snaive(training)
  fc.snaive = forecast(snaive.fit,h =12)
  snaive.fitted <- data.frame(as.numeric(snaive.fit$fitted))
  fc.predict <- data.frame(as.numeric(fc.snaive$mean))
  accuracy.snaive<- data.frame(accuracy(fc.snaive,test$Quantity ))
  
  #Theta
  thetaf.fit<- thetaf(training,fan= FALSE)
  thetaf.f<-  forecast(thetaf.fit, h= 12)
  thetaf.fitted <- data.frame(as.numeric(thetaf.fit$fitted))
  thetaf.predict <- data.frame(as.numeric(thetaf.f$mean))
  accuracy.theta <- data.frame(accuracy(thetaf.f, test$Quantity))
  
  
  #Holt's Winter
  hw.fit <- hw(training)
  hw.fc <- forecast(hw.fit,h=12)
  hw.fitted <- data.frame(as.numeric(hw.fit$fitted))
  hw.predict <-data.frame(as.numeric( hw.fc$mean))
  accuracy.hw<- data.frame (accuracy(hw.fc,test$Quantity))

  #Preparing dataset for ensemble model
  trainset<-  data.frame(train$Quantity)
  testset<-  data.frame(test$Quantity)
  
 ensemble.set.train<- cbind((trainset[13:nrow(trainset),]),ets.fitted[13:nrow(ets.fitted),],snaive.fitted[13:nrow(snaive.fitted),], thetaf.fitted [13:nrow(thetaf.fitted ),],hw.fitted[13:nrow(hw.fitted),] )
 colnames( ensemble.set.train)[c(1,2,3,4,5)] <- c("Quantity","ets.fitted","snaive.fitted","thetaf.fitted" ,"hw.fitted" )
 
 ensemble.set.test<- cbind(testset, ets.predict , fc.predict,thetaf.predict,hw.predict)
 colnames( ensemble.set.test)[c(1,2,3,4,5)] <- c("Quantity","ets.fitted","snaive.fitted","thetaf.fitted" ,"hw.fitted" )

 #using linear regression model 
 lm_model <- lm(Quantity ~ ., data = as.data.frame(ensemble.set.train) )

 
 final.predict <-data.frame(predict(lm_model, ensemble.set.test))
#storing the forecast in output dataframe 
 output<- cbind(test, ets.predict, fc.predict, thetaf.predict,hw.predict,final.predict)
 colnames(output)[c(5,6,7,8,9)] <- c("ets.predict", "fc.predict","thetaf.predict","hw.predict","final.predict" )
###the code can be used for predicting future sales in Norway 

  df_total <- rbind(output,df_total) 
}
 
  
#saving the output in a csv 
write.csv(df_total, 'c:\\Carsales.csv')



