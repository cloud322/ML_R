# a0326<-read.csv('file:///C:/Users/DJA/Downloads/m_sam.csv', header = T, sep=',')
a0326<-read.csv('file:///C:/Users/DJA/Downloads/새 폴더/0327_ANSI.txt', header = F, sep=',')
a0326
head(a0326)

colnames(a0326)<-c('a','stock','date','price','pr_ch','vol_ch','vol')

install.packages("dplyr")
library(dplyr)


samsung0326<-a0326 %>% filter(stock == '삼성전자')
head(samsung0326)
#date package
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)

## Load the data from the csv file
d_p_sam<-samsung0326 %>% select(date, price)
plot(d_p_sam$date,d_p_sam$price,pch=16)


# help(strftime)
x  <-  d_p_sam$date
# x2  <-  select(d_p_sam$date>="2018-04-02 09:00:01 KST" & d_p_sam$date>="2018-04-02 11:00:01 KST")
# x2
# time <- substr(as.POSIXct(sprintf("%06.0f", x), format='%H%M%S'), 12, 19)
time <- as.POSIXct(sprintf("%06.0f", x), format='%H%M%S')
time

# to data frame  ((time,price))
time <- c(as.POSIXct(sprintf("%06.0f", x), format='%H%M%S'))
price <- c(d_p_sam$price)
df<- data.frame(time,price)
df

# d_p_sam2<-samsung0326 %>% select (as.POSIXct(sprintf("%06.0f", date), format='%H%M%S'), price)
# d_p_sam2

# ?timeSeries
# ?strptime
# time2<-format(as.character(x), format ='%H%M%S')
# time2
# d_p_sam$price

## Plot the data
# plot(time, d_p_sam$price,pch=13)

plot(df,pch=16,type='p',cex= 0.5)
abline(h=2500000)abline(h=2400000)
# plot(time2, d_p_sam$price)
# a<-ggplot(d_p_sam,aes(time2, price))
# a
# ?plot
# par(time2)
# ?par

# Create a linear regression model
# model <- lm(d_p_sam$price ~ time )
# model

model <- lm(price ~ time , df)
model
# Add the fitted line
abline(model)

# make a prediction for each X
# predictedY <- predict(model)
# predictedY 
# plot(predictedY )

predictedY <- predict(model, df)
predictedY
# display the predictions
points(df$time, predictedY, col = "blue", pch=16, cex=0.5)
plot(df$time, predictedY, col = "red", pch=16)

##SVM
#RMSE

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 10184.28
predictionRMSE
library(e1071)
model <- svm(price ~ time , df)
model
predictedY <- predict(model, df)

points(df$time, predictedY, col = "red", pch=4, cex=0.5)
summary(df)
summary(model)
summary(predictedY)
# svrModel$residuals  != data$Y - predictedY
# compute the error
error <- df$price - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE     #6343.204

###SVM tune
# perform a grid search
# tuneResult <- tune(svm, price ~ time,  data = df,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
# print(tuneResult)


# Parameter tuning of ‘svm’:
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   epsilon cost
# 0.5  512
# 
# - best performance: 31149900 

# 0.35~0.5

# Draw the tuning graph
# plot(tuneResult)

tuneResult <- tune(svm, price ~ time,  data = df, ranges = list(epsilon = seq(0.35,0.5,0.01), cost = 2^(2:9))) 

print(tuneResult)
plot(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, df) 

error <- data$Y - tunedModelY  


# https://www.svm-tutorial.com/2014/10/support-vector-regression-r/

###########################################
# mariadb data test
test<-read.csv('file:///C:/Users/DJA/Downloads/testdataANSI.csv', header = F, sep=',')
head(test)
colnames(test)<-c('time','code','stock','date','price','pr_ch','vol_ch','vol')
test
test1<-test %>% filter(stock == '삼성전자')
test1
test2<-test1 %>% select(time, price)
test2
plot(as.Date(test1$time),test1$price,pch=16)

t1<-ggplot(test2,aes(as.Date(test1$time),test1$price))
t1<-t1+geom_line()
t1
# t2<-t1+geom_freqpoly(binwidth = 60)
# t2

