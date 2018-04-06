a0326<-read.csv('file:///C:/Users/DJA/Downloads/새 폴더/0327_ANSI.txt', header = F, sep=',')

head(a0326)

colnames(a0326)<-c('a','stock','date','price','pr_ch','vol_ch','vol')

install.packages("dplyr")
library(dplyr)


samsung0326<-a0326 %>% filter(stock == '삼성전자')
head(samsung0326)
#date package
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse)
library(lubridate)

## Load the data from the csv file
d_p_sam<-samsung0326 %>% select(date, price)
d_p_sam

#########################################################################
a0403M<-read.csv('C:/Users/DJA/Downloads/0403_samsung_min_A.csv', header = T, sep=',')
head(a0403M)

install.packages("dplyr")
library(dplyr)

#date package
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse)
library(lubridate)

## Load the data from the csv file
d_p_sam<-a0403M %>% select(date, price)
d_p_sam
str(d_p_sam)

#time series
library(xts)
sam_d_p <- xts(d_p_sam[,-1], order.by=as.Date(d_p_sam[,1], "%Y/%m/%d"))
#
#  daily returns
#
returns <- diff(sam_d_p, arithmetic=FALSE ) - 1
#
#  weekly open, high, low, close reports
#
to.weekly(sam_d_p$Hero_close, name="Hero")

#통계기반데이터분ㅅ
head(d_p_sam)
data(d_p_sam)

plot(d_p_sam) 

plot(stl(d_p_sam, s.window='periodic')) 
# 계절성 (seasonality), 추세 (trend), 불확실성 (random) 요소로 분해해서 그래 프를 확인할 수 있다.

install.packages("tseries")
library(tseries)
difflogd_p_sam <- diff(log(d_p_sam))
plot(difflogAirPassengers)
#AirPassengers 시계열 데이터에 diff 및 log함수를 적용한 데이터 플롯

adf.test(difflogAirPassengers, alternative="stationary", k=0)

install.packages("forecast")
library(forecast)
auto.arima(difflogAirPassengers)

fitted <- arima(log(AirPassengers), c(1, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))
fitted
predicted <- predict(fitted, n.ahead = 120)
ts.plot(AirPassengers, exp(predicted$pred), lty = c(1,2))
# predicted$pred 항목에 log(AirPassengers)의 예측치 값이 저장 
# AirPassengers 데이터를 바탕으로 생성한 ARIMA 모델 기반 10년 예측 데이터 플롯
