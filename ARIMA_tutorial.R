#통계기반데이터분ㅅ
head(AirPassengers)
data(AirPassengers)
AirPassengers
plot(AirPassengers) 
str(AirPassengers)
 
??tseries

plot(stl(AirPassengers, s.window='periodic')) 
# 계절성 (seasonality), 추세 (trend), 불확실성 (random) 요소로 분해해서 그래 프를 확인할 수 있다.

install.packages("tseries")
library(tseries)
difflogAirPassengers <- diff(log(AirPassengers))
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


##############################################################
