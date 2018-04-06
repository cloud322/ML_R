#SVM
# 지도학습을통한 불류를 지원
# 다양한데이터분포에서도 잘닥동하는 분류기법
# 기계학습의 한분야 자료분석 패턴인식에 주로사용

# 두카테고리 중 하나에 속하는 데이터 집합이 주어젓을떄 
# SVM 은 주어진 데이터 집합을 바탕으새 ㅅ로운 데이터가 어디에 속할지 판단하는
# (비확률)이진 (비)선형 분류모델을 사용

# 분류모델은 데이터배 배치된 공간에서 경계로 표현
# SVM 은 그중 가장 폭 ㅍ을 가진 경계를 찾는것
# SVM 은 선형불류 더불어 비선형분류에도 사용가능
# 이떄 커널트릭을 이용해서 료율적으로 분류

set.seed(6412)
x<-matrix(rnorm(20*2),ncol=2)
y<-c(rep(-1,10),rep(1,10))
x[y==1]<-x[y==1]+1
?e1071
# 산점도표시
plot(x,col=3-y)

data<-data.frame(x=x,y=as.factor(y))
data
# R 에서 SVM 을 사용하려면 e1071 이용
library(e1071)
svmfit<-svm(y~.,data=data,kernel='linear',cost=10)
plot(svmfit,data)
summary(svmfit)

tune.out<-tune(svm,y~.,data=data,kernel='linear',range=list(cost=c(0.001,0.01,0.1,1.5,10,100)))
summary(tune.out)

#검증데이터 작성 및 예측 
xtest<-matrix(rnorm(20*2),ncol=2)
ytest<-sample(c(-1,1),20,rep=T)
xtest[ytest==1]<-xtest[ytest==1]+1
testdat<-data.frame(x=xtest, y=as.factor(ytest))
testdat

#tune 함수를 통래 찾아낸 최적의 cost data 학습
bestmod<-tune.out$best.model
summary(bestmod)

pred<-predict(bestmod,testdat)
table(pred,testdat$y)

library(caret)
confusionMatrix(pred,testdat$y)

#cost 가ㅣ 0.01 때와 bestmo 일떄 비교
svmfit<-svm(y~.,data=data,kernel='linear',cost=0.01)
pred<-predict(svmfit,testdat)
table(pred,testdat$y)


install.packages("dplyr")
library(dplyr)
library(caret)

#iris 데이터집합 e1071 퍄키지 svm 분류
library(e1071)

idx<-createDataPartition(iris$Species,p=0.7,list=F)
train<-iris[idx,]
test<-iris[-idx,]

svm.result<-svm(Species~.,train,kernel='radial')

pred<-predict(svm.result,test,type='response')
table(pred,test$Species)
confusionMatrix(pred,test$Species)
# Reference
# Prediction   setosa versicolor virginica
# setosa         15          0         0
# versicolor      0         13         0
# virginica       0          2        15

# Accuracy : 0.9556    

#iris 데이터집합 kernlab 패키지의 ksvm 분류
library(kernlab)

idx<-createDataPartition(iris$Species,p=0.7,list=F)
train<-iris[idx,]
test<-iris[-idx,]
svm.result<-ksvm(Species~.,train,kernel='rbfdot')
# rbfdot = radial basic function
pred<-predict(svm.result,test,type='response')
table(pred,test$Species)
confusionMatrix(pred,test$Species)


# ISLR 패키지의 carseats 데이터집합을 이용한 SVM 예제
# Carseats : 400개의 가게에서 팔린 유아용 자동차 시트
# 판매 관련 데이터
install.packages('ISLR')
library(ISLR)
Carseats # str(Carseats)
str(Carseats)

# 판매량 Seles의 평균보다 낮으면
# Profit이라는 변수에 No, 높으면 Yes라고 설정
# Urban, US, ShelveLoc 변수를 제외
attach(Carseats)
Profit <- ifelse(Sales <= mean(Sales), "No", "Yes")
nCarseats <- data.frame(Carseats, Profit)

# nCarseats에서 Sales, Urban, US, ShelveLoc 변수 제외
nCarseats <- nCarseats[, -c(1,7,10,11)]
summary(nCarseats)

# 데이터 집합을 7:3으로 train, test로 분리
library(caret)
library(e1071)

idx <- createDataPartition(nCarseats$Profit, p=0.7, list=F)
train <- nCarseats[idx, ]
test <- nCarseats[-idx, ]
factor(test$Profit) # Profit 변수를 범주형으로 변환

# SVM 을 적용후 학습
# 단, 각 변수의 변량이 서로 다르기 때문에 scale 필요
svm.result <- svm(Profit~., data=train, kernel='radial')
pred <- predict(svm.result, test, type='response')

summary(svm.result)

table(pred, test$Profit)
confusionMatrix(pred, test$Profit)

##
mid<-read.csv('c:/java/posdata.csv',header = T,sep = ',')
# 1년동안 종목별 소비지출액을 0~1 사이 정규화 scale 했음 
# status 1이면 중산층분류
head(mid)
idx <- createDataPartition(mid$status, p=0.7, list=F)
train <- mid[idx, ]
test <- mid[-idx, ]
str(mid)
factor(test$status)
str(test$status)

svm.result <- svm(status~., data=train, kernel='radial',type='C-classification')
pred <- predict(svm.result, test)
summary(svm.result)

table(pred, test$status)
confusionMatrix(pred, test$status)

