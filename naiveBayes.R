# Naive Bayes

# 영화마케팅 문제를 베이즈정릴로해결
# 영화관객의 성향을 선문조사로 정리
# 관객의 속성으로 영화취양 파악

# 사전확률 :
# P(B) : 20대, 여, IT, 미혼, 애인없음
# P(A) : 공포 영화를 선택할 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)
# P(B|A) = P(20대, 여, IT, 미혼, 애인없음 | 공포) = 
# P(20대 | 공포) * P(여 | 공포) * P(IT | 공포) *
# P(미혼 | 공포) * P(애인없음 | 공포)

movie<-read.csv('c:/java/movie.csv')
head(movie)
movie 
nb_m<-naiveBayes(movie[1:5],movie$장르,laplace=0)
?naiveBayes
head(nb_m)

result_m<-predict(nb_m,movie[1:5])
result_m
movie$장르
sum(movie$장르!=result_m)
result_m
table(result_m,movie$장르)
confusionMatrix(result_m,movie$장르)


# P(A) - 로멘틱
# P(B) - 20대   여    디자이너       NO      YES

# P(A|B)

# iris 데이터집합을 이용해 베이즈정리로 분류분석
# 데이터를 학습/평가 데이터로분리
install.packages('ggplot2')
library(ggplot2)
library(lattice)
install.packages('caret')
library(caret)

idx<-createDataPartition(iris$Species,p=0.7,list=F)
# 7:3비율로 데이터 학습/평가 분리
train<-iris[idx,]
test<-iris[-idx,]

table(train$Species)
table(test$Species)

??e1071

#배이즈 이론으로 조건부확률 계산후적용
install.packages("e1071")
library(e1071)
result<-naiveBayes(train,train$Species,laplace=1)#학습
pred<-predict(result,test,type='class')         #예측결고
result
pred과

table(pred,test$Species)
# 베이즈 이론 적용 예측값 평가
confusionMatrix(pred,test$Species)


# 사내 메일중 스팸가려내기
spam<-read.csv('c:/java/spam.csv')
spam



## Categorical data only:
install.packages("mlbench")
library(mlbench)
data(HouseVotes84, package = "mlbench")
head(HouseVotes84)
dim(HouseVotes84)

?predict
# for(fn in methods("predict"))
#   try({
#     f <- eval(substitute(getAnywhere(fn)$objs[[1]], list(fn = fn)))
#     cat(fn, ":\n\t", deparse(args(f)), "\n")
#   }, silent = TRUE)
?eval
?substitute
?getAnywhere


model <- naiveBayes(Class ~ ., data = HouseVotes84)
model
# Conditional probabilities:
#   V1
# Y                    n         y
# democrat   0.3977273 0.6022727
# republican 0.8011696 0.1988304

pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)
?table

# pred         democrat republican
# democrat        238         13
# republican       29        155

## using laplace smoothing:
model <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)
head(HouseVotes84[,-1])
# pred         democrat republican
# democrat        237         12
# republican       30        156


##################################################
summary(HouseVotes84)
HouseVotes84[1:10,]
?HouseVotes84
model <- naiveBayes(Class ~ ., data = HouseVotes84[1:10,])
model
# A-priori probabilities:
#   Y
# democrat republican 
# 0.6        0.4 
# 
# Conditional probabilities:
#   V1
# Y              n   y
# democrat   0.6 0.4
# republican 1.0 0.0
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type = "raw")
pred <- predict(model, HouseVotes84[1:10,])
table(pred, HouseVotes84[1:10,]$Class)
#################################################
#
# 
a<-read.csv('c:/java/지원자.csv')
# P(합격)=13/20
# P(불합격)=7/20

# P(적음|합격)=     4:13
# P(적음|불합격)=   3:7

# 테스트데이터 나이-적음  장래희망-없음    태도-매우좋음    성적-보통   합격여부-?
str(a)

head(a)
# b<-c('적음','없음','매우좋음','보통')
나이 <-c('적음','많음','보통')
장래희망유무<-c('없음','있음','없음')
인터뷰태도<-c('매우좋음','보통','좋음')
고교성적<-c('보통','높음','낮음')
합격여부<-c('합격','합격','합격')
# 합격여부<-as.factor(합격여부) 

b<-data.frame(나이 ,장래희망유무,인터뷰태도,고교성적,합격여부)
head(b)
str(b)
nb_a<-naiveBayes(a,a$합격여부,laplace=0)
# ?factor
# ?naiveBayes
head(nb_a)

result_a<-predict(nb_a, b[1:4])
result_a

#합
(0.3076923*0.07692308*0.2307692*0.1538462)*(13/20)
#불
(0.4285714*1.00000000*0.1428571*0.4285714)*(7/20)

sum(b$합격여부!=result_a)
result_a
b$합격여부


table(result_a,b$합격여부)
confusionMatrix(result_a,b$합격여부)
########################################################
#원래데이터

nb_a<-naiveBayes(a,a$합격여부,laplace=0)
head(nb_a)

result_a<-predict(nb_a, a[1:4])
result_a
a$합격여부

sum(a$합격여부 != result_a)
table(result_a, a$합격여부)
confusionMatrix(result_a, a$합격여부)
########################################################


