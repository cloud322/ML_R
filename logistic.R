# 로지스틱회귀

# 데이터 x의 분류가 y일 확률 을 p N 일확률을 (1-p)라 할때 
# log(p/(1-p)) / b0+b1X 식을 활용
# 
# 독립변수 x 와 종속변수 y 의 관계를 회귀식 이용해서 연속형 값을 예측하는것과 달리
# X변수에 대한 범주형범수를 예측하는 것이 핵심
# 
# 우선 독립변수가 하나인 단순회귀의경우 
# 공부한시간 2  4  5  8
# 성적       81 93 91 97
# 
# 하지만 로지스틱의경우
# 2  4  5  8  10  12  14
# 불 불 불 불 합 합 합

# y축 0~1 사이값
# x축 -무한~+무한 ㄱ
# 이값들을 이용 1차방정식

d<-read.csv('c:/java/drink.csv')
head(d)
attach(d)
install.packages("class")

library(class)


gml.result<-glm(지각여부~나이+결혼여부+자녀여부+체력+주량+직급+성별,family=binomial(link=logit),data=d)

gml.result
# p-value 값이 0.05 이하 유의미하
summary(gml.result)


predict(gml.result,d,type='response')
# 예측값 0~1 사이 범위로 출력 

predict(gml.result,d,type='response')>=0.5
# 지각여부를 알아보기위해 0.5 이상인 값 

table(d$지각여부,predict(gml.result,d,type='response')>0.5)
# 실제값 예측값이 0.5 이상이값들 출력 
###############################################################################

# 로지스틱 회귀
# 데이터 x의 분류가 Y일 확률을 p, N일 확률을 1-p라 할때
# log(p/(1-p)) / b0 + b1X 식을 활용

# 독립변수 x 와 종속변수 y 의 관계를 회귀식을 이용해서
# 연속형 값을 예측하는 것과 달리
# X 변수에 대한 범주형 범수를 예측하는 것이 핵심

# 예) 우선 독립변수가 하나인 단순 회귀의 경우
# 공부한 시간 : 2  4  5  8
# 성적        : 81 93 91 97

# 하지만, 로지스틱 회귀의 경우
# 공부한 시간 : 2  4  5  8  10  12  14
# 성적        : 불합격 불합격 불합격 불합격 합격 합격 합격 

# y축은 0 - 1 사이 값을 갖고
# x축은 -무한대 ~ +무한대 값을 가질 수 있음
# 이 값들을 이용해서 1차방정식을 그려보면 S모양 그래프 생성
# 이러한 그래프를 그려주는 함수를 시그모이드 함수라 함

# 사원들의 근태여부 판별
# 나이, 결혼여부, 성별등이 지각여부에 영향을 미칠지 분석

emp <- read.csv('c:/Java/drink.csv', header=T)
head(emp)

str(emp)

attach(emp)
library(class)

# glm.result <- glm(지각여부~나이+결혼여부+자녀여부+
#                   체력+주량+직급+성별,
#                   family=binomial(link=logit),
#                   data=emp)

glm.result <- glm(지각여부~., family='binomial',data=emp)

glm.result
# p-value 값이 0.05 이하 : 유의미한 결과

summary(glm.result)

predict(glm.result, emp, type='response')
# 예측값을 0 ~ 1 사이의 범위로 출력

predict(glm.result, emp, type='response') >= 0.5
# 지각여부를 알아보기 위해 0.5이상인 값들을 출력

table(emp$지각여부,predict(glm.result, emp, type='response') > 0.5)
# 실제값과 예측값(0.5이상인 값)들을 출력


# iris 데이터 집합을 로지스틱 회귀로 분석
# iris 데이터 집합의 Species는 setosa, versicolor, virginica
# 따라서, 예측값은 이항이어야 하므로
# versicolor, virginica 만 예측하도록 해보자
bi_iris <- subset(iris, Species=='versicolor' |Species=='virginica')
str(bi_iris)
# Species가 새로운 범주값을 갖도록 factor 함수 적용
bi_iris$Species <- factor(bi_iris$Species)
str(bi_iris)

# glm 함수로 로지스틱 회귀 적용
glm.result <- glm(Species~., family='binomial',data=bi_iris)

pre_iris <- predict(glm.result, bi_iris, type='response')
# 0 ~ 1 사이값 예측

pre_iris >= 0.5

table(bi_iris$Species, pre_iris > 0.5)


# setosa, virginica 만 예측하도록 해보자
bi_iris <- subset(iris, Species=='setosa' |Species=='virginica')
head(bi_iris)
str(bi_iris)

bi_iris$Species <- factor(bi_iris$Species)

glm.result <- glm(Species~., family='binomial',data=bi_iris)

pred <- predict(glm.result, bi_iris, type='response')

pred >= 0.5

table(bi_iris$Species, pred > 0.5)


# 공부한시간에 따른 합격여부 예측
study <- read.csv('c:/Java/glmtest.csv')
study$Pass <- factor(study$Pass)

# study_glm <- glm(study$Pass~study$Hours,
#                  family='binomial', data=study)

study_glm <- glm(study$Pass~.,family='binomial', data=study)

summary(study_glm)


# 
# glm.result <- glm(Species~Sepal.Width, family='binomial',
#                   data=bi_iris)

# summary(glm.result) # 정상적으로? p-value 출력

glm.result <- glm(Species~Sepal.Width+Sepal.Length, family='binomial',data=bi_iris)

summary(glm.result) # 다소 비정상적으로? p-value 출력


# predict 함수 외에 fitted 함수로 예측값 조사
fitted(glm.result)


# 다항 로지스틱 회귀 - 범주가 3개이상의 값을 가진 경우
# glm 함수 대신 multinom 함수를 사용한다
install.packages('caret')
library(caret)
library(nnet)

train.idx <- createDataPartition(iris$Species, p=0.7, list=F)

iris_train <- iris[train.idx, ]
iris_test <- iris[-train.idx, ]

multi.result <- multinom(Species~., iris_train)
# 다항 로지스틱 회귀분석
multi.pred <- predict(multi.result, iris_test) # 예측

table(multi.pred, iris_test$Species)
confusionMatrix(multi.pred, iris_test$Species)


# 여성이 좋아할 만한 선물을 예측하는 예제
# 연령, 직업에 따라 선물 예측 - 다항 로지스틱 회귀
# 범주가 3개의 값을 가지고 있기 때문에
# 범주의 값을 2개로 줄여 glm 함수를 이용하던지
# 다항 로지스틱 회귀 함수인 multinom을 이용
present <- read.csv('c:/Java/present.csv', header=T)
head(present)
str(present)

p.result <- multinom(선물~., present)

summary(p.result)

p.pred <- predict(p.result, present)
table(p.pred, present$선물)

confusionMatrix(p.pred, present$선물)