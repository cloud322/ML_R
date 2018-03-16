#KNN

# iris 자료를 분석하기위해 KNN사용
data(iris)
head(iris,n=18) # 10개행출력

#iris 데이터 집합을 train test데이터로나눔
#난수생성
set.seed(6412)

idx<-sample(nrow(iris),size=nrow(iris)*0.75)
train<-iris[idx,]
test<-iris[-idx,]
dim(idx)
test


#데이터집합갯수와 차원확인
?dim
dim(train)
dim(test)
table(train$Species)    #빈도
table(test$Species)
test$Species
table(iris$Species)
iris$Species
# KNN알고리즘적
library(class)
knn1<-knn(train[,1:4],test[,1:4],train$Species,k=1,prob=T)
knn1
?knn
# knn<-knn(train[,1:4],test[,1:4],train$Species,k=1,prob=F)
knn3<-knn(train[,1:4],test[,1:4],train$Species,k=3,prob=T)
knn3
#예측값평가
knn1conf<-table(knn1,test$Species)
knn1conf

knn1acc<-sum(diag(knn1conf))/nrow(test)
knn1acc
b<-sum(diag(knn1conf))
c<-nrow(test)
d<-diag(knn1conf)
b
c
d      
?diag

knn3conf<-table(knn3,test$Species)
knn3conf
knn3acc<-sum(diag(knn3conf))/nrow(test)
knn3acc

?knn

#####################################################
like<-read.csv('c:/java/like.csv')
buy<-read.csv('c:/java/buy.csv')
head(like)
head(buy)
dim(like)
idx1<-sample(nrow(like),size=nrow(like)*0.75)
train_l<-like[idx1,]
test_l<-like[-idx1,]
train_l
test_l
dim(train_l)
dim(test_l)

knn_l_1<-knn(train_l[,1:7],test_l[,1:7],train_l$호감LABEL,k=1,prob=T)
knn_l_1
table(knn_l_1)
test_l$호감LABEL
knn_l_3<-knn(train_l[,1:7],test_l[,1:7],train_l$호감LABEL,k=3,prob=T)
knn_l_3

knn_l_1conf<-table(knn_l_1,test_l$호감LABEL)
knn_l_1conf
knn_1_1acc<-sum(diag(knn_l_1conf))/nrow(test_l)
knn_1_1acc

knn_l_3conf<-table(knn_l_3,test_l$호감LABEL)
knn_l_3conf
knn_1_3acc<-sum(diag(knn_l_3conf))/nrow(test_l)
knn_1_3acc
############################################################
head(buy)

#표준화작업
buy$age<-scale(buy$나이)
buy$income<-scale(buy$월수입)

set.seed(180315)
# idxb<-sample(nrow(buy),size=nrow(buy)*0.75)
# train<-iris[idxb,]
# test<-iris[-idxb,]
# 데이터 나누지않고 학습
head(buy)
buy
train_b<-buy[,4:5]
test_b<-data.frame(age=2.1,income=2.3)
test_b
# test_b<-test[,1:2]
labels<-buy[,3]
labels
knn_b_1=knn(train_b,test_b,buy$상품구매여부,k=1,prob=T)
knn_b_1
table(knn_b_1)

knn_b_5=knn(train_b,test_b,buy$상품구매여부,k=5,prob=T)
knn_b_5

buy$상품구매여부

knn_b_5conf<-table(knn_b_1)
knn_b_5conf
knn_b_5acc<-sum(diag(knn_b_5conf))/nrow(test_b)
knn_b_5acc

knn_b_5conf<-table(knn_b_5)
knn_b_5conf
knn_b_5acc<-sum(diag(knn_b_5conf))/nrow(test_b)
knn_b_5acc

sum(diag(knn_b_5conf))
