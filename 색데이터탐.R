#데이터 탐색
#모집단- 분석을 하기 위해 관심이 있는 대상 전체를 말한다
#표본 추출-모집단의 부분 집합을 추출하는 것을 표본 추출
#통계 분석-본이 가지고 있는 모집단 성질의 일부만을 가지고 모집단의 특성을 합리적으로 추론하는 것
#모수(Parameter)-모집단의 통계적 속성을 나타내는 수치를 모수라고 하며, 여기에는 모집단을 구성하는 데이터의 위치 정보를 나타내는 평균(mean), 중앙값(median)과 데이터 분포에 대한 지표인 분산(variance) 및 표준편차(standard deviation) 등

#표본 추출 기법 
# -단순 무작위 추출(simple random sampling)
# -계통 추출(systematic sampling)
# -층화 추출(stratified random sampling)
# -군집 추출(cluster random sampling)

#척도의 종류
# -명목 척도(nominal scale)
# -순서 척도(ordinal scale)
# -구간 척도(interval scale)
# -비율 척도(ratio scale)

#잡음
# -참값에서 벗어난 정도를 잡음(noise)라고 

phone<-read.csv('c:/java/phone-01.csv',header=F)
head(phone)
summary(phone)
var(phone)
sqrt(var(phone))

hist(phone$V1)
hist(phone$V2)
hist(phone$V3)

#상관분석(correlation analysis)
# -공분산(covariance)을 이용

#상관계수(correlation coefficient)
# -변수의 표준편차의 곱으로 나누어 주어 선형관계의 정도를 표현

#회귀분석(regression analysis)
# -한 변수가 다른 변수에 미치는 영향을 함수 형태로 추정하기 위해 
# -영향을 주는 변수는 독립변수(independent variable)라고 하며, 
# -영향을 받는 변수는 종속변수(dependent variable)라고 한다.
# -단순회귀분석(simple regression analysis)
#  독립변수가 1개이며 종속변수와의 관계가 선형적(1차 함수)
# -다중회귀분석(multiple regression analysis)
#  독립변수가 2개 이상이며 종속변수와의 관계가 선형적(1차 함수)
# -곡선회귀분석(curvilinear regression analysis)
# 독립변수가 1개이며 종속변수와의 관계가 곡선적(2차 함수 이상)

p2<-read.csv('c:/java/phone-02.csv',header=F,sep=',')
head(p2)
cor(p2)
lm(p2)
pp2<-lm(p2$V9~p2$V7)
#   y=-272.001+6.283x
summary(pp2)


# 변수간 관계확인
# 분석모형 구축시 유의사항
# 변수의 유의성 검증 후 유의성 높은 최소한의 변수들로 분석모형을 검증
# 분석모형을 구축하는대에는 일반화의 오류와 훈련오류 가있다

# 분석모형에서 산출된 값 과 실제값과의 차이의 제곱 평균을 예측오차로 RMSE
#
# 분석모형 타당성평가
# 분석모형이 실제로 현실을 묘사하는지
# 주어진데이터를 모형에 입력 얻은 결과와 실제 데이타갑 같의 차이가 적으면 좋은모형

# 분석모형 보정
# 시뮬레이션 분석 모형이 유효한지 판단하기 위해
# 시뮬레이션을 통해 모형의 타당성과 적합성을 평가
# 시뮬레이션 결과가 실제 데이터 값과 차이가 크면 분석모형 재검토/수정 필요

#무작위추출통해
s<-read.table('c:/java/phone-02.csv', header=F,sep=',')
head(s)

for(i in 1:5){
  tr<-sample(nrow(s),replace=F,size=nrow(s)*0.7)
  tr
  
  #학습 훈련데이터 추출
  s_train<-s[tr,]
  s_test<-s[tr,]
  
  #회기모형구축 -회기식작성
  lm(s_train[,9]~s_train[,7],data=s_train)
  
  # y= -261.462+6.169x  
  lrm<-function(x){-261.462+(6.169*x) }
  test_result<-lrm(s_test[,7])
  #RSME 계사산
  rmse<-sum(abs(s_test[,7]-lrm(s_test[,7]))) / sqrt(nrow(s_test))
  print(rmse)
}#for문 닫ㄱ
    

#데이터 정제하기
#데이터유형 추출 which

l2016<-which(s[,1]==2016) # 2016 출시상품 index 출력
s[l2016,]             #2016 출시상품출력

#특정범위 데이터 추출
gt_500<-s[,9]>500
s[gt_500,]

#KNN알고리즘 군집화
# 차종 마력 경기용차여부 빠른지여부
# 혼다어코드,180,F,F
# 혼다어코드,500,T,T
# 혼다어코드,200,T,F
# 혼다어코드,400,T,?

#유클리드거리측정 이용    sqrt((x-y)**2+(x-y)**2)
# 차종 마력 빠른지여부
# 혼다어코드,180,0
# 혼다어코드,500,1
# 혼다어코드,200,0
# 혼다어코드,400,?
#
km5<-kmeans(s[,9],5)
km5
km5$cluster#군집벡터값 출력
km5$centers#군집벡터값 평균
km5_cluster<-km5$cluster
for(i in 1:5){
  km5_cluster[km5_cluster==i]<-km5$centers[i]
}

s_km5<-cbind(s,km5_cluster) #평균값대체
s_km5

#
km3<-kmeans(s[,9],3)
km3
km3$cluster
s_km3<-cbind(s,km$cluster)
s_km3


#데이터 정제
# 데이터를 불완전하게 만드는요소 보통 결측치 와 잡음존제
# 결측치 - sample 에서 누락된 변수값 지정
# 오류로 인해 발생할수도 단순히 조사대상이 측정을 원하지 않을떄에도 발생
# 해결방법 샘플제고 해당변수 제거 결측치무시 결측치 추정 평균 중앙값 회귀분석
# 
# 이상치 데이터집합에서 대부분의 다른 샘플과 현저한 차이를 나타내는 샘플 변수값
# 오류면 제거 이상치면 관심을가지고 본다


