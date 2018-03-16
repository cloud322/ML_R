# 데이터시각화
# 
# 인간의 시지각 능력을 토대로 데이터에 대한 이해와 설득에 도움을 주기 위해 그림이나 도
# 형 등의 그래픽 요소들을 이용하여 데이터를 묘사하고 표현하는 것이다. 
# 
# 설명 기능
# 탐색 기능
# 표현 기능
a2015<-read.csv('c:/java/seoul_car_acci_v1.txt',sep=',')

a2016<-read.csv('c:/java/car_2016.txt',sep='\t')
head(a2016)

a20052016<-read.csv('c:/java/car_2005_2016.txt',sep='\t')
head(a20052016)


#R 시각화도구 
graphics - 별도설ㅊx

ggplot2 - 칩치별도설ㅊo

lattice - 별도설치o치

install.packages("ggplot2")
install.packages("lattice")
library(ggplot2)
library(lattice)

# 그래프 작성 보조 도구 - dplyr

install.packages("dplyr")
library(dplyr)
head(iris)
gg1<- ggplot(iris,aes(Sepal.Length,Sepal.Width))
# 그레프작성 초기화(데이터집합, x, y)  
gg1+geom_point()
# 점그래프 산점도 산포도
head(gg1)

gg2<- ggplot(iris,aes(Sepal.Length,Sepal.Width))
gg2+geom_point(aes(color=Species,size=Petal.Width))

#서울 중구 2015 교통사고 현황을 산점도 geom_point 로작성
str(a2015)
j2015<-subset(a2015,자치구명=='중구')
gg3<- ggplot(j2015,aes(월,발생건수))
gg3<- gg3+geom_point(aes(color=월,size=월))
gg3<- gg3+xlim(1,13)+ylim(0,150)+ggtitle('2015 중구 월별 교통사고 현황')
gg3<- gg3+ggtitle('2015 중구 월별 교통사고 현황')
gg3

#서울 중구 2015 교통사고 현황을 선그래프 geom_line 로작성
str(a2015)
j2015<-subset(a2015,자치구명=='중구')
gg9<-ggplot(j2015,aes(월,발생건수))
gg9<-gg9+geom_line()
gg9<-gg9+xlim(1,13)+ylim(0,150)+ggtitle('2015 중구 월별 교통사고 현황')
gg9<-gg9+scale_x_continuous(breaks=1:12)
gg9<-gg9+geom_point()
#gg9<-gg9+theme(panel.background = element_blank())
gg9<-gg9+theme(panel.background = element_rect(color='red',fill='white'),plot.title = element_text(hjust=0.5))
gg9

#막대그래프 geom_bar
str(a2015)
j2015<-subset(a2015,자치구명=='중구')
gg12<- ggplot(j2015,aes(월,발생건수))
gg12<- gg12+geom_bar(aes(월,발생건수),stat='identity')
gg12<-gg12+scale_x_continuous(breaks=1:12)+ggtitle('2015 중구 월별 교통사고 현황')
gg12<- gg12+geom_bar(aes(fill=월),stat='identity')
# or
# gg12<- gg12+geom_bar(fill=j2015$월,stat='identity')
# or
# gg12<- gg12+geom_bar(fill='red',stat='identity')
gg12



#또다른 유형 산점도
x<-1:50
y<-sapply(x,function(x) x/(x-1))
df<-data.frame(x,y)
head(df)

gg4<-ggplot(df,aes(x,y))
gg4<-gg4+geom_point()
gg4

#다이아몬드 데이터
#케럿당 프라이스 산포도
# 색상컬러
head(diamonds)

gg5<- ggplot(diamonds,aes(carat,price))
gg5+geom_point(aes(color=color,size=price))

#선 그래프 
# geom_line

head(economics)

gg6<-ggplot(economics,aes(date,unemploy))
gg6<-gg6+geom_line(color='red',size=1,linetype=6)
gg6
#여러개 선 그래프
gg7<-ggplot(economics)
gg7<-gg7+geom_line(aes(date,unemploy))
gg7<-gg7+geom_line(aes(date,pce))
gg7

# ORANGE 연령별 둘래 현황  
head(Orange)
gg8<-ggplot(Orange,aes(age,circumference))
gg8<-gg8+geom_line(aes(color=Tree))
gg8<-gg8+geom_point()
gg8

# 막대그래프
# geom_bar

head(mtcars)
str(mtcars)
gg10<-ggplot(mtcars,aes(cyl))
gg10<-gg10+geom_bar(aes(fill='red'), width=0.5)
gg10

# cyl num type -factor 형으로 변환
fcyl=factor(mtcars$cyl)
gg10b<-ggplot(mtcars,aes(fcyl))
gg10b<-gg10b+geom_bar(aes(fill=fcyl), width=0.5)
gg10b

#누적막대그래프
fgear=factor(mtcars$gear)
gg10c<-ggplot(mtcars,aes(fcyl))
gg10c<-gg10c+geom_bar(aes(fill=fgear), width=0.5)
gg10c<-gg10c+coord_flip()
gg10c

# 실린더 수 따른 연비 
#,stat='identity' - y 값의 mpg 값을 그대로 사용
gg11<-ggplot(mtcars,aes(fcyl,mpg))
gg11<-gg11+geom_bar(aes(fill=fcyl),stat='identity')
gg11


a_2015_12<-subset(a2015,연도==2015&월==12)
a_2015_12
gg13<-ggplot(a_2015_12,aes(자치구명,발생건수))
gg13<-gg13+geom_bar(aes(fill=자치구명),stat='identity')
gg13<-gg13+theme(axis.text.x = element_text(angle=90,hjust=1))
gg13


gg14<-ggplot(a_2015_12, aes(자치구명,부상자수))
gg14<-gg14+geom_bar(aes(fill=자치구명),stat='identity')
gg14


gg15<-ggplot(a_2015_12,aes(자치구명,사망자수))
gg15<-gg15+geom_bar(stat='identity',fill=rainbow(25))
gg15


#원그래프


gg16<-gg10+coord_polar()
gg16

# 일반적 파이그래프
gg17<-ggplot(mtcars,aes(factor(1),fill=fcyl))
gg17<-gg17+geom_bar(width = 1)
gg17<-gg17+coord_polar(theta = 'y')
gg17

#heat map

a_2015_12<-subset(a2015,연도==2015&월==12)
a_2015_12 #문자숫자같이있음
#행의 이름지정
row.names(a_2015_12)<-a_2015_12$자치구명
#발생건수 사망자수 부상자수 추출
m_a_2015_12<-a_2015_12[,c(4:6)]
#발생건수 사망자수 부상자수 matrix 로변환
m_a_2015_12<-data.matrix(m_a_2015_12)

heatmap(m_a_2015_12,col=cm.colors(128),scale='column',cexCol=1,Rowv = NA,Colv = NA,margins = c(5,5))
m_a_2015_12

mtcars<-as.matrix(mtcars)
heatmap(mtcars, scale = 'column',Rowv = NA,Colv = NA,col=rainbow(256)) # 정규화 후 다시 그림


#버블차트
gg18 <- ggplot(a_2015_12,aes(발생건수,자치구명))
gg18<-gg18+geom_point(aes(color=사망자수),shape=16,alpha=0.5)+scale_fill_brewer(palette = 'Accent')
# gg18<-gg18+scale_fill_brewer(palette='Set1')
gg18
a_2015_12
#
RColorBrewer:: display.brewer.all()

# Cars93 데이터 이용 - 도시, 고속도로별 연비
library(MASS)
head(Cars93)

ggplot(Cars93, aes(Weight, MPG.highway)) +
  #  geom_point(size=6, shape=21) # 기본
  #  geom_point(aes(size=MPG.highway), shape=21) # 사이즈 지정
  #  geom_point(aes(size=MPG.highway), shape=21,
  #             colour='red') # 색상지정
  geom_point(aes(size=MPG.highway), shape=21,
             fill='red') # 내부채우기

# ggplot(Cars93, aes(Weight, MPG.highway, fill=Price)) +
#   # 연속형 변수값에 따라 색깔 변화
ggplot(Cars93, aes(Weight, MPG.highway, fill=Cylinders)) +
  # 연속형 변수Cylinders에 따른 색깔 변화
  geom_point(size=5, shape=21, colour='red') +
  # scale_fill_brewer(palette='Oranges')
  # # 색깔 지정을 미리 정의된 팔레드 이용
  # scale_fill_brewer(palette='Set1')
  scale_fill_brewer(palette='Paired')

#
a2015
a_2015_12
install.packages('ggmap')
library(ggmap)
library(maps)
library(mapproj)
# “terrain”, “satellite”, “roadmap”, “hybrid”
smap<-get_googlemap('seoul',maptype = 'satellite',zoom=4)
smap<-get_googlemap('seoul',maptype = 'roadmap',zoom=11)
seoul<-ggmap(smap)
seoul
s_l<-read.csv('c:/java/seoul_lat_lon.txt')
head(s_l)

a_2015_12_m<-merge(a_2015_12,s_l,by.x = '자치구명',by.y='area' )
head(a_2015_12_m)

#맵에 자치구위치를 점으로 표시 
gmp<-seoul
gmp<-gmp+geom_point(data=a_2015_12_m,aes(x=lon,y=lat,size=발생건수),shape=16,alpha=0.5)
gmp
gmp<-gmp+geom_text(data=a_2015_12_m,aes(label=자치구명),size=3,hjust=1.2,fontface='bold')
gmp

#선긋기
gmp<-gmp+geom_path(data=a_2015_12_m,aes(x=lon,y=lat),color='red',alpha=0.5,lwd=1)
gmp


