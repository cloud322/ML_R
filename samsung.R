s<-read.csv('c:/java/samsung1998_2005_2018.csv',header = T,sep=',')
str(s)
head(s)

install.packages("ggplot2")
install.packages("lattice")
library(ggplot2)
library(lattice)
# as.date  X0_일자 숫자 date로변환
date <- as.character(s$X0_일자)
date
s_date<-as.Date(date, '%Y%m%d')
s_date
#
s_date<-as.Date(as.character(s$X0_일자), '%Y%m%d')
s_date

# 시간 거래량


s2015<-subset(s,as.Date(as.character(s$X0_일자)=='20150101'))
s2015
s1<-ggplot(s,aes(as.Date(as.character(s$X0_일자), '%Y%m%d'),X6_거래량))
s1<-s1+geom_line()
s1

gg9<-gg9+xlim(1,13)+ylim(0,150)+ggtitle('1998~2018거래량')
gg9<-gg9+scale_x_continuous(breaks=1:12)
gg9<-gg9+geom_point()
########################################################################
#  시간 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("nycflights13")
library(tidyverse)
library(lubridate)
library(nycflights13)

nycflights13::flights

str(nycflights13)
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))


make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600)
###########################################################################
s2015<-subset(s,s$X0_일자>=20150101 & s$X0_일자<=20151231)
s2015
s2<-ggplot(s2015,aes(ymd(X0_일자),X6_거래량))
s2<-s2+geom_line()
s2
###################################################################
# Naive Bayes
sam<-read.csv('c:/java/samsung1998_2018.csv',header = T,sep=',')
library(e1071)
head(sam)
nb_sam<-naiveBayes(sam[1:13],sam$X13_updown,laplace=0)
head(nb_sam)
capture.output(head(nb_sam), file ='c:/java/nb_sam.txt',append = T)

result_sam<-predict(nb_sam,sam[1:13])
result_sam
sam$X13_updown
sum(sam$X13_updown!=result_sam)
result_sam
table(result_sam,sam$X13_updown)

library(caret)
confusionMatrix(result_sam,sam$X13_updown)
###
sams<-read.csv('c:/java/samsung2011_2018.csv',header = T,sep=',')
str(sams)
data.frame(sams)
str(sams)

nb_sams<-naiveBayes(sams[1:14],sams$X13_price_up, laplace=0)
head(nb_sams)
capture.output(head(nb_sams), file ='c:/java/nb_sam.txt',append = T)

result_sams<-predict(nb_sams,sams[1:14])
sum(sams$X13_price_up!=result_sams)
table(result_sams,sams$X13_price_up)
confusionMatrix(result_sams,sams$X13_price_up)
