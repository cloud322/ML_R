# a0326<-read.csv('file:///C:/Users/DJA/Downloads/m_sam.csv', header = T, sep=',')
a0326<-read.csv('file:///C:/Users/DJA/Downloads/새 폴더/0327_ANSI.txt', header = F, sep=',')
a0326
head(a0326)

colnames(a0326)<-c('a','stock','date','price','pr_ch','vol_ch','vol')

install.packages("dplyr")
library(dplyr)

samsung0326<-a0326 %>% filter(stock == '삼성전자')









