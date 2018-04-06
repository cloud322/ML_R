a0326<-read.csv('file:///C:/Users/DJA/Downloads/새 폴더/0327_ANSI.txt', header = F, sep=',')
head(a0326)

colnames(a0326)<-c('a','stock','date','price','pr_ch','vol_ch','vol')

# install.packages("dplyr")
library(dplyr)

str(a0326)

samsung0326<-a0326 %>% filter(stock == '삼성전자')
head(samsung0326)
#date package
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse)
library(lubridate)

## Load the data from the csv file
d_p_sam<-samsung0326 %>% select(date, price)
plot(d_p_sam$date,d_p_sam$price,pch=16)


# help(strftime)
x  <-  d_p_sam$date
# x2  <-  select(d_p_sam$date>="2018-04-02 09:00:01 KST" & d_p_sam$date>="2018-04-02 11:00:01 KST")
# x2
# time <- substr(as.POSIXct(sprintf("%06.0f", x), format='%H%M%S'), 12, 19)
time <- as.POSIXct(sprintf("%06.0f", x), format='%H%M%S')
time

# to data frame  ((time,price))
time <- c(as.POSIXct(sprintf("%06.0f", x), format='%H%M%S'))
price <- c(d_p_sam$price)
df<- data.frame(time,price)
df
substr(df$time,12,19)

library(sqldf)
sqldf('select * from df where time <= "2018-04-03 10:08:00" & time >= "2018-04-03 10:00:00"')

sqldf('select * from df where time = "2018-04-03 09:02:17"')


sqldf('select * from a0326 where stock="삼성전자"')

a <- sqldf('select * from a0326 where date >= 90000 and date <= 100001 and stock="삼성전자"')

plot(a$date, a$price)
