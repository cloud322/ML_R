# 함수 모음

# HIVE 연결
# install.packages("rJava")
# install.packages("RJDBC")

connhive <- function(dpath, hiveip, dbname) {
  options(java.parmeters = "-Xmx4g")
  # install.packages("rJava")
  # install.packages("RJDBC")
  library(DBI)
  library(rJava)
  library(RJDBC)
  
  hive.class.path = list.files(path=c(paste0(dpath, "/rhive/hive")), 
                               pattern="jar", full.names=T);
  hadoop.lib.path = list.files(path=c(paste0(dpath, "/rhive/hadoop/lib")), 
                               pattern="jar", full.names=T);
  hadoop.class.path = list.files(path=c(paste0(dpath, "/rhive/hadoop")), 
                                 pattern="jar", full.names=T); 
  class.path = c(hive.class.path, hadoop.lib.path, 
                 hadoop.class.path); 
  .jinit(classpath = class.path)
  
  drv <- JDBC("org.apache.hive.jdbc.HiveDriver")
  
  conn <- dbConnect(drv, 
                    paste0("jdbc:hive2://", hiveip, ":10000/",
                           dbname), "hive", "hive")
  return(conn)
}


dpath <- "C:/JAVA"
hiveip <- "j322j322.iptime.org"
dbname <- "stockdata"



# sql문 실행

sqlexe <- function(sql) {
  conn <- connhive(dpath, hiveip, dbname)
  rs <- dbGetQuery(conn, sql);
  # df <- dbFetch(rs, n = -1)
  df <- rs
  # dbClearResult(rs)
  dbDisconnect(conn)
  return(df)# query result
}


# 특정 테이블 불러오기

table <- "0326_0857"
# stock <- "삼성전자"
# data <- calldata(table)

calltable <- function(table) {
  sql <- paste0("select * from ", table)
  table <- sqlexe(sql)
  colnames(table) <- chfield(table)
}

a<-calltable(table)



# 특정 필드 데이터 불러오기

# table <- "0326_0857"
# stock <- "삼성전자"
# data <- calldata(table, stock)

calldata <- function(table, field, data) {
  sql <- paste0("select * from ", table, 
                " where ", "`", field, "` = '", data, "'")
  table <- sqlexe(sql)
  colnames(table) <- chfield(table)
  return(table)
}

# 증감량 변환
# ud <- data0326$ud
# ud(ud)

ud <- function(data) {
  udud <- NULL
  udud[1] <- 0
  n <- nrow(data)
  p <- data$price
  d <- p[-1] - p[-n]
  for (i in 2:n) {
    if (d[i - 1] > 0) udud[i] <- 1
    if (d[i - 1] < 0) udud[i] <- -1
    if (d[i - 1] == 0) udud[i] <- 0
  }
  return(as.factor(udud))
}

# csv파일로 저장

savecsv <- function(data, name, type) {
  if (type == 1) {
    write.csv(data, paste0("D:/빅데이터/project/pdata/", name, ".csv"), 
              fileEncoding = 'UTF-8',
              row.names = F,
              quote = F)
  }else {
    write.csv(data, paste0("D:/빅데이터/project/pdata/", name, ".csv"),
              row.names = F,
              quote = F)
  }
}

# 데이터 범위 조정a

ctrda <- function(data, timediff = NULL, starttime = NULL, endtime = NULL) {
  # data <- hanmi;
  
  
  ctrDatas <- data.frame()
  # tmp <- data.frame()
  
  cdt <- data$time;
  # print(cdt)
  
  if (!is.null(timediff)) {
    lag <- timediff
  } else {
    lag <- 1
  }
  if (!is.null(starttime)) {
    t1 <- timeplus(starttime, lag)
  } else {
    t1 <- timeplus(cdt[1], lag)
  }
  if (!is.null(endtime)) {
    t2 <- endtime
  } else {
    t2 <- cdt[length(cdt)]
  }
  # lag <- 100
  # starttime <- 90000
  # endtime <- 152000
  
  # t1 <- timeplus(starttime, lag)
  # t2 <- endtime

  ts <- seq(t1,t2,lag)
  
  for (t in ts) {
    # t <- t1
    # t <- t + 10
    # t <- 84940
    hh <- t %/% 10000
    mmtt <- t %% 10000
    mm <- mmtt %/% 100
    tt <- mmtt %% 100
    # mm < 60 & tt < 60
    if (mm < 60 & tt < 60) {
      idx <- which(cdt > timeminus(t, lag) & cdt <= t)
      # length(idx) == 0
      if (length(idx) == 0) {
        s <- 0
        idx <- which(ctrDatas$time == timeminus(t, lag))
        tmp <- ctrDatas[idx,]
      } else {
        s <- sum(data$vol_ch[idx])
        idx <- idx[length(idx)]
        tmp <- data[idx, ]
        # print(tmp)
      }
      if (length(tmp) != 0) {
      tmp$time <- t
      # print(tmp)
      tmp$vol_ch <- s
      # print(tmp)
      ctrDatas <- rbind(ctrDatas, tmp)
      }
    }
  }
  rownames(ctrDatas) <- 1:nrow(ctrDatas)
  return(ctrDatas)
}

# c삼성전자 <- 1
# c삼성전자 <- ctrd(삼성전자,10)


# 데이터 나누기

splitdata1 <- function(data) {
  
  # data <- 삼성전자;
  attach(data)
  as.numeric(time)
  as.numeric(vol_ch)
  as.numeric(vol)
  
  # str(minDatas)
  # colnames(minDatas) <- c('time', 'price')
  
  ii <- vol[-1] - vol[-length(vol)]
  
  sd <- subset(data, time <= 090500)
  sd2 <- subset(data, time >= 152500)
  sd3 <- subset(data, time < 152500)
  
  # print(sd2)
  
  idx1 <- which(sd$vol_ch == max(sd$vol_ch))
  idx2 <- which(ii == min(ii))
  idx3 <- which(sd2$vol_ch == max(sd2$vol_ch))
  idx3 <- idx3 + nrow(sd3)
  
  # print(idx1)
  # print(idx3)
  
  # print(data[(idx1-1):(idx1+1),])
  # print(data[(idx2-1):(idx2+1),])
  # print(data[(idx3-1):(idx3+1),])
  
  sdsd1 <- data[idx1:idx2,]
  sdsd2 <- data[(idx2+1):idx3,]
  
  # sdsdc <- lapply(paste('sdsd1', seq(1,2,1), sep=''), get)
  # sdsdc <- lapply(c('sdsd1', 'sdsd2'), get)
  # sdsdc1 <- sdsdc[[1]]
  
  # print(head(sdsd1))
  # print(tail(sdsd1))
  # print(head(sdsd2))
  # print(tail(sdsd2))
  detach(data)
  return(sdsd1)
}

splitdata2 <- function(data) {
  
  # data <- 삼성전자;
  attach(data)
  as.numeric(time)
  as.numeric(vol_ch)
  as.numeric(vol)
  
  # str(minDatas)
  # colnames(minDatas) <- c('time', 'price')
  
  ii <- vol[-1] - vol[-length(vol)]
  
  sd <- subset(data, time <= 090500)
  sd2 <- subset(data, time >= 152500)
  sd3 <- subset(data, time < 152500)
  
  # print(sd2)
  
  idx1 <- which(sd$vol_ch == max(sd$vol_ch))
  idx2 <- which(ii == min(ii))
  idx3 <- which(sd2$vol_ch == max(sd2$vol_ch))
  idx3 <- idx3 + nrow(sd3)
  
  # print(idx1)
  # print(idx3)
  
  # print(data[(idx1-1):(idx1+1),])
  # print(data[(idx2-1):(idx2+1),])
  # print(data[(idx3-1):(idx3+1),])
  
  sdsd1 <- data[idx1:idx2,]
  sdsd2 <- data[(idx2+1):idx3,]
  
  # sdsdc <- lapply(paste('sdsd1', seq(1,2,1), sep=''), get)
  # sdsdc <- lapply(c('sdsd1', 'sdsd2'), get)
  # sdsdc1 <- sdsdc[[1]]
  
  # print(head(sdsd1))
  # print(tail(sdsd1))
  # print(head(sdsd2))
  # print(tail(sdsd2))
  detach(data)
  return(sdsd2)
}
# sdsdc <- 1
# sdsd1 <- splitdata1(삼성전자)
# sdsd2 <- splitdata2(삼성전자)


# 데이터 범위 조정

ctrd <- function(data, timediff = NULL, starttime = NULL, endtime = NULL) {
  sdsd1 <- splitdata1(data)
  ctrData1 <- ctrda(sdsd1, timediff, starttime, endtime)
  
  sdsd2 <- splitdata2(data)
  ctrData2 <- ctrda(sdsd2, timediff, starttime, endtime)
  
  ctrData <- rbind(ctrData1, ctrData2)
}

# c삼성전자 <- 1
# c삼성전자 <- ctrd(삼성전자, 10)


# ud 트렌드 분석

udtrd <- function(data, timediff = NULL, starttime = NULL, endtime = NULL) {
  if (is.null(timediff)) {lag <- 100}
  else {lag <- timediff}
  if (is.null(starttime)) {start <- 90000}
  else {start <- starttime}
  if (is.null(endtime)) {end <- 152000}
  else {end <- endtime}
  # data <- cell
  # lag <- 100
  # start <- 90000
  # end <- 152000
  
  cdata <- ctrda(data, lag, start, end)
  cdata$ud <- ud(cdata)
  cdata$price <- scale(cdata$price)
  
  cd1 <- cdata$ud[7:nrow(cdata)]
  cd2 <- cdata$ud[6:(nrow(cdata)-1)]
  cd3 <- cdata$ud[5:(nrow(cdata)-2)]
  cd4 <- cdata$ud[4:(nrow(cdata)-3)]
  cd5 <- cdata$ud[3:(nrow(cdata)-4)]
  cd6 <- cdata$ud[2:(nrow(cdata)-5)]
  cd7 <- cdata$ud[1:(nrow(cdata)-6)]
  cd <- data.frame(cd1, cd2, cd3, cd4, cd5, cd6, cd7)
  return(cd)
}


# 시간 빼기

timeminus <- function(a, b) {
  
  hh <- a %/% 10000
  mmtt <- a %% 10000
  mm <- mmtt %/% 100
  tt <- mmtt %% 100
  
  lhh <- b %/% 10000
  lmmtt <- b %% 10000
  lmm <- lmmtt %/% 100
  ltt <- lmmtt %% 100
  
  dh <- lhh
  dm <- lmm
  dt <- ltt
  # tttresult <- tt < lag
  if (tt < dt) {
    dm <- dm + 1
    # mmmresult <- mm == 0
  }
  if (mm < dm) {
    dh <- dh + 1
  }
  it <- (hh - dh) * 10000 + 
    ((mm + 60 - dm) %% 60) * 100 + 
    ((tt + 60 - dt) %% 60)
  return(it)
}

# print(timeminus(90000, 110))


# 시간 더하기

timeplus <- function(a, b) {
  hh <- a %/% 10000
  mmtt <- a %% 10000
  mm <- mmtt %/% 100
  tt <- mmtt %% 100
  
  lhh <- b %/% 10000
  lmmtt <- b %% 10000
  lmm <- lmmtt %/% 100
  ltt <- lmmtt %% 100
  
  ph <- lhh
  pm <- lmm
  pt <- ltt
  # tttresult <- tt < lag
  if (tt + pt >= 60) {
    pm <- pm + 1
    # mmmresult <- mm == 0
  }
  if (mm + pm >= 60) {
    ph <- ph + 1
  }
  it <- (hh + ph) * 10000 + 
    ((mm + pm) %% 60) * 100 + 
    ((tt + pt) %% 60)
  return(it)
}


# knnacc

knnacc <- function(train, test, realdata) {
  library(class)
  knn <- knn(cltrd, hmtrd, hmtrd[, 7], k = 1, prob = T)
  
  knnconf <- table(knn, hmtrd[, 7])
  knnacc <- sum(diag(knnconf)) / nrow(hmtrd)
  return(knnacc)
}

# 시계열 그래프 그리기

tgraph <- function(x, y) {
  library(ggplot2)
  df <- data.frame(x, y)
  # df
  pg <- ggplot(df, aes(df$x, df$y))
  pg <- pg + geom_line()

  # pg <- pg + geom_hline(yintercept = 70)
  # pg <- pg + geom_hline(yintercept = 30)
  
  # print(pg)
  return(pg)
}

# rsi 그래프 그리기

plotrsi <- function(x, y, n = 14) {
  # install.packages('TTR')
  library(TTR)
  # data <- mhanmi
  # data <- sams
  rsid <- RSI(y, n, maType = 'WMA')
  
  rsid
  
  idx <- which(rsid > 0.01)
  nx <- x[idx]
  ny <- rsid[idx]
  pg <- tgraph(nx, ny)
  pg <- pg + geom_hline(yintercept = 70)
  pg <- pg + geom_hline(yintercept = 30)
  print(pg)
}

# 필드명 변경


chfield <- function(data) {
  library(stringr)
  cname <- colnames(data)
  for (i in 1:length(cname)) {
    tmp <- str_split(cname[i], '\\.')
    cname[i] <- tmp[[1]][2]
  }
  return(cname)
}

yyyymmdd <- function(date) {
  library(stringr)
  date <- as.character(date)
  t <- strptime(date, format='%Y%m%d')
  return(t)
}

hhmmtt <- function(time) {
  library(stringr)
  time <- as.character(time)
  time <- str_pad(time, 6, pad = "0")
  t <- strptime(time, format='%H%M%S')
  return(t)
}


# ma 교차 이용 거래

matrade <- function(data, shortterm, longterm) {
  st <- shortterm
  lt <- longterm
  
  data$cls_mast = ma(data[,'close'], order=st)
  data$cls_malt = ma(data[,'close'], order=lt)
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$cls_mast[i]) && !is.na(data$cls_malt[i])) {
      data$ud[i] <- data$cls_mast[i] - data$cls_malt[i]
    } else {
      data$ud[i] <- NA
    }
  }
  
  profit <- 0
  # if (lt %% 2 == 0) {
  #   lth <- lt %/% 2
  # } else {
  #   lth <- lt %/% 2 - 1 
  # }
  lth <- lt %/% 2
  
  for (i in (lth + 2):nrow(data)) {
    ii <- i - lth
    if (!is.na(data$ud[ii - 1]) && !is.na(data$ud[ii])){
      if (data$ud[ii - 1] >= 0 && data$ud[ii] < 0) {
        profit <- profit + data$close[i]
      }
      if (data$ud[ii - 1] <= 0 && data$ud[ii] > 0) {
        profit <- profit - data$close[i]
      }  
    }
  }
  
  profit
}


# 최적 term 구하기

bestterm <- function(data, seqd = c(7,14,21,30,60,90,180,270,365)) {
  library('forecast')
  maxprofit <- 0
  bestst <- 0
  bestlt <- 0
  
  seqd <- seqd[seqd < nrow(data)]
  for (i in seqd) {
    for (j in seqd) {
      if (i < j) {
        tmp <- matrade(data, i, j)
        if (tmp > maxprofit) {
          maxprofit <- tmp
          bestst <- i
          bestlt <- j
        }
      }
    }
  }
  data.frame(data$stock[1], bestst, bestlt, maxprofit)
}

matrdgraph <- function(data, st, lt, starttime = "1000-01-01") {
  library(ggplot2)
  data$Date <- strptime(data$date, format='%Y%m%d')
  
  idx <- which(data$Date >= starttime)
  ydata <- data[idx, ]
  
  ydata$cls_mast = ma(ydata[, c('close')], order=st)
  ydata$cls_malt = ma(ydata[, c('close')], order=lt)
  
  pgy <- ggplot() + 
    geom_line(data = ydata,
              aes(x = Date, y = close, colour = "Counts")) +
    geom_line(data = ydata, aes(x = Date, y = cls_mast,
                                colour = "Long Term Moving Average")) + 
    geom_line(data = ydata, aes(x = Date, y = cls_malt,
                                colour = "Short Term Moving Average")) + 
    ylab('Close Price')
  pgy
}
