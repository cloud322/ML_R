daily_data <- calltable('bydaym2', 'y')
# head(daily_data)
# daily_data <- arrange(daily_data, stock, date)

stocklist <- unique(daily_data$stock)
# stocklist <- stocklist[1:2]

bb <- data.frame()
slcnt <- 0
# install.packages('progress')
for (sl in stocklist) {
  slcnt <- slcnt + 1
  # slcnt <- 132
  print(sprintf('%d / %d : %s', slcnt, length(stocklist), sl))
  idx <- which(daily_data$stock == sl)
  sdata <- daily_data[idx, ]
  rownames(sdata) <- 1:nrow(sdata)
  # matrdgraph(daily_data, 30, 60, "2017-04-01")
  
  bb1 <- bestrsi(sdata)
  
  bb <- rbind(bb, bb1)
  
  # bb  
}


savecsv(bb, 'rsiresultU', 1)
savecsv(bb, 'rsiresultA', 0)
