## Imply Asset Class

## Correlation of unlisted share prices with market indices

library(xts)
library(quantmod)

## Input
# set working directory
tsp <- read.csv("data/shareprices.csv",stringsAsFactors=FALSE)
tsp$date <- as.Date(tsp$date,format="%m/%d/%y")
tsp <- as.xts(tsp[,-1],order.by=tsp[,1])

## ul = unlisted share prices
ul     <- tsp[,3:5]
header <- names(ul)

## Monthly returns of unlisted
ul <- xts(do.call(cbind.xts,
                   lapply(names(ul),function(x)
                   monthlyReturn(ul[,x]))))
names(ul) <- header

## Market Indices
init_date <- index(ul)[1]
symbols  <- c("SPY","IJR","ACWX")
getSymbols(symbols,from=init_date)
mkt <- xts(do.call(cbind.xts,
                   lapply(symbols,function(sym)
                   monthlyReturn(get(sym)))))
names(mkt) <- symbols

## Results
round(cor(na.omit(cbind.xts(ul,mkt))),3)
