library(quantmod)
library(DMwR)
library(TTR)

#The following function implements the proposed indicator
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)
  v[1] <- Cl(quotes)[1] # correction to the book!
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)
  x <- apply(r,1,function(x)
    sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

ticker <- 'MSFT'
from.date <- '2015-01-01'
getSymbols(ticker, from=from.date)
quote.data <- eval(as.symbol(ticker))

#Inspecting the Values of the T Indicator using candlechart
candleChart(last(quote.data,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()

#Auxiliary functions we will use to obtain the predictors
myATR <- function(x) ATR(HLC(x))[,'atr']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myEMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
mySAR <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]

# StockPred changes with a new change
