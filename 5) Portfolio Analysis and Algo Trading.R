library(quantmod)
library(tseries)
library(PerformanceAnalytics)

getSymbols('DIS', from='2016-01-01')
sym <- DIS$DIS.Adjusted
ret.sym<- Delt(sym,k=1:3) # 1, 2 and 3 lag returns
ret.sym <- ret.sym[complete.cases(ret.sym),]
par(mfrow=c(2,1))
plot(ret.sym[,1])
plot(sym)

# returns plot
charts.PerformanceSummary(ret.sym)

# Price and Volume chart
chartSeries(DIS, type='line', subset='2022', theme=chartTheme('white'))

# Candlestick chart
chartSeries(DIS, type='candlesticks', subset='2022', 
         up.col='green', down.col='red', theme=chartTheme('white'))


# Simple-moving Average trading signal
# Buy signal - when short-run SMA crosses from below to above long-run SMA
# Sell signal - when short-run SMA crosses from above to below long-run SMA
chartSeries(DIS, type='line', theme=chartTheme('white'))
addSMA(n=30, on=1, col='blue')
addSMA(n=200, on=1, col='red')


# Exponential-moving Average trading signal
# Buy signal - when short-run EMA crosses from below to above long-run EMA
# Sell signal - when short-run EMA crosses from above to below long-run EMA
chartSeries(DIS, type='line', theme=chartTheme('white'))
addEMA(n=30, on=1, col='blue')
addEMA(n=200, on=1, col='red')


# Bollinger band trading signal
# Buy signal - when price is above the band
# Sell signal - when price is below the band
chartSeries(DIS, type='line', theme=chartTheme('white'))
addBBands(n=20, sd=2)


# Momentum trading signal
# Buy signal - when momentum changes from negative to positive
# Sell signal - when momentum changes from positive to negative
M <- momentum(Cl(DIS), n=2)
tail(M)
chartSeries(DIS, theme=chartTheme('white'))
addMomentum(n=1)


# Rate of Change (ROC) trading signal
# Buy signal - when ROC changes from negative to positive
# Sell signal - when ROC changes from positive to negative
roc <- ROC(Cl(DIS), n=2)
tail(roc)
chartSeries(DIS, theme=chartTheme('white'))
addROC(n=7)


# Moving average convergence/divergence (MACD) trading signal
# Buy signal - when MACD crosses from below to above the signal line
# Sell signal - when MACD crosses from above to below the signal line
macd <- MACD(Cl(DIS), nFast=12, nSlow=26, nSig=9, percentage=FALSE)
tail(macd)
chartSeries(DIS, theme=chartTheme('white'))
addMACD(fast=12, slow=26, signal=9, type='SMA')


# Relative Strenght Index (RS) trading signal
# Buy Signal - when RSI is less than 30
# Sell Signal - when RSI is higher than 30
rsi <- RSI(Cl(DIS), n=14, maType='EMA')
tail(rsi)
chartSeries(DIS, theme=chartTheme('white'))
addRSI(n=14, maType='EMA')


# Correlation based pairs trading
getSymbols(c('SONY', 'NKE'), from='2016-01-01')
sym1 <- NKE$NKE.Adjusted
sym2 <- SONY$SONY.Adjusted
pairs.trd <- cbind(sym, sym1, sym2)
cor(pairs.trd)
plot(pairs.trd)

correlation <- function(x)  
{ 
         result <- cor(x[,2],x[,3]) 
         return (result) 
} 
corr.pr <- rollapply(pairs.trd,30,correlation,by.column=FALSE) 
plot(corr.pr)
hedge_ratio <- pairs.trd[,2]  / pairs.trd[,3]
hedge_ratio

# bounds
roll_me<- rollapply(hedge_ratio,14,mean)
roll_std<- rollapply(hedge_ratio,14,sd)
n <- 1
roll_ub<- roll_me + n * roll_std
roll_lb<- roll_me - n * roll_std

# signal generation
signal <- NULL
signal <- ifelse(hedge_ratio> roll_ub,-1,ifelse(hedge_ratio< roll_lb,1,0))
lagsignal<- Lag(signal,1)
signal <- ifelse(lagsignal == -1 &hedge_ratio> roll_me, -1,ifelse(lagsignal == 1 &hedge_ratio< roll_me,1,0))
# short signal (-1) when the hedge ratio is over the upper band
# buy signal (1) when the hedge ratio is below the lower band

spread_return <-  dailyReturn(pairs.trd[,2]) - dailyReturn(pairs.trd[,3])
trade_return<- spread_return*lag(signal)

summary(trade_return) 
cumm_ret<- Return.cumulative(trade_return) 
annual_ret<- Return.annualized(trade_return) 
charts.PerformanceSummary(trade_return) 
maxdd<- maxDrawdown(trade_return) 
sd<- StdDev(trade_return) 
sda<- StdDev.annualized(trade_return) 
VaR(trade_return, p = 0.95)   
SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev") 
SharpeRatio.annualized(trade_return, Rf = 0) 


# Co-integration based pairs trading
adf.test(sym2) # price is not stationary
adf.test(ret.sym[,2]) # return is stationary
po.test(pairs.trd, demean=TRUE, lshort=TRUE)
model <- lm(pairs.trd[,2] ~ pairs.trd[,3] + 0)

# extract residuals and test for unit roots
adf.test(as.ts(model$residuals))
# less than critical value at 90% confidence value

roll_me<- rollapply(model$residuals,14,mean)
roll_std<- rollapply(model$residuals,14,sd)
n <- 1
roll_ub<- roll_me + n * roll_std
roll_lb<- roll_me - n * roll_std

signal <- NULL
signal <- ifelse(model$residuals>roll_ub,-1,ifelse(model$residuals<roll_lb,1,0))
lagsignal<- Lag(signal,1)
signal <- ifelse(lagsignal == -1 &model$residuals> roll_me,-1,ifelse(lagsignal == 1 &model$residuals< roll_me,1,0))

trade_return<- spread_return*lag(signal)

summary(trade_return) 
cumm_ret<- Return.cumulative(trade_return) 
annual_ret<- Return.annualized(trade_return) 
charts.PerformanceSummary(trade_return) 
maxdd<- maxDrawdown(trade_return) 
sd<- StdDev(trade_return) 
sda<- StdDev.annualized(trade_return) 
VaR(trade_return, p = 0.95)   
SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev") 
SharpeRatio.annualized(trade_return, Rf = 0) 


# Capital asset pricing model (CAPM)
getSymbols('^GSPC', from='2016-01-01')
getSymbols('NKE', from='2016-01-01')
getSymbols('SONY', from='2016-01-01')
sym1 <- NKE$NKE.Adjusted
sym2 <- SONY$SONY.Adjusted
mr <- GSPC$GSPC.Adjusted
ret1 <- dailyReturn(sym1, type='log')
ret2 <- dailyReturn(sym2, type='log')
mr.ret <- dailyReturn(mr, type='log') # market return

# NIKE beta 
CAPM.alpha(ret1, mr.ret)
CAPM.beta(ret1, mr.ret)

# SONY beta 
CAPM.alpha(ret2, mr.ret)
CAPM.beta(ret2, mr.ret)

# CAPM model 1
model <- lm((ret1 - mr.ret) ~ (ret2 - mr.ret))
model
plot(as.ts(ret1),as.ts(ret2),xlab="Return1",ylab="Return2")
abline(model,col="red")
cor(ret1, ret2)

# CAPM model 2
model2 <- lm(ret1 ~ mr.ret)
model2
plot(as.ts(ret1),as.ts(mr.ret),xlab="Return1",ylab="Market Return")
abline(model2,col="red")

par(mfrow = c(2, 2))
plot(model2)

