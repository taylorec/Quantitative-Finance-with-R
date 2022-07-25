library("quantmod")
library("PerformanceAnalytics")
library(TSstudio)
library(forecast)
library(rugarch)
library(rmgarch)
library(vars)
getSymbols('SONY')
sym <- SONY$SONY.Adjusted
sym <- window(sym,start="2011-07-01")
ret.sym <- dailyReturn(sym, type='log')

## Stock Price Time Series and Forecasting

plot(sym)

ts_plot(sym, 
        title = "Stock Price",
        Ytitle = "Price",
        Xtitle = "Date")

# ACF and PACF on price
par(mfrow=c(1,2))
acf(sym, lag.max = 10)
pacf(sym, lag.max = 10)

# plot of differences
PriceDiff <- diff(sym, 1)
PriceDiff <- window(PriceDiff ,start="2021-07-01")
plot(PriceDiff) 

# ACF and PACF on differences
PriceDiff <- diff(sym, 1)
PriceDiff <- na.omit(PriceDiff)
par(mfrow=c(1,2))
acf(PriceDiff, lag.max = 10)
pacf(PriceDiff, lag.max = 10)

# Series shows (2,1,2)
arima_model <- auto.arima(sym)
arima_model
tsdiag(arima_model)

# Forecasting
FutureForecast<-forecast(arima_model,h=25) 
FutureForecast
plot_forecast(FutureForecast) 

# Check for significant autocorrelation in the residuals
Box.test(FutureForecast$residuals, lag=20, type="Ljung-Box") 

# GARCH
gspec.ru <- ugarchspec(mean.model=list( armaOrder=c(0,0)), distribution="std") 
gfit.ru <- ugarchfit(gspec.ru, sym) 
gfit.ru
GARCH.FutureForecast=ugarchforecast(gfit.ru, n.ahead = 5) 
GARCH.FutureForecast 



## Time Series and Forecasting Returns

# Histogram and plot of returns
par(mfrow=c(1,2))
hist(ret.sym, breaks=100, main='Histogram of Returns')
plot(ret.sym)

# 99% VaR shows the 1% risk of return on any given day
quantile(ret.sym, probs=0.01)

# ACF and PACF on returns
par(mfrow=c(1,2))
acf(ret.sym, lag.max = 10)
pacf(ret.sym, lag.max = 10)

# ARIMA
arima_ret <- auto.arima(ret.sym)
arima_ret
tsdiag(arima_ret)

# Forecasting
ReturnsForecast<-forecast(arima_ret,h=25) 
head(ReturnsForecast)
plot_forecast(ReturnsForecast)

# GARCH
#  specify the model
gspec.ru <- ugarchspec(
     variance.model = list(garchOrder = c(1, 1)),     
     mean.model=list(armaOrder=c(0,0)), distribution="std") 
gfit.ru <- ugarchfit(gspec.ru, ret.sym) 
gfit.ru
GARCH.FutureForecast=ugarchforecast(gfit.ru, n.ahead = 5) 
GARCH.FutureForecast 

# News impact
ni.garch11 <- newsimpact(gfit.ru)
plot(ni.garch11$zx, ni.garch11$zy, type="l", lwd=2, col="blue", main="GARCH(1,1) - News Impact", ylab=ni.garch11$yexpr, xlab=ni.garch11$xexpr)

# EGARCH
egarch11.spec = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))
ret.egarch11.fit = ugarchfit(spec=egarch11.spec, data=ret.sym)
coef(ret.egarch11.fit)

ni.egarch11 <- newsimpact(ret.egarch11.fit)
plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact",
ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)


## Cointegration - Cross hedging jet fuel
setwd('C:/Users/evan/Documents/Data')
library("urca")
prices <- read.zoo("JetFuelHedging.csv", sep = ",", FUN = as.yearmon, format = "%Y-%m", header = TRUE)
head(prices)
simple_mod <- lm(diff(prices$JetFuel) ~ diff(prices$HeatingOil)+0)
# The +0 term means that the intercept is set to zero; that is, no cash holdings.
# optimal hedge ratio - the beta coefficient of the regression
summary(simple_mod)

plot(prices$JetFuel, main = "Jet Fuel and Heating Oil Prices", xlab = "Date", ylab = "USD")
lines(prices$HeatingOil, col = "red")
cor(prices)

# Jet Fuel unit root (non-stationarity) test (Dickey-Fuller test)
# The null hypothesis of non-stationarity
jf_adf <- ur.df(prices$JetFuel, type = "drift")
summary(jf_adf)

# Heating Oil unit root (non-stationarity) test (Dickey-Fuller test)
# The null hypothesis of non-stationarity
ho_adf <- ur.df(prices$HeatingOil, type = "drift")
summary(ho_adf)

mod_static <- summary(lm(prices$JetFuel ~ prices$HeatingOil))
error <- residuals(mod_static)
# Errors unit root (non-stationarity) test (Dickey-Fuller test)
error_cadf <- ur.df(error, type = "none")
summary(error_cadf)
# reject the null hypothesis of non-stationarity

# Error-Correction Model (ECM)
# dynamic model of how (and how fast) the 
# system moves back to the static equilibrium
djf <- diff(prices$JetFuel)
dho <- diff(prices$HeatingOil)
error_lag <- lag(error, k = -1)
mod_ecm <- lm(djf ~ dho + error_lag)
summary(mod_ecm)

# Vector autoregressive models (VAR)
getSymbols('MSFT', from='2021-01-02', to='2022-06-30')
getSymbols('SNP', from='2021-01-02', , to='2022-06-30')
getSymbols('DTB3', src='FRED')

#  daily close-to-close returns
chartSeries(ClCl(MSFT))

# Log returns
ret.MSFT <- dailyReturn(MSFT, type='log')
ret.SNP <- dailyReturn(SNP, type='log')
ret.DTB3 <- dailyReturn(DTB3, type='log')
dataDaily <- na.omit(merge(ret.SNP,ret.MSFT,ret.DTB3), join='inner')

# Monthly Closing Prices
SNP.M  <- to.monthly(ret.SNP)$SNP.ret.Close
MSFT.M <- to.monthly(ret.MSFT)$MSFT.ret.Close
DTB3.M <- to.monthly(ret.DTB3)$DTB3.sub.Close

# VAR model
var1 <- VAR(dataDaily, lag.max=4, ic="AIC")
summary(var1)
plot(var1)       #Diagram of fit and residuals for each variables
coef(var1)       #concise summary of the estimated variables
residuals(var1)  #list of residuals (of the corresponding ~lm)
fitted(var1)     #list of fitted values
Phi(var1)        #coefficient matrices of VMA representation
var.pred <- predict(var1, n.ahead=10, ci=0.95)
var.irf <- irf(var1) # Impulse responses
plot(var.irf)


# SVAR model
amat <- diag(3)
amat[2, 1] <- NA
amat[2, 3] <- NA
amat[3, 1] <- NA
svar1 <- SVAR(var1, estmethod='direct', Amat = amat)
irf.svar1 <- irf(svar1)
plot(irf.svar1)

# Volatility Modeling
par(mfrow=c(2,2))
acf(ret.MSFT, main="Return ACF");
pacf(ret.MSFT, main="Return PACF");
acf(ret.MSFT^2, main="Squared return ACF");
pacf(ret.MSFT^2, main="Squared return PACF")

m=mean(ret.MSFT)
s=sd(ret.MSFT)
par(mfrow=c(1,2))
hist(ret.MSFT, nclass=40, freq=FALSE, main='Return histogram');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(ret.MSFT), main='Return empirical distribution');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")

kurtosis(ret.MSFT)

# QQ-Plot
# Deviations from this straight line may indicate the presence of fat tails
qqnorm(ret.MSFT)
qqline(ret.MSFT)


