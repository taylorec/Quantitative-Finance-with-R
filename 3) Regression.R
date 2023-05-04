library("quantmod")
library("PerformanceAnalytics")

getSymbols('DIS')
getSymbols('NKE')
getSymbols('ENPH')
getSymbols('SONY')
sym <- DIS$DIS.Close
sym2 <- NKE$NKE.Open
sym3 <- ENPH$ENPH.Open
sym4 <- SONY$SONY.Open

ret.sym <- dailyReturn(sym)
ret.sym2 <- dailyReturn(sym2)
ret.sym3 <- dailyReturn(sym3)
ret.sym4 <- dailyReturn(sym4)

ret.sym <- window(ret.sym, start='2017-01-01')
ret.sym2 <- window(ret.sym2, start='2017-01-01')
ret.sym3 <- window(ret.sym3, start='2017-01-01')
ret.sym4 <- window(ret.sym4, start='2017-01-01')

returns <- cbind(ret.sym, ret.sym2)
colnames(returns) <- c('DIS', 'NKE')
returns <- as.data.frame(returns)

# Scatter Plot
YPrice = returns$DIS
XPrice = returns$NKE
cor(returns)

# Regression Model
LinearR.lm = lm(YPrice ~ XPrice, data=returns)
summary(LinearR.lm)

# Residual plot
LinearR.res = resid(LinearR.lm)
plot(YPrice, LinearR.res, ylab="Residuals", xlab="YPrice", 
                          main="Residual Plot") 

# Normality distribution of errors
qqnorm(LinearR.res, ylab="Standardized Residuals",
                       xlab="Normal Scores",  
                       main="Error Normal Distribution plot")  
qqline(LinearR.res)


returns2 <- cbind(ret.sym, ret.sym2, ret.sym3, ret.sym4)
colnames(returns2) <- c('DIS', 'NKE', 'ENPH', 'SONY')
returns2 <- as.data.frame(returns2)
cor(returns2)

# Multiple Regression Model
YPrice = returns2$DIS
X1Price = returns2$NKE
X2Price = returns2$ENPH
X3Price = returns2$SONY
MultipleR.lm = lm(YPrice ~ X1Price+X2Price+X3Price, data=returns2)
summary(MultipleR.lm) 
MultiR.res = resid(MultipleR.lm)
plot(YPrice, MultiR.res, ylab="Residuals", xlab="YPrice", 
                          main="Residual Plot")
qqnorm(MultiR.res, ylab="Standardized Residuals",
       xlab="Normal Scores",  
       main="Error Normal Distribution plot")  
qqline(MultiR.res)

# Multicollinearity
car::vif(MultipleR.lm)

qqnorm(MultiR.res, ylab="Standardized Residuals",
       xlab="Normal Scores",  
       main="Error Normal Distribution plot")  
qqline(MultiR.res)