library("quantmod")
library("PerformanceAnalytics")

getSymbols('DIS')
getSymbols('NKE')
sym <- DIS$DIS.Adjusted
sym2 <- NKE$NKE.Adjusted

ret.sym <- dailyReturn(sym, type='log')
ret.sym2 <- dailyReturn(sym2, type='log')

x = mean(ret.sym)
s = sd(ret.sym)

# normal distribution
dnorm(ret.sym, x, s)
y <- dnorm(ret.sym, mean = x, sd =s)
plot(ret.sym,y)

# cumulative distribution function
pnorm(ret.sym, x, s)

# probably of getting a return less than or equal to 2%
pnorm(.02, mean = x, sd = s)

# probably of getting a return greater than 5%
1 - pnorm(.05, mean = x, sd = s)

# generate 5 random numbers from a normal distribution
rnorm(5, mean = x, sd = s)

# density function of the lognormal distribution
vol <- DIS$DIS.Volume
dlnorm(vol, mean = mean(vol), sd = sd(vol))
y <- dlnorm(vol, meanlog = mean(vol), sdlog= sd(vol))
plot(vol,y)

# cdf for volume
plnorm(vol, mean = mean(vol), sd = sd(vol))
y <- plnorm(vol, meanlog = mean(vol), sdlog= sd(vol))
plot(y)

# quantiles of the distribution
ret.sym <- dailyReturn(sym)
x = mean(ret.sym)
s = sd(ret.sym)
head(qnorm(ret.sym, x, s))

# quantiles of the log distribution
head(qlnorm(ret.sym, x, s))

# Random Sample
(RandomSample <- DIS[sample(1:nrow(DIS), 10, replace=FALSE),])

mean(DIS$DIS.Volume) # Mean trading volume
median(DIS$DIS.Volume) # median trading volume

# summary statistics
summary(DIS$DIS.Volume)
summary(DIS$DIS.Adjusted)

library(e1071)
moment(DIS$DIS.Volume, order=3, center=TRUE) 
kurtosis(DIS$DIS.Volume)
skewness(DIS$DIS.Volume)

# correlation
ret.sym <- dailyReturn(sym)
ret.sym2 <- dailyReturn(sym2)
ret.sym <- window(ret.sym, start='2017-01-01')
ret.sym2 <- window(ret.sym2, start='2017-01-01')
cor(ret.sym, ret.sym2, method="pearson") 

par(mfrow=c(2,2))
acf(DIS$DIS.Adjusted) 
pacf(DIS$DIS.Adjusted)
acf(DIS$DIS.Volume) 
pacf(DIS$DIS.Volume)

# Hypothesis testing
## test if returns are less than 0.00%
t.test(ret.sym, mu = 0.00, alternative="less")
# fail to reject null hypothesis

# Hypothesis testing
## test if returns are equal
t.test(ret.sym, ret.sym2, paired=TRUE, alternative="two.sided")
# fail to reject null hypothesis
# true difference in means is not equal to 0
mean(ret.sym)
mean(ret.sym2)

# Regression
Y<-DIS$DIS.Close
X1<-DIS$DIS.Open 
X2 <- DIS$DIS.Low
fit <- lm(Y ~ X1+X2) 
summary(fit) 

# Outlier detection with Boxplot
boxplot(vol, main="Volume", boxwex=0.1) # boxplot of volume
boxplot(ret.sym, main="Returns", boxwex=0.1) # boxplot of returns

# Quantile Plot
chart.QQPlot(ret.sym, main="Returns")

hist(ret.sym)

# Standardization (z scores)
scale(vol, center=TRUE, scale=FALSE) 
scale(vol, center=TRUE, scale=TRUE) # entered column values are divided by the column's standard deviation
scale(ret.sym, center=TRUE, scale=FALSE)
scale(ret.sym, center=TRUE, scale=TRUE) # entered column values are divided by the column's standard deviation

# Normalization
## normalized = (x-min(x))/(max(x)-min(x))
X2 <- (vol - min(vol))/(max(vol)-min(vol))

# Regression
Y<-DIS$DIS.Close
X1<-DIS$DIS.Open 
fit <- lm(Y ~ X1+X2) 
summary(fit)


