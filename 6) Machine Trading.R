library("caret")
library("corrplot")
library("forecast")
library("kernlab")
library("neuralnet")
library("PerformanceAnalytics")
library("quantmod")
library("tseries")
library("xgboost")
library(gbm)
library(randomForest)
library(mgcv)

getSymbols('SONY', from='2014-01-01')
getSymbols('NKE', from='2014-01-01')
sym1 <- SONY$SONY.Adjusted
sym2 <- NKE$NKE.Adjusted
ret.sym1 <- dailyReturn(sym1, type='log')
ret.sym2 <- dailyReturn(sym2, type='log')
date <- Sys.Date()
y <- xts(0.001, date)
ret.sym1 <- rbind(ret.sym1, y)
ret.sym2 <- rbind(ret.sym2, y)

# Lag Predictor Features 
ret.sym3 <- Lag(ret.sym1,k=1)
ret.sym4 <- Lag(ret.sym1,k=2)
ret.sym5 <- Lag(ret.sym1,k=3)
ret.sym6 <- Lag(ret.sym2,k=1)
ret.sym7 <- Lag(ret.sym2,k=2)
ret.sym8 <- Lag(ret.sym2,k=3)

sym.all <- cbind(ret.sym1, ret.sym3, ret.sym4, ret.sym5, ret.sym6, ret.sym7, ret.sym8)
colnames(sym.all) <- cbind('ret.sym1', 'ret.sym3', 'ret.sym4', 'ret.sym5', 'ret.sym6', 'ret.sym7', 'ret.sym8')
sym.all <- sym.all[complete.cases(sym.all),]

# Training Range
sym.t <- window(sym.all, start='2014-01-01', end='2019-12-31')

# Machine Trading Models
set.seed(101)

# Linear Regression Model
# MT.sym <- lm(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t)
# summary(MT.sym)
# MT.sym <- lm(ret.sym~ret.sym1+ret.sym2+ret.sym4, data=sym.t) #significant predictors
# summary(MT.sym)

# Random Forest Model
# MT.sym <- randomForest(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t,importance=TRUE)

# Gradient Boosting Model
 MT.sym <- randomForest(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t,importance=TRUE)

# XGB Model
# MT.sym <- train(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t, method="xgbTree", preProcess="pca")

# Neural Net Model
# tsctrlt <- trainControl(method="timeslice",initialWindow=168,horizon=82,fixedWindow=TRUE)
# MT.sym <- train(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t, method="neuralnet", preProcess="pca", trControl=tsctrlt)

# Support Vector Machine Model
# MT.sym <- train(ret.sym1~ret.sym3+ret.sym4+ret.sym5+ret.sym6+ret.sym7+ret.sym8, data=sym.t, method="svmRadial", preProcess="pca")

#Trading Range
sym.i <- window(sym.all, start='2020-01-01')

#Trading Strategy
MT.sym.mi <- predict(MT.sym,newdata=sym.i)
MT.sym.mdfi <- cbind(index(sym.i),as.data.frame(MT.sym.mi))
MT.sym.mli <- xts(MT.sym.mdfi[,2],order.by=as.Date(MT.sym.mdfi[,1]))
MT.sym.ms <- window(MT.sym.mli,start="2020-01-01")

#Regression trading signals
MT.sym.msig <- Lag(ifelse(Lag(MT.sym.ms)<0&MT.sym.ms>0,1,ifelse(Lag(MT.sym.ms)>0&MT.sym.ms<0,-1,0)))
MT.sym.msig[is.na(MT.sym.msig)] <- 0

MT.sym.mpos <- ifelse(MT.sym.msig>1,1,0)
for(i in 1:length(MT.sym.mpos)){MT.sym.mpos[i] <- ifelse(MT.sym.msig[i]==1,1,ifelse(MT.sym.msig[i]==-1,0,MT.sym.mpos[i-1]))}
MT.sym.mpos[is.na(MT.sym.mpos)] <- 0
MT.sym.mtr <- cbind(MT.sym.ms,MT.sym.msig,MT.sym.mpos)
colnames(MT.sym.mtr) <- c("MT.symb.ms","MT.sym.msig","MT.sym.mpos")
View(MT.sym.mtr)

# Trading Performance since 2019
MT.sym.mret <- MT.sym.mpos*ret.sym[,1]
MTmcomp <- cbind(MT.sym.mret, ret.sym[,1])
colnames(MTmcomp) <- c("MT.sym.mret","ret.sym")
table.AnnualizedReturns(na.omit(MTmcomp))
charts.PerformanceSummary(MTmcomp)

# Last 10 trading-days trading performance
Return.cumulative(tail(MTmcomp, 10))
charts.PerformanceSummary(tail(MTmcomp, 10))

# Variation at Risk: comparing trading strategy vs buy and hold
quantile(MT.sym.mret, probs=0.05)
quantile(ret.sym, probs=0.05)







