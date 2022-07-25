library(corrplot)
library(PerformanceAnalytics)
library(tseries)
library(xts)
library(Quandl)
Quandl.api_key("fn-jfrd1jkpKosDRQFsA")
library(quantmod)
library(copula)
library(ggplot2)

#Energy Stock Symbols
getSymbols("ENPH", from='2020-01-01', to='2021-01-24')
getSymbols("NEE",  from='2020-01-01', to='2021-01-24')
getSymbols("XOM",  from='2020-01-01', to='2021-01-24')
getSymbols("FSLR", from='2020-01-01', to='2021-01-24')
df_stock <- Ad(cbind(ENPH, NEE, XOM, FSLR))
df_stock
df_stock_ret <- diff(log(df_stock))
df_stock_ret <- df_stock_ret[-1, ] #xts object

#Mean, Variance, std Dev, Skewness, Excess Kurtosis
ENPH_stats = c(mean(df_stock_ret[,1]), var(df_stock_ret[,1]), 
			sd(df_stock_ret[,1]), kurtosis(df_stock_ret[,1]))
NEE_stats = c(mean(df_stock_ret[,2]), var(df_stock_ret[,2]), 
			sd(df_stock_ret[,2]), kurtosis(df_stock_ret[,2]))
XOM_stats = c(mean(df_stock_ret[,3]), var(df_stock_ret[,3]), 
			sd(df_stock_ret[,3]), kurtosis(df_stock_ret[,3]))
FSLR_stats = c(mean(df_stock_ret[,4]), var(df_stock_ret[,4]), 
			sd(df_stock_ret[,4]), kurtosis(df_stock_ret[,4]))
df_stock_ret_stats = rbind(ENPH_stats, NEE_stats, XOM_stats, FSLR_stats)
colnames(df_stock_ret_stats) = c('Mean', 'Var', 'St Dev', 'Skewness')
df_stock_ret_stats
boxplot(coredata(df_stock_ret))
summary(df_stock_ret)

#Statistics
mu.ENPH=mean(df_stock_ret[,1])
sig.ENPH=sd(df_stock_ret[,1])
sig2.ENPH=var(df_stock_ret[,1])
mu.NEE=mean(df_stock_ret[,2])
sig.NEE=sd(df_stock_ret[,2])
sig2.NEE=var(df_stock_ret[,2])
mu.XOM=mean(df_stock_ret[,3])
sig.XOM=sd(df_stock_ret[,3])
sig2.XOM=var(df_stock_ret[,3])
mu.FSLR=mean(df_stock_ret[,4])
sig.FSLR=sd(df_stock_ret[,4])
sig2.FSLR=var(df_stock_ret[,4])
rho.ENPH.NEE=cor(df_stock_ret[,1], df_stock_ret[,2])
rho.ENPH.XOM=cor(df_stock_ret[,1], df_stock_ret[,3])
rho.ENPH.FSLR=cor(df_stock_ret[,1], df_stock_ret[,4])
rho.NEE.XOM=cor(df_stock_ret[,2], df_stock_ret[,3])
rho.NEE.FSLR=cor(df_stock_ret[,2], df_stock_ret[,4])
rho.XOM.FSLR=cor(df_stock_ret[,3], df_stock_ret[,4])
sig.ENPH.NEE <- rho.ENPH.NEE*sig.ENPH*sig.NEE
sig.ENPH.XOM <- rho.ENPH.XOM*sig.ENPH*sig.XOM
sig.ENPH.FSLR <- rho.ENPH.FSLR*sig.ENPH*sig.FSLR
sig.NEE.XOM <- rho.NEE.XOM*sig.NEE*sig.XOM
sig.NEE.FSLR <- rho.NEE.FSLR*sig.NEE*sig.FSLR
sig.XOM.FSLR <- rho.XOM.FSLR*sig.XOM*sig.FSLR

#Portfolio Frontier Value at Risk (VaR)
Pft.portion1 <- seq(from=0.1, to=0.4, by=0.01)
Pft.portion2 <- 1-Pft.portion1
Pft.portion3 <- seq(from=0.1, to=0.4, by=0.01)
Pft.portion4 <- 1-Pft.portion1
mu.p <- (Pft.portion1*mu.ENPH) + (Pft.portion2*mu.NEE) + 
 	(Pft.portion3*mu.XOM) + (Pft.portion4*mu.FSLR)
sig2.p <- Pft.portion1*Pft.portion1*sig2.ENPH + Pft.portion2*Pft.portion2*sig2.NEE + 
	Pft.portion3*Pft.portion3*sig2.XOM + Pft.portion4*Pft.portion4*sig2.FSLR +
 	2*Pft.portion1*Pft.portion2*sig.ENPH.NEE + 2*Pft.portion1*Pft.portion3*sig.ENPH.XOM + 2*Pft.portion1*Pft.portion4*sig.ENPH.FSLR +
	2*Pft.portion2*Pft.portion3*sig.NEE.XOM + 2*Pft.portion2*Pft.portion4*sig.NEE.FSLR + 2*Pft.portion3*Pft.portion4*sig.XOM.FSLR
sig.p <- sqrt(sig2.p)
plot(sig.p, mu.p, type='b', pch=16,
	xlab=expression(sigma[p]), ylab=expression(mu[p]))