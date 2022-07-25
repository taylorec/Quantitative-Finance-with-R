library(quantmod)
library(tseries)
library(PerformanceAnalytics)

getSymbols('^GSPC', from='2016-01-01')
getSymbols('NKE', from='2016-01-01')
getSymbols('SONY', from='2016-01-01')
getSymbols('DIS', from='2016-01-01')
sym1 <- NKE$NKE.Adjusted
sym2 <- SONY$SONY.Adjusted
sym3 <- DIS$DIS.Adjusted
mr <- GSPC$GSPC.Adjusted
ret1 <- dailyReturn(sym1, type='log')
ret2 <- dailyReturn(sym2, type='log')
ret3 <- dailyReturn(sym3, type='log')
mr.ret <- dailyReturn(mr, type='log') # market return



# NIKE beta
plot(density(ret1)) 
CAPM.alpha(ret1, mr.ret)
CAPM.beta(ret1, mr.ret)
VaR(ret1, p=0.99) # value at risk at 1%
ES(ret1, p=0.99) # expected short fall at 1%

# SONY beta
plot(density(ret2))
CAPM.alpha(ret2, mr.ret)
CAPM.beta(ret2, mr.ret)
VaR(ret2, p=0.99) # value at risk at 1%
ES(ret2, p=0.99) # expected short fall at 1%

# DIS beta 
plot(density(ret3))
CAPM.alpha(ret3, mr.ret)
CAPM.beta(ret3, mr.ret)
VaR(ret3, p=0.99) # value at risk at 1%
ES(ret3, p=0.99) # expected short fall at 1%

# Simulated VaR under normal distributions
n_sim = 240
ret1.mean <- mean(ret1)
ret2.mean <- mean(ret2)
ret3.mean <- mean(ret3)
ret1.sd <- sd(ret1)
ret2.sd <- sd(ret2)
ret3.sd <- sd(ret3)
sim_norm_ret1<-rnorm(n_sim, ret1.mean, ret1.sd)
sim_norm_ret2<-rnorm(n_sim, ret2.mean, ret2.sd)
sim_norm_ret3<-rnorm(n_sim, ret3.mean, ret3.sd)
VaR(sim_norm_ret1, p=0.99)
VaR(sim_norm_ret2, p=0.99)
VaR(sim_norm_ret3, p=0.99)
