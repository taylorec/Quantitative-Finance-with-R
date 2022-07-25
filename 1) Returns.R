library("PerformanceAnalytics")
library("quantmod")
getSymbols('DIS')

# Future Value of Money: FV = PV*(1+R)^n
PV = 1000 # present value of $1000
R = 0.07 # APR of 7%
FV1 = PV*(1+R) # future value after 1 year
FV5 = PV*(1+R)^5 # future value after 5 years
FV10 = PV*(1+R)^10 # future value after 10 years
c(FV1, FV5, FV10)

# Present Value of Money: PV = FV.n/(1+R)^n
# Annual Interest Rate: R = (FV.n/PV)^(1/n)-1
# Future Value of Money multiple compounding: FV = PV*(1+R/m)^(m*n)
m = c(1, 2, 3, 356, 1000)
FV = PV*(1+R/m)^(m)
print(cbind(m, FV))

# Future Value of Money continuously compounding: FV = PV*e^(R*n)
print(PV*exp(R), digits=6)

#Effective Annual Rate
#Simple effective rate: Ra = (1 + R/m)^(m) - 1; R is rate
#Effective rate: Ra = e^R - 1
RA = FV/PV - 1
print(cbind(m, FV, RA), digits = 6)#effective rate
print(exp(R) - 1, digits = 6) #continuous effective rate

# Net Return: R.t = (P.t-P.tm1)/P.tm1
# Gross Return: P.t/P.tm1
# revenue = initial_investment * net_return
sym <- DIS$DIS.Adjusted
ret.sym <- dailyReturn(sym)
head(ret.sym)

# Log Return: r.t = log(P.t/P.tm1) = p.t-p.tm1
log.ret.sym <- dailyReturn(sym, type='log')
head(log.ret.sym)

# Returns with Dividends: 1+R.t = (P.t+D.t)/Ptmk

