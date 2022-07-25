library(GUIDE)
GUIDE()


# Price of Bond
bondValue <- function(c, T, r, par) {
     # c = annual coupon payments
     # T = time to maturity (in years)
     # r = market interest rate
     # par = par value
 Tt <- 1:T
 bv <- 0
 for (i in Tt) { bv <- bv + c/((1+r)^i) }
 bv <- bv + par/((1+r)^T)
 YTM <- (bv/par)^(1/T)
 print(c('bond value:', round(bv, 2)))
 print(c('YTM:', round(YTM, 2)))
}

bondValue(80, 3, 0.024, 1000)




