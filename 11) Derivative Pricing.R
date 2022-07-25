BlackScholes <- function(S, K, r, T, sig, type){
  
  # S = Stock Price
  # K = Strike Price at Expiration 
  # r = Risk-free Interest Rate
  # T = Time to Expiration
  # sig = Volatility of the Underlying asset

  if(type=="C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  value <- round(S*pnorm(d1) - K*exp(-r*T)*pnorm(d2), 2)
  return(value)}
  
  if(type=="P"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  value <-  round((K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)), 2)
  return(value)}
}


# call option with 3 months to time to maturity
# current price of the underlying stock is USD 900
# strike price is USD 950
# the volatility is 22% and the risk-free rate is 2%
BlackScholes(900, 950, 0.02, 3/12, 0.22, 'C')


# put option with 3 months to time to maturity
# current price of the underlying stock is USD 900
# strike price is USD 950
# the volatility is 22% and the risk-free rate is 2%
BlackScholes(900, 950, 0.02, 3/12, 0.22, 'P')
