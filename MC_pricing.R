sigma <- 0.3
mu <- 0.05
Nt <- 252
Np <- 1e4
s0 <- 100
K <- 100
r <- 0.1
T <- 1

MCPrice<-function(Np, T, r, sigma, s0, K) {
  
  z <- rnorm(Np, mean=0, sd=1)
  S <- s0*exp((r - 0.5*sigma^2)*T + sigma*sqrt(T) * z)
  
  # call option price 
  call_payoffs <- exp(-r*T)*pmax(S-K,0)
  print(call_payoffs)
  call_price <- mean(call_payoffs)
  
  # put option price and 
  put_payoffs <- exp(-r*T)*pmax(K-S,0)
  put_price <- mean(put_payoffs)
  
  # standard deviation 
  err_call <- sd(call_payoffs)/sqrt(Np)
  err_put <- sd(put_payoffs)/sqrt(Np)
  
  output<-list(call_price=call_price, err_call=err_call, 
               put_price=put_price, err_call=err_call)
  
  return(output)
  
}

results<-MCPrice(Np, T, r, sigma, s0, K)
results
  
 