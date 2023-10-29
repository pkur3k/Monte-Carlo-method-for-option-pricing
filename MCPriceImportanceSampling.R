MCPriceImportanceSampling<-function(Np, Nt, T, r, sigma, s0, K) {
  
  dt = T/Nt
  Z <-  matrix(rnorm(Np * Nt, mean=0, sd=1),nrow = Nt, ncol = Np) 
  dB <- Z*sqrt(dt) 
  B <- matrix(numeric(Np*(Nt+1)), nrow = (Nt+1), ncol = Np)
  S <- matrix(numeric(Np*(Nt+1)), nrow = (Nt+1), ncol = Np)
  
  for(i in 1:Np){
    B[,i] <- c(0, cumsum(dB[,i]))
    S[,i] <- s0*exp((r - 0.5*sigma^2)*T + sigma*B[,i]) 
  }
  
  ST <- S[nrow(S),]
  
  # call option price 
  call_payoffs <- exp(-r*T)*pmax(ST-K,0)[ST>K]
  call_price <- mean(call_payoffs*mean(ST>K))
  
  # put option price 
  put_payoffs <- exp(-r*T)*pmax(K-ST,0)[ST<K]
  put_price <- mean(put_payoffs*mean(ST<K))
  
  # standard deviation 
  err_call <- sd(call_payoffs*mean(ST>K))/sqrt(Np)
  err_put <- sd(put_payoffs*mean(ST<K))/sqrt(Np)
  
  output<-list(call_price=call_price, err_call=err_call, 
               put_price=put_price, err_put=err_put)
  
  return(output)
}

sigma <- 0.3
mu <- 0.05
Nt <- 252
Np <- 1e4
s0 <- 100
K <- 100
r <- 0.1
T <- 1

results<-MCPriceImportanceSampling(Np, Nt, T, r, sigma, s0, K)
results

# for results = S
matplot (results[,1:10], type="l", lty=1, col=c("green", "blue", "red"), 
         main="Monte Carlo price paths with importance sampling", 
         xlab="Time", ylab="Price",ylim = c(100,250))


