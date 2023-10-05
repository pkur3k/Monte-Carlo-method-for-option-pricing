MCPrice<-function(Np, Nt, T, r, sigma, s0, K) {
  
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
  call_payoffs <- exp(-r*T)*pmax(ST-K,0)
  call_price <- mean(call_payoffs)
  
  # put option price 
  put_payoffs <- exp(-r*T)*pmax(K-ST,0)
  put_price <- mean(put_payoffs)
  
  # standard deviation 
  err_call <- sd(call_payoffs)/sqrt(Np)
  err_put <- sd(put_payoffs)/sqrt(Np)
  
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

results<-MCPrice(Np, Nt, T, r, sigma, s0, K)
results

# compare with true value
library(RQuantLib)
EuropeanOption("call", s0, K, 0, r, T, sigma)$value
EuropeanOption("put", s0, K, 0, r, T, sigma)$value

# for results = S
matplot (results[,1:3], type="l", lty=1, col=c("green", "blue", "red"), 
         main="Monte Carlo price paths", 
         xlab="Time", ylab="Price")

# for results = S
hist(results[Nt+1,], main="Terminal values", 
     xlab="Values", ylab="Frequency")

# for results = call_payoffs
hist(results, main="Terminal call values", 
     xlab="Values", ylab="Frequency")


 