sigma <- 0.3
mu <- 0.05
Nt <- 252
Np <- 1e4
s0 <- 100
K <- 100
rf <- 0.1
T <- 1

ps <- MCPrice(s0, K, rf, T, sigma, Nt, Np)

matplot (ps[1:2], type="l", lty=1, col=c("green", "blue", "red"), 
         main="Monte Carlo price paths", 
         xlab="Time Step", ylab="Price") 

MCPrice <- function(s0, K, rf, T, sigma, Nt, Np){
  
  dt <- T/Nt
  #r <- matrix(rnorm(Nt*Np, mean=mu*dt, sd=sigma*sqrt(dt)), nrow=Nt)
  z <- matrix(rnorm(Nt*Np), nrow=Nt)
  r <- (mu-0.5*sigma**2)*T + z*sigma*sqrt(dt)
  s <- matrix(0, Nt+1, Np)

  for(t in 1:Nt) {
    s[t+1, ] <- s[t, ] + r[t, ]
  }
  
  
  P <- s0*exp(s)    
  return(P)
}
  
 