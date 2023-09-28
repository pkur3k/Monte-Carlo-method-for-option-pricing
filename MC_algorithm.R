sigma <- 0.3
mu <- 0.1
dt <- 1/252
Nt <- 252
Np <- 1e4
z <- matrix(rnorm(Nt*Np), nrow=Nt)
r <- mu*dt + z*sigma*sqrt(dt)
r <- matrix(rnorm(Nt*Np, mean=mu*dt, sd=sigma*sqrt(dt)), nrow=Nt)
s <- matrix(0, Nt+1, Np)


for(t in 1:Nt) {
  s[t+1, ] <- s[t, ]+r[t, ]
}

P <- exp(s)
matplot (s[, 1:3], type="l", lty=1, col=c("green", "blue", "red"), 
         main="Monte Carlo price paths", 
         xlab="Time Step", ylab="Price") 


