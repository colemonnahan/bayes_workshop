### Metropolis MCMC; Niter is the number of samples; f is the un-normalized
### pdf function; x0 is the initial value; U controls how big of values
### are proposed from a uniform distribution.
mcmc <- function(Niter, f, x0=0, U=1){
  x <- rep(NA,Niter)
  x[1] <- x0
  for(i in 2:Niter){
    new <- x[i-1]+runif(1,-U,U)
    if( f(new)/f(x[i-1]) > runif(1)){
      ## Update or "accept" this new point
      x[i] <- new
    } else {
      ## Stay at previous point
      x[i] <- x[i-1]
    }
  }
  return(x)
}

### Often need a "burn in" or "warmup" period for the chain to stabilize
f <- function(x) dnorm(x,0,1)
set.seed(235)
U <- 1
Niter <- 1000
x1 <- mcmc(Niter, f, U=U, x0=30)
x2 <- mcmc(Niter, f, U=U, x0=0)
x3 <- mcmc(Niter, f, U=U, x0= -30)
par(mfrow=c(1,1))
plot(x1, type='l', col=1, ylim=c(-30,30))
lines(x2, col=2)
lines(x3,, col=3)

## If we include the first part of the chain we get a very wrong answer.
par(mfrow=c(1,2), mar=c(2,2,1,.5))
hist(c(x1,x2,x3), freq=FALSE, main='With Warmup', ylim=c(0,.4), breaks=30)
curve(dnorm, -3,3, add=TRUE)
## This period is the 'burn-in' or 'warmup' period and during it the
## samples are **not** from the distribution f. So we discard the first
## chunk.
warmup <- 1:200
x.sampling <- c(x1[-warmup], x2[-warmup], x3[-warmup])
hist (x.sampling, freq=FALSE, main='Without Warmup', ylim=c(0, .4), breaks=20)
curve(dnorm, -3,3, add=TRUE)

### Markov chains need to be tuned
set.seed(235)
Niter <- 5000
## Three chains with different proposals (tuning parameters)
x1 <- mcmc(Niter, f, U=2)
x2 <- mcmc(Niter, f, U=50)
x3 <- mcmc(Niter, f, U=.1)
par(mfcol=c(3,2), mar=c(2,2,1,.5))
plot(x1, type='l', col=1, ylim=c(-3,3))
plot(x2, type='l', col=2, ylim=c(-3,3))
plot(x3, type='l', col=3, ylim=c(-3,3))
library(coda)
## Visual autocorrelation
acf(x1)
acf(x2)
acf(x3)
### Why do we care? Because we want as many samples as
### possible. Autocorrelation lowers the "effective samples"
effectiveSize(x1)
effectiveSize(x2)
effectiveSize(x3)

## If a chain mixes very slowly, we have to run it for a long time to get
## enough "effective" samples. To make it easier we often thin, or save,
## every i^th sample.
set.seed(232)
x1 <- mcmc(Niter, f, U=2)
x2 <- mcmc(10*Niter, f, U=50)
x3 <- mcmc(100*Niter, f, U=.1)
## Save every 10th
x2 <- x2[seq(1, length(x2), by=10)]
## save every 100th
x3 <- x3[seq(1, length(x3), by=100)]
par(mfcol=c(3,1))
plot(x1, type='l', col=1, ylim=c(-3,3))
plot(x2, type='l', col=2, ylim=c(-3,3))
plot(x3, type='l', col=3, ylim=c(-3,3))
effectiveSize(x1)
effectiveSize(x2)
effectiveSize(x3)



library(R2jags)
dat <- dget('datos/insect_data2.txt')
plot(dat$x, dat$Y, xlab = "Female Egg Compliment",
     ylab = "Eggs laid on host", pch = 15)
inits <- function() list(lambda=runif(1,1,15))
fit1 <- jags(data=dat, inits=inits, parameters.to.save='lambda',
             model.file='modelos/insect1.jags', n.chains=3, n.iter=2000,
             n.burnin=500, n.thin=1)
traceplot(fit1)
geweke.diag(fit1)
effectiveSize(fit1)
gelman.diag(as.mcmc(fit1))
post <- data.frame(fit1$BUGSoutput$sims.matrix)
hist(post$lambda)
