

## Exercise 1
lam <- seq(1,10, len=1000)
like1 <- dpois(x=5, lambda=lam)
par(mfrow=c(1,3))
plot(lam, like1, type='l')
like2 <- sapply(lam, function(i) prod(dpois(x=c(5,4,7), lambda=i)))
plot(lam, like2, type='l')
plot(lam, log(like2), type='l')

## normal-normal posterior
y <- .5 # observed
sigma <- 1 # assume known
theta <- seq(-3,3, len=10000) # parameter
mu0 <- -2 # prior mean
tau0 <- .5 # prior SD
prior <- dnorm(theta, mean=mu0, sd=tau0)
like <- dnorm(y, mean=theta, sd=sigma)
## Posterior calculations (mean and SD)
mu1 <- (mu0/tau0^2 + y/sigma^2) / (1/tau0^2 + 1/sigma^2)
tau1 <- sqrt(1 / (1/tau0^2 + 1/sigma^2))
posterior <- dnorm(theta, mean=mu1, tau1)
## Make plot of all 3
ymax <- max(c(prior, like, posterior))
plot(0,0, type='n', xlim=range(theta), ylim=c(0, 1.1*ymax))
lines(theta, prior, lty=3)
lines(theta, like, lty=2)
lines(theta, posterior)

## Area to left of 0 [ P(theta<0) ]
pnorm(q=0, mean=mu1, sd=tau1)
plot(theta, posterior, type='l', ylab='Density', xlim=c(-2.5,1))
## 95% confidence interval
CI <- qnorm(p = c(0.025, 0.975), mean=mu1, sd=tau1)
abline(v=CI)

library(stats)
## This function plots the beta prior, binomial likelihood, and posterior
## beta-binomial for given inputs. Intervals can be added.
plot.beta.binomial <- function(deaths=5, survivors=4, alpha=1.5,
                               beta=1.5, Bayesian=TRUE, Intervals=FALSE){
  ## Calculate the distributions
  alpha <- max(alpha, .001)
  beta <- max(beta, .001)
  num.trials <- deaths+survivors
  p.seq <- seq(0.00001,.99999, len=500)
  likelihood <- dbinom(x=survivors, size=num.trials, prob=p.seq)
  prior <- dbeta(x=p.seq, alpha, beta)
  param1 <- alpha+survivors
  param2 <-  num.trials-survivors+beta
  posterior <- dbeta(x=p.seq, param1,param2)
  ## Scale them for plotting
  scale <- .9
  likelihood <- likelihood*scale/max(likelihood)
  density.max <- max(c(prior, posterior))
  prior <- prior*scale/density.max
  posterior <- posterior*scale/density.max
  p.hat <- survivors/num.trials        # the MLE
  if(Intervals){
    CI.x <- qbeta(p=c(.025, .975), param1, param2)
    CI.y <- dbeta(CI.x, param1, param2)*scale/density.max
    p.hat.se <-sqrt( p.hat*(1-p.hat)/num.trials)
    p.CI <- c(-1.96*p.hat.se + p.hat, 1.96*p.hat.se+p.hat)
  }
  ## Plot them
  plot(p.seq, likelihood, type="l", lwd=2, col=1, ylab=NA,
       axes=FALSE,xlab="Probability of survival (p)",
       xlim=c(0,1), ylim=c(0,1))
  ## mtext(side=3, text="Beta-Binomial", line=1, cex=2.5)
  ## mtext(side=2, text="Scaled Likelihood/Density", line=1)
  axis(1); box()
  points(p.hat, scale, cex=1.25, pch=16)
  if(Bayesian){
    lines(p.seq, prior, lwd=2, col=2)
    lines(p.seq, posterior, lwd=2, col=4)
  }
  ## par(xpd=TRUE)
  # legend(x=0, y=1.05,
  #        legend=c("Likelihood", "Conf. Interval",
  #                 "Prior", "Posterior", "Cred. Interval"),
  #        col=c(1,1,2,4,4), ncol=1, lty=c(1,2,1,1,2),
  #        bty="n", lwd=2)
  ## Add confidence intervals and posterior credible intervals
  if(Intervals){
    if(Bayesian){
      points(CI.x, c(0,0), pch=16, col=4)
      segments(x0=c(CI.x[1], CI.x[2],CI.x[1]),
               x1=c(CI.x[1], CI.x[2],CI.x[2]),
               y0=c(0,0,0), y1=c(CI.y[1], CI.y[2],0),
               lwd=2, lty=2, col=4)
    }
    points(x=p.CI, y=c(scale,scale), pch=16)
    lines(p.CI, y=c(scale,scale), lwd=2, lty=2)
  }
}


library(manipulate)
dev.off()
manipulate(plot.beta.binomial(deaths, survivors, alpha, beta), 
           deaths=slider(1,10, 5),
           survivors=slider(1,10,5),
           alpha=slider(0,5, 3.5),
           beta=slider(0,5, 1.5)
           )

## Demonstration of what happens to posterior with increasing data for the
## beta-binomial example.
par(mfrow=c(1,3), mar=c(2.5,1,2,.1))
plot.beta.binomial(2,2, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=4')
plot.beta.binomial(10,10, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=20')
plot.beta.binomial(5000,2000, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=100')



### A very brief demonstration of Markov chains.
## A simple Markov chain
set.seed(5324)
Niter <- 2000
mc <- function(Niter){
  x <- rep(NA,Niter)
  x[1] <- 0
  for(i in 2:Niter){
    x[i] <- x[i-1]+runif(1,-.5,.5)
  }
  return(x)
}
x1 <- mc(Niter)
plot(x1, type='l', ylim=c(-20,20))

## Run two more
set.seed(24123)
x2 <- mc(Niter)
x3 <- mc(Niter)
lines(x2, col=2)
lines(x3, col=3)
## These "chains" wander randomly.


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

f <- function(x) dnorm(x, 0, 1) # returns probablity density
f(0)/f(.5)
f(1)/f(.5)
mean(f(1)/f(.5) > runif(1e6))
f(10)/f(.5)


Niter <- 2000
set.seed(23523)
plot(0, type='n', xlim=c(1,Niter), ylim=c(-4,4))
x1 <- mcmc(Niter, f=f)
x2 <- mcmc(Niter, f=f)
x3 <- mcmc(Niter, f=f)
lines(x1, col=1)
lines(x2, col=2)
lines(x3, col=3)

## These chains have a "limiting distribution"... what is it?
hist(c(x1,x2,x3), freq=FALSE, breaks=30)
lines(xseq <- seq(-4,4, len=1000), dnorm(xseq, 0,1), lwd=2, col=2)
## It is the density f! (Standard normal in this case)

## From the example last week with conjugacy. Let's assume we can't
## calculate the constant
y <- .5 # observed data point
sigma <- 1 # assumed known variance
mu0 <- -1 # prior mean
tau0 <- .5 # prior SD

## We can calculate the posterior density without the constant like this
f2 <- function(theta){
  likelihood <- exp(-(y-theta)^2/(2*sigma^2))
  prior <- exp(-(theta-mu0)^2/(2*tau0^2))
  post <- likelihood * prior
  return(post)
}

Niter <- 2000
set.seed(98347)
plot(0, type='n', xlim=c(1,Niter), ylim=c(-3,2))
x1 <- mcmc(Niter, f=f2)
x2 <- mcmc(Niter, f=f2)
x3 <- mcmc(Niter, f=f2)
lines(x1, col=1)
lines(x2, col=2)
lines(x3, col=3)

## Did it give us the distribution we calculated analytically before?
hist(c(x1,x2,x3), freq=FALSE, breaks=30)
mu1 <- (mu0/tau0^2 + y/sigma^2) / (1/tau0^2 + 1/sigma^2)
tau1 <- sqrt(1 / (1/tau0^2 + 1/sigma^2))
lines(xseq <- seq(-4,4, len=1000),
      dnorm(xseq, mu1, tau1), lwd=2)
### So the Markov chain give us samples from a posterior even if we dont
### know the constant. We just need to calculate the likelihood and
### prior. We can always do this.

## Exercise: normal-normal posterior with MCMC
y <- .5 # observed
sigma <- 1 # assume known
mu0 <- -2 # prior mean
tau0 <- .5 # prior SD
prior <- function(theta) dnorm(theta, mean=mu0, sd=tau0)
like <- function(theta) dnorm(y, mean=theta, sd=sigma)
## Does not include the constant
posterior <- function(theta) like(theta)*prior(theta)
samples <- mcmc(Niter=5000, f=posterior, x0=0, U=1)
hist(samples, probability=TRUE)

## Analytical Posterior calculations (mean and SD)
theta <- seq(-3,0, len=1000) # parameter
mu1 <- (mu0/tau0^2 + y/sigma^2) / (1/tau0^2 + 1/sigma^2)
tau1 <- sqrt(1 / (1/tau0^2 + 1/sigma^2))
posterior2 <- dnorm(theta, mean=mu1, tau1)
lines(theta, posterior2, lwd=2)
## Compare inference
quantile(samples, probs=c(.025, .975))
qnorm(p=c(0.025, .975), mu1, tau1)


