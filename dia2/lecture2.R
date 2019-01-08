
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

### Monte Carlo integration. Approximates integrals with percentages of
### samples
## First with only 10 samples
x <- rnorm(10, mean=mu1, sd=tau1)
mean(x<0)
quantile(x, probs=c(0.025, 0.975))
## It improves with more samples:
x <- rnorm(1e6, mean=mu1, sd=tau1)
mean(x<0)
quantile(x, probs=c(0.025, 0.975))

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

## Demonstration of what happens to posterior with increasing data for the
## beta-binomial example.
par(mfrow=c(1,3), mar=c(2.5,1,2,.1))
plot.beta.binomial(2,2, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=4')
plot.beta.binomial(10,10, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=20')
plot.beta.binomial(5000,2000, alpha=3.5, beta=1.5, Intervals=FALSE)
mtext('n=100')
