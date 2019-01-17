library(R2jags)

## Exercise: Simulate a hierarchical Poisson
set.seed(13425)
nreps <- 15
nsites <- 15
## index for site number
site <- rep(1:nsites, each=nreps)
tau <- .5 # hyper sd in log space
mu <- 3        # hyper mean in log space
## Simulate random effects
loglambdas <- rnorm(nsites, mu, tau)
lambdas <- exp(loglambdas)
## Simulate data collection
y <- rpois(n=nreps*nsites, lambdas[site])
dat <- list(nsites=nsites, ndata=length(y),
            site=site, y=y)
boxplot(y~site, data=dat, xlab='Site', ylab='Count')


## Explore mode of a hierarchical model
hyper <- function(lambda, mu, tau) {
  exp(sum(dnorm(lambda, mu, tau), log=TRUE))
}
lambda <- rep(3,10)
hyper(lambda, mu=3, tau=1)
hyper(lambda, mu=3, tau=.01)
hyper(lambda, mu=3, tau=.0001)


## Exercise: Look at prior predictive
N <- 10000
tau <- abs(rnorm(n=N, mean=0, sd=1))
mu <- rnorm(n=N, mean=1, sd=1)
lambdas <- sapply(1:N, function(i) exp(rnorm(1, mu[i], tau[i])))
y <- rpois(N, lambdas)
par(mfrow=c(1,4))
hist(tau)
hist(mu)
hist(log(lambdas))
hist(log(y))
abline(v=log(250))
## How many above 250?
mean(y>250)

## Now fit it in JAGS. dat is defined above with the simulated
## data
str(dat)
inits <- function() list(tau=abs(rnorm(1)), mu=rnorm(1),
                         loglambda=rnorm(dat$nsites,1,1))
pars <- c('tau', 'mu', 'lambda', 'loglambda', 'ypred')
fit <- jags(dat, inits, parameters.to.save=pars,
            model='modelos/hierarchical.jags', 
            n.iter=50000, n.thin=10)
## This tool is better for checking convergence
library(shinystan)
library(rstan)
mon <- rstan::monitor(fit$BUGSoutput$sims.array, print=FALSE)
max(mon[,'Rhat'])
min(mon[, 'n_eff'])
## launch_shinystan(as.shinystan(as.mcmc(fit)))

par(mfrow=c(1,3))
lambdas.median <- apply(fit$BUGSoutput$sims.list$lambda,2, median)
ypred <- fit$BUGSoutput$sims.list$ypred
boxplot(y~site, xlim=c(1,nsites+1), data=dat, xlab='Site', ylab='Count')
points(1:nsites-.25, lambdas.median, pch=16, col='red')
segments(x0=16, y0=quantile(ypred, probs=.025), y1=quantile(ypred, probs=.975), lwd=1.5)
segments(x0=16, y0=quantile(ypred, probs=.25), y1=quantile(ypred, probs=.75), lwd=2)
points(16, median(ypred), pch=15)
hist(fit$BUGSoutput$sims.list$tau, prob=TRUE, breaks=50)
x <- seq(0,5, len=1000)
lines(x, dnorm(x, mean=0, sd=1), lwd=2)
hist(fit$BUGSoutput$sims.list$mu, prob=TRUE, breaks=50)
x <- seq(0,5, len=1000)
lines(x, dnorm(x, mean=1, sd=1), lwd=2)
