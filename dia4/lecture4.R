library(R2jags)
library(shinystan)

## Simulate a hierarchical Poisson
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


## Look at prior predictive
N <- 10000
ptau <- abs(rnorm(n=N, mean=0, sd=1))
pmu <- rnorm(n=N, mean=1, sd=1)
plambdas <- sapply(1:N, function(i) exp(rnorm(1, pmu[i], ptau[i])))
py <- rpois(N, plambdas)
par(mfrow=c(1,4))
hist(ptau)
hist(pmu)
hist(log(plambdas))
hist(log(py))
abline(v=log(250))
## How many above 250?
mean(py>250)

## Now fit it in JAGS
dat$y <- dat$y[-(2:nreps)]
dat$site <- dat$site[-(2:nreps)]
dat$ndata <- length(dat$y)
inits <- function() list(tau=abs(rnorm(1)), mu=rnorm(1),
                         loglambda=rnorm(nsites))
pars <- c('tau', 'mu', 'lambda', 'loglambda', 'ypred')
fit <- jags(dat, inits, parameters.to.save=pars,
            model='modelos/hierarchical.jags', n.iter=20000, n.thin=10)
mon <- rstan::monitor(fit$BUGSoutput$sims.array)
max(mon[,'Rhat'])
min(mon[, 'n_eff'])
## launch_shinystan(as.shinystan(as.mcmc(fit)))


par(mfrow=c(1,3))
lambdas.median <- apply(fit$BUGSoutput$sims.list$lambda,2, median)
ypred <- fit$BUGSoutput$sims.list$ypred
boxplot(y~site, xlim=c(1,nsites+1), data=dat, xlab='Site', ylab='Count')
points(1:nsites-.25, lambdas.median, pch=16, col='red')
points(1:nsites+.25, lambdas, pch=16, col='blue')
segments(x0=16, y0=quantile(ypred, probs=.025), y1=quantile(ypred, probs=.975), lwd=1.5)
segments(x0=16, y0=quantile(ypred, probs=.25), y1=quantile(ypred, probs=.75), lwd=2)
points(16, median(ypred), pch=15)
hist(fit$BUGSoutput$sims.list$tau, prob=TRUE, breaks=50)
x <- seq(0,5, len=1000)
lines(x, dnorm(x, mean=0, sd=1), lwd=2)
hist(fit$BUGSoutput$sims.list$mu, prob=TRUE, breaks=50)
x <- seq(0,5, len=1000)
lines(x, dnorm(x, mean=1, sd=1), lwd=2)
