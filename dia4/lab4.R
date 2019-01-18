library(R2jags)

## Hierarchical version of the logistic model
dat <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             N=rep(20, 10), R=10)
inits <- function()
  list(theta=rnorm(10, 0, 5), mu=rnorm(1), sigma=runif(1,0,2))
pars <- c('theta', 'mu', 'sigma', 'ypred')
fit <- jags(data=dat,
            inits=inits, parameters.to.save=pars,
            model='modelos/logistic_hierarchical.jags',
            n.iter=500000, n.thin=500)
library(shinystan)
library(rstan)
mon <- rstan::monitor(fit$BUGSoutput$sims.array, print=FALSE)
max(mon[,'Rhat'])
min(mon[, 'n_eff'])
## launch_shinystan(as.shinystan(as.mcmc(fit)))
ypred <- fit$BUGSoutput$sims.list$ypred
post <- fit$BUGSoutput$sims.matrix

## Posterior predictive
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Fit 3: Posterior Predictive Distribution')
N <- nrow(ypred)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat$y[i], col='red', pch=16)
}

## Priors vs posterior
par(mfrow=c(1,3))
hist(post[,'mu'], breaks=30, prob=TRUE)
x <- seq(-1,2, len=1000)
lines(x, dnorm(x, mean=1, sd=0.5), lwd=2)
hist(post[,'sigma'], breaks=30, prob=TRUE)
x <- seq(-1,2, len=1000)
lines(x, dnorm(x, mean=0, sd=1), lwd=2)
## Posterior for theta's
theta <- fit$BUGSoutput$sims.list$theta
boxplot(theta, xlab='site', ylab='theta')
