## Code to run the JAGS model from Meyer and Millar 1999

library(R2jags)
library(shinystan)

dat <- read.csv('tuna.csv')
cpue <- dat$CPUE
catches <- dat$Catches
data <- list(catches=dat$Catches, logcpue=log(dat$CPUE), N=nrow(dat))
## Random initial values
inits.fn <- function()
  list(logr=log(runif(1, .01, 1)),
       logK=log(runif(1, 150, 500)),
       iq=runif(1, 2,10),
       isigma2=runif(1, 1, 20),
       itau2=runif(1, 1, 20),
       u=rnorm(n=nrow(dat),0,2))
params.jags <- c('logr', 'logK', 'isigma2', 'iq', 'itau2', 'u')

set.seed(23524)
inits <- lapply(1:3, function(i) inits.fn())
fit <- jags(data=data, parameters.to.save=params.jags, inits=inits,
     model.file='logistic.jags', n.chains=3,
     n.iter=200001, n.burnin=500, n.thin=100)
fit.mcmc <- as.mcmc(fit)

## Plot traces
par(mfrow=c(5,6), mar=.1*c(1,1,1,1))
coda::traceplot(fit.mcmc)

## Look at parameter correlation
post <- data.frame(do.call(rbind, fit.mcmc))
plot(exp(post$logr), exp(post$logK), xlab='r', ylab='K')

## Check convergence numerically
sort(effectiveSize(fit))                # effective samples
gelman.diag(fit.mcmc)                   # Rhat

## Fancy tool to explore output interactively
launch_shinystan(as.shinystan(fit.mcmc))

## Can get samples directly if we want
posterior <- fit$BUGSoutput$sims.matrix
hist(exp(posterior[,'logr']))
mean(exp(posterior[,'logr'])>.2) # approximate probability!
quantile(exp(posterior[,'logr']),probs=c(0.025, .975))
