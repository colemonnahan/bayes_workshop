library(R2jags)


## Prior predictive distribution for simple Poisson model

## Simulate from the prior
lambda <- abs(rnorm(500, mean=0, sd=6.4))
## Simulate data collection by drawing multiple observations for each value
## of lambda
prior_pred <- matrix(NA, length(lambda), 1000)
for(i in 1:length(lambda)){
  prior_pred[i,] <- rpois(n=1000, lambda=lambda[i])
}
get.prop <- function(x) sapply(0:25, function(i) mean(x==i))
prior_prop <- t(apply(prior_pred, 1, get.prop))

## Look at three
par(mfrow=c(1,4))
barplot(prior_prop[1,], names=0:25)
barplot(prior_prop[2,], names=0:25)
barplot(prior_prop[3,], names=0:25)

## Total combined
probs <- c(.025, .5, .975)
temp <- t(apply(prior_prop, 2, function(x) quantile(x, probs)))
plot(0:25, temp[,3], type='l', lty=3)
lines(0:25, temp[,2])
lines(0:25, temp[,1], lty=3)

## Prior predicive for logistic
ilogit <- function(x) 1/(1+exp(-x))
theta <- rnorm(1000, 0, 100)
theta <- rnorm(1000,1,.5)
## Implied prior on p
p <- ilogit(theta)
S <- matrix(NA, length(theta), 1000)
for(i in 1:length(theta)){
  S[i,] <- rbinom(n=1000, size=20, prob=p[i])
}
barplot(table(S))

## Now run it in JAGS
library(coda)
library(R2jags)
inits <- function() list(theta=rnorm(1))
pars <- c('theta', 'p')
fit <- jags(data=list(y=15, N=20), inits=inits,
            model='modelos/logistic.jags',
            parameters.to.save=pars, n.iter=2000)
effectiveSize(fit)
gelman.diag(as.mcmc(fit))
theta <- fit$BUGSoutput$sims.matrix[,'theta']

## prior vs posterior
hist(theta, prob=TRUE)
x <- seq(-1,3, len=1000)
lines(x, dnorm(x, 1, .5))

## Repeat with replicated experiments
dat2 <- list(y=c(15, 12, 11), N=c(20,20,20), R=3)
dat3 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             N=rep(20, 10), R=10)
fit2 <- jags(data=dat3,
             inits=inits,
             model='modelos/logistic2.jags',
             parameters.to.save=pars, n.iter=2000)
effectiveSize(fit2)
gelman.diag(as.mcmc(fit2))
theta <- fit2$BUGSoutput$sims.matrix[,'theta']
## prior vs posterior
par(mfrow=c(1,2))
hist(theta, prob=TRUE)
x <- seq(-1,3, len=1000)
lines(x, dnorm(x, 1, .5))
hist(ilogit(theta), xlim=c(0,1))


## Add posterior predictive checks to JAGS model
pars <- c('theta', 'p', 'ypred')
fit3 <- jags(data=dat3,
             inits=inits,
             model='modelos/logistic3.jags',
             parameters.to.save=pars, n.iter=2000)
effectiveSize(fit2)
gelman.diag(as.mcmc(fit2))
theta <- fit2$BUGSoutput$sims.matrix[,'theta']
## prior vs posterior
par(mfrow=c(1,2))
hist(theta, prob=TRUE)
x <- seq(-1,3, len=1000)
lines(x, dnorm(x, 1, .5))
hist(ilogit(theta), xlim=c(0,1))
ypred <- fit3$BUGSoutput$sims.list$ypred

plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Posterior Predictive Distribution')
N <- nrow(ypred)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat3$y[i], col='red', pch=16)
}

