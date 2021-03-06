library(R2jags)

## Prior predicive for logistic
ilogit <- function(x) 1/(1+exp(-x))
theta <- rnorm(1000, 1, .5)
theta <- rnorm(1000, 1, 100)
## Implied prior on p
p <- ilogit(theta)
S <- rep(NA, length(theta))
for(i in 1:length(theta)){
  S[i] <- rbinom(n=1, size=20, prob=p[i])
}
table(S)
barplot(table(S))

## Now run it in JAGS
inits <- function() list(theta=rnorm(1))
pars <- c('theta', 'p')
dat <- list(y=15, N=20, mu=1, sigma=0.5)
fit <- jags(data=dat, inits=inits,
            model='modelos/logistic.jags',
            parameters.to.save=pars, n.iter=2000)
effectiveSize(fit)
gelman.diag(as.mcmc(fit))
theta <- fit$BUGSoutput$sims.matrix[,'theta']
## prior vs posterior
hist(theta, prob=TRUE, breaks=30)
x <- seq(-1,3, len=1000)
lines(x, dnorm(x, 1, dat$sigma), lwd=2)

## Repeat with replicated experiments
dat2 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             N=rep(20, 10), R=10, mu=1, sigma=0.5)
fit2 <- jags(data=dat2,
             inits=inits,
             model='modelos/logistic2.jags',
             parameters.to.save=pars, n.iter=2000)
effectiveSize(fit2)
gelman.diag(as.mcmc(fit2))
theta2 <- fit2$BUGSoutput$sims.matrix[,'theta']


## prior vs posterior for the two examples
par(mfrow=c(2,1))
hist(theta, prob=TRUE, breaks=30, xlim=c(0,2.5))
x <- seq(-1,3, len=1000)
lines(x, dnorm(x, 1, dat2$sigma), lwd=2)
hist(theta2, prob=TRUE, xlim=c(0,2.5))
lines(x, dnorm(x, 1, dat$sigma), lwd=2)

## prior vs posterior for the two examples
p1 <- fit$BUGSoutput$sims.list$p
p2 <- fit2$BUGSoutput$sims.list$p
par(mfrow=c(2,1))
hist(p1, prob=TRUE, breaks=30, xlim=c(0,1))
hist(p2, prob=TRUE, breaks=30, xlim=c(0,1))



## Add posterior predictive checks to JAGS model
pars <- c('theta', 'p', 'ypred')
fit3 <- jags(data=dat2,
             inits=inits,
             model='modelos/logistic3.jags',
             parameters.to.save=pars, n.iter=2000)
effectiveSize(fit3)
gelman.diag(as.mcmc(fit3), multivariate = FALSE)
ypred <- fit3$BUGSoutput$sims.list$ypred

par(mfrow=c(1,1))
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Posterior Predictive Distribution')
N <- nrow(ypred)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred[,i]+rnorm(N,0,.1), pch='.')
}
points(x=1:10, y=dat2$y, col='red', cex=2, pch=16)


