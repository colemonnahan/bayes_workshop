## Homework 2: Fitting models in JAGS

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

## The normal-normal example from lecture 2 (conjugacy)
dat <- list(y=.5)
inits <- function() list(theta=runif(1,-5,5))
fit2 <- jags(data=dat, inits=inits, parameters.to.save='theta',
             model.file='modelos/normal.jags', n.chains=3, n.iter=2000,
             n.burnin=500, n.thin=1)
traceplot(fit2)
geweke.diag(fit2)
effectiveSize(fit2)
gelman.diag(as.mcmc(fit2))
post <- data.frame(fit2$BUGSoutput$sims.matrix)
hist(post$theta)

