
library(R2jags)
## Add posterior predictive checks to JAGS model
pars <- c('theta', 'ypred', 'beta1', 'beta2')
x1 <- c(9.450, 8.079, 7.686, 8.003, 2.882, 11.095, 10.696, 8.263, 12.043,
        9.238)
x2 <- c(0.08, 0.252, 0.158, 0.25, 0.081, 0.037, 0.002, 0.042, 0.053,
        -0.02, 0.141, -0.001, -0.078, 0.103, -0.076, 0.037, 0.071, 0.113,
        0.23, 0.246)
## x1 <- (x1-mean(x1))/sd(x1)
## x2 <- (x2-mean(x2))/sd(x2)
dat4 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             x1=x1, x2=x2, N=rep(20, 10), R=10)
inits <- function() list(theta=rnorm(1), beta1=rnorm(1), beta2=rnorm(1))
fit4 <- jags(data=dat,
             inits=inits,
             model='modelos/logistic4.jags', n.thin=50,
             parameters.to.save=pars, n.iter=100000)
effectiveSize(fit4)
gelman.diag(as.mcmc(fit4))
ypred <- fit4$BUGSoutput$sims.list$ypred

## Check the posterior predictive again
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Posterior Predictive Distribution')
N <- nrow(ypred)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat4$y[i], col='red', pch=16)
}



fit3$BUGSoutput$DIC
fit4$BUGSoutput$DIC
