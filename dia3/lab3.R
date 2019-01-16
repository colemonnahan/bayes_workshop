library(R2jags)
set.seed(2345)

### Compare the three versions of the model using posterior predictive and DIC
x1 <- c(9.450, 8.079, 7.686, 8.003, 2.882, 11.095, 10.696, 8.263, 12.043,
        9.238)
x2 <- c(0.08, 0.252, 0.158, 0.25, 0.081, 0.037, 0.002, 0.042, 0.053,
        -0.02, 0.141, -0.001, -0.078, 0.103, -0.076, 0.037, 0.071, 0.113,
        0.23, 0.246)
## Here we normalize the covariates so they are unit-scale.
x1 <- (x1-mean(x1))/sd(x1)
x2 <- (x2-mean(x2))/sd(x2)


## Model with just x1 coviarate
pars <- c('theta', 'p', 'ypred', 'beta1')
dat4 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             x1=x1, N=rep(20, 10), R=10, mu=1, sigma=0.5)
inits <- function() list(theta=rnorm(1), beta1=rnorm(1))
fit4 <- jags(data=dat4,
             inits=inits,
             model='modelos/logistic4.jags', n.thin=50,
             parameters.to.save=pars, n.iter=50000)
effectiveSize(fit4)
gelman.diag(as.mcmc(fit4), multivariate = FALSE)

## Model with both x1 and x2
pars <- c('theta', 'p', 'ypred', 'beta1', 'beta2')
dat5 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             x1=x1, x2=x2, N=rep(20, 10), R=10, mu=1, sigma=0.5)
inits <- function() list(theta=rnorm(1), beta1=rnorm(1), beta2=rnorm(1))
fit5 <- jags(data=dat5,
             inits=inits,
             model='modelos/logistic5.jags', n.thin=50,
             parameters.to.save=pars, n.iter=50000)
effectiveSize(fit5)
gelman.diag(as.mcmc(fit5), multivariate = FALSE)

## Compare posterior predictive distribution
par(mfrow=c(1,3))
ypred3 <- fit3$BUGSoutput$sims.list$ypred
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Fit 3: Posterior Predictive Distribution')
N <- nrow(ypred3)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred3[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat4$y[i], col='red', pch=16)
}
ypred4 <- fit4$BUGSoutput$sims.list$ypred
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Fit 4: Posterior Predictive Distribution')
N <- nrow(ypred4)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred4[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat4$y[i], col='red', pch=16)
}
ypred5 <- fit5$BUGSoutput$sims.list$ypred
plot(0,0, type='n', xlim=c(0,11), ylim=c(0,20), xlab='Replicate',
     ylab='Fit 5: Posterior Predictive Distribution')
N <- nrow(ypred5)
for(i in 1:10){
  points(x=rep(i, N)+ rnorm(N,0,.1), y=ypred5[,i]+rnorm(N,0,.1), pch='.')
  points(x=i, y=dat5$y[i], col='red', pch=16)
}

fit3$BUGSoutput$DIC
fit4$BUGSoutput$DIC
fit5$BUGSoutput$DIC

## I choose model 4. Calculate probability p>0.8
surv <- fit4$BUGSoutput$sims.matrix[,'p[1]']
hist(surv)
mean(surv>0.75)
