library(R2jags)
set.seed(2345)

### Compare the three versions of the model using posterior predictive and DIC
x1 <- c(9.450, 8.079, 7.686, 8.003, 2.882, 11.095, 10.696, 8.263, 12.043,
        9.238)
## Here we normalize the covariates so they are unit-scale.
x1 <- (x1-mean(x1))/sd(x1)

## Model with just x1 coviarate
pars <- c('theta', 'p', 'ypred', 'beta1')
dat4 <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             x1=x1, N=rep(20, 10), R=10, mu=1.5, sigma=0.5)
inits <- function() list(theta=rnorm(1), beta1=rnorm(1))
fit4 <- jags(data=dat4,
             inits=inits,
             model='modelos/logistic4.jags', n.thin=50,
             parameters.to.save=pars, n.iter=50000)
effectiveSize(fit4)
gelman.diag(as.mcmc(fit4), multivariate = FALSE)

## Compare posterior predictive distribution
par(mfrow=c(1,2))
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

fit3$BUGSoutput$DIC
fit4$BUGSoutput$DIC

## I choose model 4. Calculate probability p>0.8
par(mfrow=c(1,1))
surv <- fit4$BUGSoutput$sims.matrix[,'p[3]']
hist(surv)
mean(surv>0.8)
sd(surv)
quantile(surv, probs=c(.025, .975))
