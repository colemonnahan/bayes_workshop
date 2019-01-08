library(rjags)
library(coda)
library(R2jags)
## Simulate and estimate a simple Poisson count process

lambda.true <- 15
n <- 10
x <- rpois(n, lambda.true)

inits <- list(lambda=4)
pars <- 'lambda'
out <- jags(data=list(x=x, N=length(x)), model='../modelos/poisson.jags',
     parameters.to.save=pars)
plot(out)
traceplot(out)
sims <- out$BUGSoutput$sims.matrix
mean(sims[,1])
mean(sims[,2])
print(out)
out$
