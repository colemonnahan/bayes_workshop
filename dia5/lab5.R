### Lab 5: Demonstration of Stan software for Bayesian analysis

library(shinystan)
library(rstan)
## chains <- parallel::detectCores()-1
## options(mc.cores = chains)
rstan_options(auto_write = TRUE)


## Rerun the logistic hierarchical model in Stan
dat <- list(y=c(15, 12, 11, 12, 4, 15, 17, 12, 16, 14),
             N=rep(20, 10), R=10)
inits <- function()
  list(theta=rnorm(10, 0, 5), mu=rnorm(1), sigma=runif(1,0,2))
fit <- stan(file='modelos/logistic_hierarchical.stan',
            data=dat, init=inits, control=list(adapt_delta=.99))
launch_shinystan(fit)


## Here's a more complicated Stan exapmple
dat <- readRDS('datos/growth_data.RDS')
inits <- function() list(
        logLinf_mean=runif(1, 2,4),
        logLinf_sigma=runif(1,0,1),
        logk_mean=runif(1, -4,-2),
        logk_sigma=runif(1,0,1),
        sigma_obs=runif(1,0,1),
        delta=runif(1,.5,1.5),
        logk=runif(dat$Nfish, -4,0),
        logLinf=runif(dat$Nfish, 3,5))
inits.nc <- function() list(
        logLinf_mean=runif(1, 2,4),
        logLinf_sigma=runif(1,0,1),
        logk_mean=runif(1, -4,-2),
        logk_sigma=runif(1,0,1),
        sigma_obs=runif(1,0,1),
        delta=runif(1,.5,1.5),
        logk_raw=runif(dat$Nfish, -4,4),
        logLinf_raw=runif(dat$Nfish, 4,4))

## The "centered" version of the model. This one has many problems
## that show up as divergences.
growth.stan <- stan('modelos/growth.stan', data=dat, seed=1,
                    init=inits, open_progress=FALSE,
                    control=list(adapt_delta=.9))
launch_shinystan(growth.stan)

## Repeat after non-centering. Now the divergences are gone and it runs 
## much faster and mixes much better.
growth.nc.stan <- stan('modelos/growth_nc.stan', data=dat, seed=1,
                       init=inits.nc, open_progress=FALSE,
                       control=list(adapt_delta=.9))
launch_shinystan(growth.nc.stan)
