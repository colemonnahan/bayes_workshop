library(shinystan)
library(rstan)
chains <- parallel::detectCores()-1
options(mc.cores = chains)
rstan_options(auto_write = TRUE)

## Load empirical data and inits
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

growth.stan <- stan('modelos/growth.stan', data=dat, chains=chains,
                 init=inits, open_progress=FALSE, control=list(adapt_delta=.99))
launch_shinystan(fit.stan)

## Repeat after
growth.nc.stan <- stan('modelos/growth_nc.stan', data=dat, chains=chains,
                       init=inits.nc, open_progress=FALSE,
                       control=list(adapt_delta=.99))
launch_shinystan(growth.nc.stan)

