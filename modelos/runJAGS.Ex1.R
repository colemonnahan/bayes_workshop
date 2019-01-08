# source("runJAGS.Ex1.r")
#SCRIPT TO RUN A BAYESIAN LINEAR REGRESSION USING JAGS

#read the data:
the.data<-read.table("ex1.data.csv", sep = ',')
plot(1:6, the.data, pch = 15, ylab = "Log Abundance", xlab = "Time", ylim = c(5, 9) )

#load libraries
library(rjags)
library(coda)

#################
#Example1
#################

#format the data
y<- as.numeric(the.data)
N<-length(y)
x<- 1:N

#construct lists and functions for constructing JAGS model
dat<- list("N" = N, "y" = y, "x" = x)
jags.inits <- function() {list (alpha = rnorm(1), beta = rnorm(1), tau.y = runif(1))}
parameters<- c("alpha", "beta", "tau.y")

#initial JAGS model call with adapt
reg.jags<-jags.model(file="example1.bug", data=dat, inits=jags.inits, n.chains=3, n.adapt=1000)

#return samplers being used for each Gibbs step
list.samplers(reg.jags)

#last coefficient values
coef(reg.jags)

# burn in
update(reg.jags, n.iter=1000)
coef(reg.jags)

#update model to draw from posteriors
seal.sim<-coda.samples(reg.jags, variable.names=parameters, n.iter=5000)

#obtain summary statistics
summary(seal.sim)

#Plotting diagnostics
#1. Plot of the traces and density of each chain for each parameter
plot(seal.sim)
#2. Cumulative distribution plot
cumuplot(seal.sim)

#Diagnostics
#1. Heidelberger: 1) Cramer-von Mises test for stationarity, 2) half-width test for samples with same mean
heidel.diag(seal.sim)

#2. Geweke - for each variable in each chain, compare to Z score (+/- 1.96)
geweke.diag(seal.sim)
#How Geweke statistic changes as larger portions from first part of chain are discarded
geweke.plot(seal.sim[[1]])  #NOTE SLIGHT MODIFICATION HERE
#3. Gelman-Rubin
gelman.diag(seal.sim)
gelman.plot(seal.sim)

#Evaluate autocorrelation in the chain
effectiveSize(seal.sim)

#Run the chains a bit longer
seal.sim2<-coda.samples(reg.jags, variable.names=parameters, n.iter=10000, thin = 5)
summary(seal.sim2)  #note that iterations are 6000 - 16000
effectiveSize(seal.sim2)  #based on entire chain of 15000

# Gelman-Rubin
gelman.diag(seal.sim2)
gelman.plot(seal.sim2)

summary(seal.sim2)

#Accessing simulations
class(seal.sim)

#First 10 elements of the 1st chain
  seal.sim[[1]][1:10,]

#First 10 elements of the 2nd variable (beta) for each of the 3 chains
seal.sim[1:10,2]

#Calculate probability of beta being greater than 0 for each chain
length(which(seal.sim[[1]][4001:5000,2] > 0 ) )/1000
length(which(seal.sim[[2]][4001:5000,2] > 0 ) )/1000
length(which(seal.sim[[3]][4001:5000,2] > 0 ) )/1000

#What about increasing the number of samples per chain and calculating the probability across all chains?
#Your turn:





#################
#Example1a						#
#################

#new times for predicted log abundances out to 2008
x.new<- 7:14
M<- length(x.new)
dat1a<- list("N" = N, "y" = y, "x" = x, "x.new" = x.new, "M"= M)
jags.inits1a <- function() {list (alpha = rnorm(1), beta = rnorm(1), tau.y = runif(1), y.new = rnorm(M), y.hat = rnorm(N)  )  }

#Quick Quiz - Why is y.new in the inits function?

parameters1a<-c("alpha", "beta", "sigma", "mu.hat", "y.hat" , "mu.new", "y.new")
#initial JAGS model call with adapt
reg.jags1a<-jags.model(file="example1a.bug", data=dat1a, inits=jags.inits1a, n.chains=3, n.adapt=1000)

update(reg.jags1a, n.iter=5000)

seal.sim1a<-coda.samples(reg.jags1a, variable.names=parameters1a, n.iter=15000, thin = 5)
summary(seal.sim1a)[[2]]

#construct a plot of the observed and predicted outputs from the Example1a model:
plot(1:6, y, pch = 15, ylab = "Log Abundance", xlab = "Time", ylim = c(1, 11) , xlim = c(1,14) )

#1. Fitted mean log abundance (median and 95% Credible Interval)
lines(1:6, summary(seal.sim1a)[[2]][3:8,3], col = 4, lwd = 2)
lines(1:6, summary(seal.sim1a)[[2]][3:8,1], col = 4, lwd = 2, lty = 2)
lines(1:6, summary(seal.sim1a)[[2]][3:8,5], col = 4, lwd = 2, lty = 2)

#2. Forecasted observed log abundance - includes observation error (median and 95% Credible Interval)
#  helper function to id the location of the params which(varnames(seal.sim1a) =="y.hat[1]")
lines(1:6, summary(seal.sim1a)[[2]][18:23,3], col = 2, lwd = 2)
lines(1:6, summary(seal.sim1a)[[2]][18:23,1], col = 2, lwd = 2, lty = 2)
lines(1:6, summary(seal.sim1a)[[2]][18:23,5], col = 2, lwd = 2, lty = 2)

#3. Forecasted mean log abundances (median and 95% Credible Interval)
# which(varnames(seal.sim1a) =="mu.new[1]")
lines(7:14, summary(seal.sim1a)[[2]][9:16,1], col = "dark blue", lwd = 2, lty = 2)
lines(7:14, summary(seal.sim1a)[[2]][9:16,3], col = "dark blue", lwd = 2)
lines(7:14, summary(seal.sim1a)[[2]][9:16,5], col = "dark blue", lwd = 2, lty = 2)

#4. Forecasted observed log abundance - includes observation error (median and 95% Credible Interval)
# which(varnames(seal.sim1a) =="y.new[1]")
lines(7:14, summary(seal.sim1a)[[2]][24:31,3], col = "dark red", lwd = 2)
lines(7:14, summary(seal.sim1a)[[2]][24:31,1], col =  "dark red", lwd = 2, lty = 2)
lines(7:14, summary(seal.sim1a)[[2]][24:31,5], col =  "dark red", lwd = 2, lty = 2)

#observed data point in 2007 (14) log abundance = 6.786
points(14, 6.78, pch = 15, col = "dark red", cex = 2)
