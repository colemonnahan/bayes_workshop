# source("insect_jags.r")
#SCRIPT FILE TO RUN THE TWO MODELS FOR INSECT OVIPOSITION
#Noble Hendrix
#noblehendrix@gmail.com
#11/27/14

the.data<- dget("insect_data2.txt")
plot(the.data$x, the.data$Y, xlab = "Female Egg Compliment", ylab = "Eggs laid on host", pch = 15)

library(rjags)

#run the first oviposition model
Y<-the.data$Y
N<-length(Y)

bug1.inits<-function()
list(alpha = runif(1,1,4) )
bug1.data<-list("Y"=Y,"N"=N)
bug1.params<-c("alpha") 

#initial JAGS model call with adapt 
insect1.jags<-jags.model(file="insect1.bug", data=bug1.data, inits=bug1.inits, n.chains=3, n.adapt=1000)

#return samplers being used for each Gibbs step
list.samplers(insect1.jags)  
update(insect1.jags, n.iter=1000) 
insect1.sim<-coda.samples(insect1.jags, variable.names=bug1.params, n.iter=10000, thin = 5)

gelman.diag(insect1.sim)
gelman.plot(insect1.sim)
summary(insect1.sim)

#DIC estimate
DIC.1<- dic.samples( insect1.jags, n.iter = 5000, thin = 5, type = "pD")


insect
#plot of 1st model
plot(the.data$x, the.data$Y, xlab = "Female Egg Compliment", ylab = "Eggs laid on host", pch = 15)
abline(h= summary(insect1.sim)[[2]][3], lwd = 3, col = 2)
abline(h=summary(insect1.sim)[[2]][1], lwd = 1, col = 2, lty = 2)
abline(h=summary(insect1.sim)[[2]][5], lwd = 1, col = 2, lty = 2)


#run the second oviposition model - note convergence dependent on the starting values since x.change and betas are correlated
x<-the.data$x
x.new<-4:23
N.new<-length(x.new)

# Two options for initializing chains:
# 1) First option - set the values directly
bug2.inits <- list( 
			  list(beta0 = c(2,2), x.change = 6),
			  list(beta0 = c(1,3),  x.change = 8),
			  list(beta0 = c(1,4), x.change = 10)  )

#2) Second option - use a function to initialize with random values:
bug2.inits<-function()
list( beta0 = append(runif(1, 1,2), runif(1, 3,4) ),  x.change = runif(1,4,23), Y.new = rpois(N.new, 2)  )
#Inits structured:

bug2.data<-list("Y"=Y,"N"=N, "x"=x, "x.new"=x.new, "N.new"=N.new)
bug2.params<-c("beta", "x.change", "mu.new", "Y.new") 
insect2.jags<-jags.model(file="insect2.bug", data=bug2.data, inits=bug2.inits, n.chains=3, n.adapt=1000)

#return samplers being used for each Gibbs step
list.samplers(insect2.jags)  
update(insect2.jags, n.iter=1000) 
insect2.sim<-coda.samples(insect2.jags, variable.names=bug2.params, n.iter=50000, thin = 5)

summary(insect2.sim)

#diagnostics
gelman.diag(insect2.sim, multivariate = F)
gelman.plot(insect2.sim)   #doesn't work due to multivariate gelman.diag not being positive definite

#want to see plot for the beta and change point parameters though - 
ind.b1<- which(varnames(insect2.sim)  == "beta[1]")
ind.b2<- which(varnames(insect2.sim)  == "beta[2]")
ind.x<- which(varnames(insect2.sim)  == "x.change")

#when above call doesn't work (multivariate may be singular)
par(mfrow = c(2,2))
 gelman.plot(insect2.sim[,ind.b1], auto.layout = F, main = "beta[1]")
 gelman.plot(insect2.sim[,ind.b2], auto.layout = F, main = "beta[2]")
 gelman.plot(insect2.sim[,ind.x], auto.layout = F, main = "x.change")

#DIC estimate
DIC.2<- dic.samples( insect2.jags, n.iter = 5000, thin = 5, type = "pD")

#Compare DIC estimates.  Hilborn and Mangel (1997) analyzed these data using 
#sums of squares and concluded that the best model was a single change point.  
#What is your conclusion after running these 2 models?


