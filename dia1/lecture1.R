### Lecture 1


## Review the R functions for normal random variables
rnorm(5)
dnorm(x=3, mean=0, sd=1)
pnorm(.5, mean=0, sd=1)
qnorm(.005, mean=0, sd=1)


## Exercise 1.1
png('exercise1.png', width=5, height=2.5, units='in', res=300)
n <- 1e5
z <- rnorm(n)
par(mgp=c(1.5,.5,0), mar=c(3,3,1,1))
hist(z, probability=TRUE, main=NA)
x <- seq(-4,4, len=100)
lines(x, dnorm(x), lwd=2)
box()
dev.off()

