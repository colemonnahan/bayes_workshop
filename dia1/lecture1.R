### Lecture 1

## Review the R functions for normal random variables
rnorm(5)
dnorm(x=3, mean=0, sd=1)
pnorm(.5, mean=0, sd=1)
qnorm(.5, mean=0, sd=1)

## Exercise 1.1
plot.samples <- function(n){
  z <- rnorm(n)
  hist(z, probability=TRUE, main=NA,  xlim=c(-4,4), ylim=c(0,.6))
  x <- seq(-4,4, len=100)
  lines(x, dnorm(x), lwd=2)
  box()
}
par(mfcol=c(3,1), mar=c(2,2,1,.1))
plot.samples(5)
plot.samples(100)
plot.samples(10000)

