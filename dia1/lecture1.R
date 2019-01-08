##

## Lecture 1

## Exercise 1
z <- rnorm(1e5)
hist(z, probability=TRUE)
x <- seq(-4,4, len=100)
lines(x, dnorm(x))


## Exercise 2
mu <- 0
sd <- 1

x <- rnorm(1e4, mean=0, sd=1)
y <- rnorm(1e4, mean=0, sd=1)
mean(x>1 & y>1)
mean(x< -2 & y>1/2)

mean(x< -5)
pnorm(-5, mean=mu, sd=sd)
