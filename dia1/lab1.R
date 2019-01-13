##


## Homework 1

## Triangle distribution
f <- function(x, c=4){
  if(0<x & x<1/2) return(c*x)
  if(1/2<x & x<1) return(c*(1-x))
  return(0)
}
x <- seq(0,1, len=100)
y <- sapply(x, f, c=1)
plot(x,y, type='l')

## Piecewise integration: P(0<x<1)
F1 <- function(x) 4*x^2/2
F2 <- function(x) 4*x-4*x^2/2
p1 <- F1(1/2)-F1(0)
p2 <- F2(1)-F2(1/2)
p1+p2
## Integration P(.35 < x < .98)
p1 <- F1(1/2)-F1(.35)
p2 <- F2(.98)-F2(1/2)
p1+p2

## 1.2
mean(rnorm(1e5)< -5)
pnorm(-5, 0,1)

x <- rnorm(500)
y <- sapply(1:500, function(n) mean(x[1:n]))
plot(1:500, y)
abline(h=0)
hist(x^2, prob=TRUE)
mean(x^2)
mean(x)^2

## Use change of variables formulat to get the PDF of Y
fy <- function(y) dnorm(sqrt(y))/sqrt(y)
y <- seq(.001, 12, len=1000)
hist(rnorm(1e6)^2, breaks=100, prob=TRUE, xlim=c(0,10))
lines(y, fy(y), lwd=2, col='red')

