f <- function(a, b) {
  a^2
}
  
f <- function(a, b) {
  print(a)
  print(b)
}

## I am going to redefine the plot funtion and now some of its arguments by default behave differently
myplot <- function(x, y, type = "l", ...) {
  plot(x, y, type = type, ...)
}

## A new function containing z as a free variable. How R determines the content of z?
f <- function(x, y) {
  x^2 + y / z
}

## Function returning a function that given an argument raises the argument to the nth power
make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) {
  x*y
}

g <- function(x) {
  a <- 3
  x+a+y
}

## Example of a constructor function
make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma^2)
    b <- -0.5*sum((data-mu)^2) / (sigma^2)
    -(a + b)
  }
}