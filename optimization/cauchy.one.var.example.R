#############################################
# MATH 7580 Spring 2014
# Optimization in One Variable
#
# Illustrate simple iteration, Newton, and bracketing
#############################################

# normal mean, logistic prior example

# define g function which is logarithm of posterior

g <- function(x){
  -(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x))
}

# plot log g over an interval

x.grid <- seq(-5, 5, length=100)
plot(x.grid, g(x.grid), type="l")

# this is a quick way of doing the same thing

curve(g(x), -5, 5)

# program the derivative of log g

g1 <- function(x){
  - x + 2 * exp(- x) / (1 + exp(- x))
}

curve(g1(x), -5, 5)
abline(h = 0)  # add a horizontal line at 0

# simple iteration

simple.iteration <- function(g1, x0, iterations){
  current <- x0
  stored.values <- vector(length = iterations + 1)
  stored.values[1] <- x0
  for (j in 1:iterations){
    xnew <- current + g1(current)
    stored.values[j + 1] <- xnew  # store new value
    current <- xnew               # new value is now current value
  }
  return(data.frame(Iteration=0:iterations, Value=stored.values))
}

# illustrate simple iteration starting at x = 5 and 10 iterations

simple.iteration(g1, 5, 10)

# Newton's method

g <- function(x){
  -(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x))
}

# using D function to compute first derivative

g1 <- function(x){
  2 * (1 - x)/2 - 1 + 2 * (exp(-x)/(1 + exp(-x)))
}

# use D function again to compute derivatives
# syntax:  D(expression(-(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x)) ), 'x')

g2 <- function(x){
  -(2 * (exp(-x)/(1 + exp(-x)) - exp(-x) * exp(-x)/(1 + exp(-x))^2) + 
      2/2)
}

# Newton step 

current <- 1
current <- current - g1(current) / g2(current); current

# write program

newton <- function(g1, g2, x0, iterations){
  current <- x0
  stored.values <- vector(length = iterations + 1)
  stored.values[1] <- x0
  for (j in 1:iterations){
    xnew <- current - g1(current) / g2(current)
    stored.values[j + 1] <- xnew  # store new value
    current <- xnew               # new value is now current value
  }
  return(data.frame(Iteration=0:iterations, Value=stored.values))
}

# Newton method, starting at x = 5 and four steps

newton(g1, g2, 5, 4)

# bisection method

bisection <- function(f, L, U, n){
  output <- NULL
  for (i in 1:n) {
    x <- (L + U) / 2
    if (f(x) * f(L) < 0) U <- x else L <- x
    output <- rbind(output, c(i, x))
  }
  dimnames(output)[[2]] <- c("Iteration", "Value")
  return(output)
}

# bracketing, start with interval [-5, 5] and 10 iterations

bisection(g1, -5, 5, 10)

