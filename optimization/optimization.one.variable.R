## --------------------------------------------------------
g <- function(x){
  -(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x))
}


## --------------------------------------------------------
x.grid <- seq(-5, 5, length=100)
plot(x.grid, g(x.grid), type="l")


## --------------------------------------------------------
curve(g(x), -5, 5)


## --------------------------------------------------------
g1 <- function(x){
  - x + 2 * exp(- x) / (1 + exp(- x))
}


## --------------------------------------------------------
curve(g1(x), -5, 5)
abline(h = 0)


## --------------------------------------------------------
simple.iteration <- function(g1, x0, iterations, delta=0.1){
  current <- x0
  stored.values <- vector(length = iterations + 1)
  stored.values[1] <- x0
  for (j in 1:iterations){
    xnew <- current + g1(current)
    stored.values[j + 1] <- xnew 
    if(abs(xnew - current) < delta) break
    current <- xnew               
  }
  return(data.frame(Iteration=1:(j + 1), 
                    Value=stored.values[1:(j + 1)]))
}


## --------------------------------------------------------
simple.iteration(g1, 5, 10, delta=0.05)


## --------------------------------------------------------
g <- function(x){
  -(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x))
}


## --------------------------------------------------------
D(expression(-(1 - x) ^ 2 / 2 - x - 2 * log(1 + exp(- x))), 'x')
g1 <- function(x){
  2 * (1 - x)/2 - 1 + 2 * (exp(-x)/(1 + exp(-x)))
}


## --------------------------------------------------------
g2 <- function(x){
  -(2 * (exp(-x)/(1 + exp(-x)) - exp(-x) * exp(-x)/(1 + exp(-x))^2) + 2/2)
}


## --------------------------------------------------------
current <- 1
current <- current - g1(current) / g2(current); current


## --------------------------------------------------------
newton <- function(g1, g2, x0, iterations){
  current <- x0
  stored.values <- vector(length = iterations + 1)
  stored.values[1] <- x0
  for (j in 1:iterations){
    xnew <- current - g1(current) / g2(current)
    stored.values[j + 1] <- xnew  
    current <- xnew               
  }
  return(data.frame(Iteration=0:iterations, Value=stored.values))
}


## --------------------------------------------------------
newton(g1, g2, 5, 4)


## --------------------------------------------------------
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


## --------------------------------------------------------
bisection(g1, -5, 5, 10)

