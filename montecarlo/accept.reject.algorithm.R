###################################################
# Illustration of accept reject algorithm
####################################################

acc.reject <- function(n){
  # generic accept/rejection algorithm
  # want to simulate n values from density f
  # density g is covering density
  # put in values between *'s for specific example
  # we illustrate here for f=beta(3,2) and g=triangular density defined below
  
  Gi <- function(x){
    # inverse cdf for density (CORRECTION:  Correct f is given)
    # f(x) = 2.5 x, 0 < x < .8
    #        10 (1-x), .8 < x < 1
    y1 <- sqrt(.8 * x)
    y2 <- 1 - sqrt(.2 * (1 - x))
    y <- y1 * (x < .8) + y2 * (x >= .8)
    return(y)
  }
  
  f <- function(x){  #***************************** define target density
    y <- x ^ (3 - 1) * (1 - x) ^ (2 - 1)           # kernel of a beta(3,2)
    return(y)
  }
  
  g=function(x){  #***************************** define covering density
    y <- 3 * x * (x < .8) + 12 * (1 - x) * (x>= .8)
    return(y)
  }
  
  #***************  MAIN PROGRAM ***************************************
  
  # PUT IN c=max(f/g)
  c <- .0833
  #*********************************************************
  i <- 0                              # i is index for simulated values
  N <- 0                              # N counts # of iterations of algorithm
  z <- rep(0, n)                      # z stores simulated values from f
  while (i < n){                      # continue until n values simulated from f
    x <- Gi(runif(1))                 # simulate variate from density g
    u <- runif(1)                     # simulate uniform
    if (u <= (f(x) / c / g(x))){      # if condition satisfied
      i <- i + 1                      # update index
      z[i] <- x                       # store simulated value
    }           
    N <- N + 1                   # update counter
  }
  acceptrate <- n / N            # computes acceptance rate
  
  output <- list(arate=acceptrate, var=z)
  return(output)
  #*****************************************************************
}

# simulate 10000 draws using this algorithm

S <- acc.reject(10000)

# acceptance rate?

S$arate

# construct a density histogram of simulated draws and overlay
# the exact beta(3, 2) density

library(MASS)
truehist(S$var)
curve(dbeta(x, 3, 2), add=TRUE, col="red", lwd=3)

#########################################################