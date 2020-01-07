jamesstein <- function(theta, m){
  # function to compute risk of james-stein estimator
  # theta - collection of parameter values th1 = th2 = ... = thp = theta
  # m - number of simulation iterations
  
  p <- 5        # number of means to estimate
  c <- 3        # constant in james-stein estimator
  
  N <- length(theta)
  risk <- array(0, c(N, 1))
  mcse <- risk
  
  for (i in 1:N){              # loop over values of theta
    loss <- array(0, c(m, 1))
    for (j in 1:m){            # loop over m iterations	
      x <- rnorm(p,theta[i],1)
      f <- 1 - c / sum(x ^ 2)
      est <- (f > 0) * f * x
      loss[j] <- sum((est - theta[i]) ^ 2)
    }
    
    risk[i] <- mean(loss)          # estimate at risk for ith value of theta
    mcse[i] <- sd(loss) / sqrt(m)  # mc estimate at standard error of estimate
  }
  d <- data.frame(Risk=risk, MC.SE=mcse)
  row.names(d) <- theta
  d
}
