jamesstein.M <- function(THETA, p, c, m){
  # function to compute risk of james-stein estimator
  # THETA - vector of commonvalues th1 = th2 = ... = thp = theta 
  # p - number of means to estimate
  # c - constant in james-stein estimator
  # m - number of simulation iterations
  
  N <- length(THETA)
  loss <- matrix(0, m, N)

  for (j in 1:m){            # loop over m iterations	
      x <- outer(rnorm(p, 0, 1), THETA, "+")
      f <- 1 - c / apply(x ^ 2, 2, sum)
      fm <- t(matrix(f, N, p))
      est <- (fm > 0) * fm * x
      loss[j, ] <- apply((est - outer(rep(1, p), THETA)) ^ 2, 2, sum)
    }
     
  risk <- apply(loss, 2, mean)          # estimate at risk for ith value of theta
  mcse <- apply(loss, 2, sd) / sqrt(m)  # mc estimate at standard error of estimate
  d <- data.frame(Risk=risk, MC.SE=mcse)
  row.names(d) <- THETA
  d
}

theta <- seq(0, 2, .05)
O <- jamesstein.M(theta, 5, 3, 1000)
plot(theta, O$Risk)

