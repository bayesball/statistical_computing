# control variate example
# estimating bias of median(x1, ..., xn)
# when yi are exponential(theta)

# naive mc
n <- 10
theta <- 1

naive.mc <- function(m){
  MED <- replicate(m, median(rexp(n, theta)))
  c(ESTIMATE=mean(MED), SE=sd(MED) / sqrt(m))
}

naive.mc(1000)

# use sample mean as a control variate

control.mc <- function(m){
  Y1 <- replicate(m, 
          {y <- rexp(n, theta)
          median(y) - (mean(y) - theta)}
          )
  c(ESTIMATE=mean(Y1), SE=sd(Y1) / sqrt(m))
}

control.mc(1000)

# we should see some variance reduction

