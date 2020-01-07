mc_wilcox <- function(iter){
# my naive program to compare the mean sq errors of the Wilcoxon stat 
# and the sample mean under normal(0, 1) sampling
# focus on estimating 
# D = mse(W) - mse(xbar)=E(W-THETA)^2-E(xbar-THETA)^2
# using independent sampling

wilcoxon <- function(data){
    # computes wilcoxon estimate for the median of a population
    # estimate is the median of the averages {(x(i)+x(j))/2, i<=j}
    n <- length(data)
    datam <- matrix(data, c(n, 1))   
    data1 <- datam %*% array(1, c(1, n))
    data2 <- array(1, c(n, 1)) %*% t(datam)
    avgs <- (data1 + data2) / 2
    a=c()
    for (i in 1:n){
      for (j in 1:i){
        a <- c(a, avgs[i,j])
      }
    }
    median(a)
}  
  
n <- 10 		# sample of size 10
mn <- 0  	  # assuming normal mean is equal to 0, stan dev is 1

lossW <- array(0, c(iter, 1))
lossXBAR <- lossW

for (i in 1:iter){
   data <- rnorm(n)
   West <- wilcoxon(data)
   lossW[i] <- (West - mn) ^ 2
}

for (i in 1:iter){
   data <- rnorm(n)
   xbar <- mean(data)
   lossXBAR[i] <- (xbar - mn) ^ 2
}

Dest <- mean(lossW) - mean(lossXBAR)
Dse <- sqrt(sd(lossW) ^ 2 / iter + sd(lossXBAR) ^ 2 / iter)
c(Estimate=Dest, Standard.Error=Dse)
}

# run MC simulation for 10,000 iterations

mc_wilcox(10000)