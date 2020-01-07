# program to compare the mean sq errors of the Wilcoxon stat 
# and the sample mean under normal(0, 1) sampling

# illustrate variance reduction methods

# first common random variables

mc_wilcox_commonrv <- function(iter){
# focus on estimating 
# D = mse(W)-mse(xbar)=E(W-THETA)^2-E(xbar-THETA)^2
# using sampling with common random variables

n <- 10 		# sample of size 10
mn <- 0  	# assuming normal mean is equal to 0

dloss <- replicate(iter,
            {data <- rnorm(n)
             West <- wilcox.test(data, conf.int=TRUE)$estimate
             xbar <- mean(data)
             (West - mn) ^ 2 - (xbar - mn) ^ 2 
             })

Dest <- mean(dloss)
Dse <- sd(dloss) / sqrt(iter)
c(Diff.Estimate=Dest, Standard.Error=Dse)
}

mc_wilcox_commonrv(1000)

# control variables

mc_wilcox_control <- function(iter){
  # focus on estimating D = mse(W)=E(W-THETA)^2
  # using control sampling
  
  n <- 10   	# sample of size 10
  mn <- 0  	# assuming normal mean is equal to 0
  
cv <- replicate(iter,
   {data <- rnorm(n)
   West <- wilcox.test(data, conf.int=TRUE)$estimate
   xbar <- mean(data)
   lossW <- (West - mn) ^ 2
   lossXBAR <- (xbar - mn) ^ 2
   lossW - (lossXBAR - 1 / n)
})
Dest <- mean(cv)
Dse <- sd(cv) / sqrt(iter)
c(Estimate=Dest, Standard.Error=Dse)
}

mc_wilcox_control(1000)

