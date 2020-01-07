mc_wilcox_commonrv <- function(iter)
{
# program to compare the mean sq errors of the Wilcoxon stat 
# and the sample
# mean under normal(0, 1) sampling

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
