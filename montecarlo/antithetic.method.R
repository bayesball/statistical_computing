# Illustration of Antithetic Method

mc_wilcox_anti <- function(iter){
  # focus on estimating E(W) from exponential sampling
  # using sampling with antithetic random variables 
  n=10   	# sample of size 10 
  variates <- replicate(iter / 2, 
    { u <- runif(n)
      data1 <- -log(u)
      data2 <- -log(1 - u)
      W1 <- wilcox.test(data1, conf.int=TRUE)$estimate
      W2 <- wilcox.test(data2, conf.int=TRUE)$estimate
      (W1 + W2) / 2
  })  
  return(c(ESTIMATE=mean(variates), 
           SE.EST=sd(variates) / sqrt(iter)))
}

mc_wilcox_naive <- function(iter){  
  n=10     # sample of size 10  
  variates <- replicate(iter,  
      W <- wilcox.test(-log(runif(n)), conf.int=TRUE)$estimate
  )
return(c(ESTIMATE=mean(variates), 
         SE.EST=sd(variates) / sqrt(iter)))
}
mc_wilcox_naive(1000)
mc_wilcox_anti(1000)