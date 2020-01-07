folly <- function(mu0, n, alpha.t, alpha.g, m){

# Monte Carlo simulation of prob(Type I) error for Schucany and Ng paper
# exponential data case
# inputs:  mu0 - value to be tested
#          n - sample size
#          alpha.t - type 1 error rate for t test
#          alpha.g - type 1 error rate for S-W test
#          m - number of MC simulations

n1 <- 0       # count of number of samples that pass normality test
nreject <- 0  # number of rejections

for (i in 1:m){
	y <- rexp(n, rate=mu0)		      	  # simulate exponential data
	s <- shapiro.test(y)		            # initial normality test on data

	if(s$p.value > alpha.g){	# if you fail to reject ...	
		n1 <- n1 + 1
		Tstat <- (mean(y) - mu0) / (sd(y) / sqrt(n))
		nreject <- nreject + (abs(Tstat) > qt(1 - alpha.t / 2, n - 1))
	}
}
power <- (m - n1) / m
se.power <- sqrt(power * (1 - power) / m)
p <- nreject / n1         # estimate of (conditional) Type I error
se <- sqrt(p * (1 - p) / n1)

return(list(power=power, se.power=se.power, p=p, se=se))
}

# try it out for exponential data, testing mu=1, n=20, both error rates of .1
# and 10,000 iterations

folly(1, 20, 0.1, 0.1, 10000)
