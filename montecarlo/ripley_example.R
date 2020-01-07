# ripley_example.R

# Brian Ripley's example of variance reduction

# illustrate four methods of approximating P(X > 2) when X is Cauchy(0,1)

# try each method 10 times, look at average estimate and standard deviation
# of the estimates

results <- c()  # matrix that will store results

N <- 10    # number of times each method will be tried
m <- 1000  # each estimate based on 1000 simulated values

truth <- 1 - pcauchy(2)  # actual Prob(X > 2) 
truth

for (i in 1:N){

	p1 <- sum(rcauchy(m) > 2) /m  		          # METHOD 1

	p2 <- (1 / 2) *sum(abs(rcauchy(m)) > 2) / m	# METHOD 2

	x <- runif(m, min=0, max=2)              
	p3 <- 1 / 2 - sum(2 / pi / (1 + x ^ 2)) / m # METHOD 3

	y <- runif(m, min=0, max=.5)
	p4 <- sum(y ^ (- 2) / (2 * pi * (1 + y ^ (- 2)))) / m  # METHOD 4

	results <- rbind(results, c(p1, p2, p3, p4)) # STORE RESULTS
}

apply(results, 2, mean)    # computes mean of 10 estimates
apply(results, 2, sd)      # computes sd of 10 estimates

