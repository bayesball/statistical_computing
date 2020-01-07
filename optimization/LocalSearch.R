#########################################################################
# COMPUTATIONAL STATISTICS
# by Geof Givens and Jennifer Hoeting
# CHAPTER 3 EXAMPLES (last update 10/1/12) - edited by Jim Albert
#########################################################################
### EXAMPLE 3.3 RANDOM STARTS LOCAL SEARCH
#########################################################################
# baseball.dat 	= entire data set
# baseball.sub 	= matrix of all predictors
# salary.log	= response, log salary
# n 		= number of observations in the data set
# m 		= number of predictors in the data set
# num.starts 	= number of random starts
# runs 		= matrix containing the random starts where each row is a
# 		  vector of the parameters included for the model
# 		  (1 = included, 0 = omitted)
# runs.aic	= AIC values for the best model found for each
# 		  of the random starts
# itr 		= number of iterations for each random start
#########################################################################

## INITIAL VALUES
baseball.dat <- read.table("baseball.dat", header=TRUE)
baseball.dat$freeagent <- factor(baseball.dat$freeagent)
baseball.dat$arbitration <- factor(baseball.dat$arbitration)
baseball.sub <- baseball.dat[, -1]      # matrix of potential covariates
salary.log <- log(baseball.dat$salary)  # response variable
n <- length(salary.log)                 # sample size
m <- length(baseball.sub[1,])           # number of predictors
num.starts <- 5                         # number of random starts
runs <- matrix(0, num.starts, m)        # store final model as indicator vector
itr <- 15                               # number of steps in each search
runs.aic <- matrix(0, num.starts, itr)  # store AIC values

# INITIALIZES STARTING RUNS
# set.seed(19676)                         # set random seed (may want to change)
for(i in 1:num.starts)
  {runs[i, ] <- rbinom(m, 1, .5)}  # each predictor has 1/2 chance of being in model

## MAIN
for(k in 1:num.starts){
	run.current <- runs[k, ]      # pick up starting model for kth random start

	# ITERATES EACH RANDOM START
	for(j in 1:itr){
		run.vars <- baseball.sub[, run.current==1]  # subset of predictor matrix
		g <- lm(salary.log ~ ., run.vars)    # fit current model
		run.aic <- extractAIC(g)[2]          # get  current AIC
		run.next <- run.current

		# TESTS ALL MODELS IN THE 1-NEIGHBORHOOD AND SELECTS THE
		# MODEL WITH THE LOWEST AIC
		for(i in 1:m){
			run.step <- run.current
			run.step[i] <- ! run.current[i]         # either removes or adds ith covariate
			run.vars <- baseball.sub[, run.step==1] # get subset of predictor matrix
			g <- lm(salary.log ~ .,run.vars)        # fit model and extract AIC
			run.step.aic <- extractAIC(g)[2]
			if(run.step.aic < run.aic){             # if AIC is lower, record model and AIC
				run.next <- run.step
				run.aic <- run.step.aic
			}
		}
		run.current <- run.next                   # record model with smallest AIC
		runs.aic[k, j] <- run.aic
	}
	runs[k, ] <- run.current                    # record final model after itr moves
}

## OUTPUT
runs 		  # LISTS OF PREDICTORS
runs.aic 	# AIC VALUES

##PLOT
plot(1:(itr*num.starts), -c(t(runs.aic)),
     xlab="Cumulative Iterations",
     ylab="Negative AIC",
     ylim=c(360,420),
     type="n")
for(i in 1:num.starts) {
  lines((i-1) * itr + (1:itr), -runs.aic[i,]) }

# alternative graph by JA

D <- NULL
for(j in 1:5){
  D <- rbind(D, data.frame(Step=1:15, 
                           Neg.AIC=-runs.aic[j, ], 
                           Run=paste("Random Start", j)))
}
library(ggplot2)
ggplot(D, aes(Step, Neg.AIC)) + geom_line() + facet_wrap(~ Run) + ylim(350, 430)

#########################################