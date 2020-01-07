#########################################################################
### EXAMPLE 3.4 SIMULATED ANNEALING - edited by Jim Albert
#########################################################################
# baseball.dat   = entire data set
# baseball.sub 	= matrix of all predictors
# salary.log 	= response, log salary
# n 		= number of observations in the data set
# m 		= number of predictors in the data set
# run 		= vector of the parameters included in the best model found
# 		  (1 = included, 0 = omitted)
# best.aic 	= AIC value for the best model found
# aics 		= AIC values for the model at each step
# 		  (used for plotting)
# cooling 	= cooling schedule
# tau.start 	= initial temperature
# tau 		= temperature schedule
#########################################################################

## INITIAL VALUES
baseball.dat <- read.table("baseball.dat", header=TRUE)
baseball.dat$freeagent <- factor(baseball.dat$freeagent)
baseball.dat$arbitration <- factor(baseball.dat$arbitration)
baseball.sub <- baseball.dat[, -1]
salary.log <- log(baseball.dat$salary)
n <- length(salary.log)
m <- length(baseball.sub[1, ])
cooling <- c(rep(60, 5), rep(120, 5), rep(220, 5))
tau.start <- 10
tau <- rep(tau.start, 15)
aics <- NULL

# INITIALIZES STARTING RUN, TEMPERATURE SCHEDULE
set.seed(1999)
run <- rbinom(m, 1, .5)
run.current <- run
run.vars <- baseball.sub[, run.current==1]
g <- lm(salary.log ~ ., run.vars)
run.aic <- extractAIC(g)[2]
best.aic <- run.aic
aics <- run.aic
for(j in 2:15){tau[j] <- 0.9 * tau[j - 1]}

## MAIN
for(j in 1:15){
  
  # RANDOMLY SELECTS A PREDICTOR TO ADD/REMOVE FROM THE MODEL
  # AND ACCEPTS THE NEW MODEL IF IT IS BETTER OR WITH PROBABILITY p
  for(i in 1:cooling[j]){
    pos <- sample(1:m, 1)
    run.step <- run.current
    run.step[pos] <- !run.current[pos]
    run.vars <- baseball.sub[, run.step==1]
    g <- lm(salary.log ~., run.vars)
    run.step.aic <- extractAIC(g)[2]
    p <- min(1, exp((run.aic - extractAIC(g)[2]) / tau[j]))
    if(run.step.aic < run.aic){
      run.current <- run.step
      run.aic <- run.step.aic}
    if(rbinom(1, 1, p)){
      run.current <- run.step
      run.aic <- run.step.aic}
    if(run.step.aic < best.aic){
      run <- run.step
      best.aic <- run.step.aic}
    aics <- c(aics, run.aic)
  }
}

## OUTPUT
run 		# BEST LIST OF PREDICTORS FOUND
best.aic 	# AIC VALUE
aics		# VECTOR OF AIC VALUES

## PLOT OF AIC VALUES
plot(aics, ylim=c(-420, -360), type="n", ylab="AIC", xlab="Iteration")
lines(aics)

(1:2001)[aics==min(aics)]


#########################################################################