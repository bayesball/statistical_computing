jamesstein.common=function(theta,m)
{
# function to compute risk of james-stein estimator
# theta - collection of parameter values th1 = th2 = ... = thp = theta
# m - number of simulation iterations

p=5        # number of means to estimate
c=3        # constant in james-stein estimator

N=length(theta)
drisk=array(0,c(N,1))
mcse=drisk

for (i in 1:N)                 # loop over values of theta
{
	lossd=array(0,c(m,1))
	for (j in 1:m)            # loop over m iterations
	{
        	x=rnorm(p,theta[i],1)
        	f=1-c/sum(x^2);
        	est=(f>0)*f*x
        	lossd[j]=sum((x-theta[i])^2)-sum((est-theta[i])^2)
	}
   
	drisk[i]=mean(lossd)        # estimate at risk for ith value of theta
	mcse[i]=sd(lossd)/sqrt(m)  # mc estimate at standard error of estimate
}
return(cbind(drisk,mcse))
}

