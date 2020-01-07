mc_wilcox_control=function(iter)
{
# program to compute the mean sq error of the Wilcoxon stat
# mean under normal(0, 1) sampling

# focus on estimating D = mse(W)=E(W-THETA)^2
# using control sampling

n=10 		# sample of size 10
mn=0  	# assuming normal mean is equal to 0

cv=array(0,c(iter,1))

for (i in 1:iter)
{
   data=rnorm(n)
   West=wilcoxon(data); xbar=mean(data)
   lossW=(West-mn)^2
   lossXBAR=(xbar-mn)^2
   cv[i]=lossW-(lossXBAR-1/n)
}

return(c(mean(cv),sd(cv)/sqrt(iter)))
}

