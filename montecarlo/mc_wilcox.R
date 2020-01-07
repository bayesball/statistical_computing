mc_wilcox=function(iter)
{
# my naive program to compare the mean sq errors of the Wilcoxon stat and the sample
# mean under normal(0, 1) sampling

# focus on estimating D = mse(W) - mse(xbar)=E(W-THETA)^2-E(xbar-THETA)^2
# using independent sampling

n=10 		# sample of size 10
mn=0  	# assuming normal mean is equal to 0

lossW=array(0,c(iter,1))
lossXBAR=lossW

for (i in 1:iter)
{
   data=rnorm(n)
   West=wilcoxon(data)
   lossW[i]=(West-mn)^2
}

for (i in 1:iter)
{
   data=rnorm(n)
   xbar=mean(data)
   lossXBAR[i]=(xbar-mn)^2;
}

Dest=mean(lossW)-mean(lossXBAR)
Dse=sqrt(sd(lossW)^2/iter+sd(lossXBAR)^2/iter)
return(c(Dest,Dse))
}

