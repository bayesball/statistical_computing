mc.ztest=function(n,m)
{
# monte carlo program
# to compute size of z-test for normal mean that rejects when z > 1.28
#
# n is sample size, m is number of simulations

reject=0
for (i in 1:m)
{
	ybar=rnorm(1,mean=0,sd=1/sqrt(n))
	z=sqrt(n)*ybar
	reject=reject+(z>1.28)
}
size=reject/m
return(c(size,sqrt(size*(1-size)/m)))
}
