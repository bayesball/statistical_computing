mc.trim2.is=function(n,m)
{
# monte carlo program with importance sampling
# to compute size of trim 2 t-stat that rejects when z > 1.28
#
# n is sample size, m is number of simulations

d=array(0,c(m,1))
for (i in 1:m)
{
	ybar=rnorm(1,mean=.4,sd=1/sqrt(n))
      z=sqrt(n)*ybar
	d[i]=(z>1.28)*dnorm(ybar,mean=0,sd=1/sqrt(n))/dnorm(ybar,mean=.4,sd=1/sqrt(n))
}
size=mean(d); se=sd(d)/sqrt(m)
return(c(size,se))
}
