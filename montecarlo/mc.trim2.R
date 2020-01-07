mc.trim2=function(n,m)
{
# monte carlo program
# to compute size of trim 2 t-stat that rejects when z > 1.28
#
# n is sample size, m is number of simulations

reject=0
for (i in 1:m)
{
	y=rnorm(n,mean=0,sd=1)
      ts=trim2(y)
	reject=reject+(ts>1.28)
}
size=reject/m
return(c(size,sqrt(size*(1-size)/m)))
}
