mc.trim2.cv=function(n,m)
{
# monte carlo program with control variable
# to compute size of trim 2 t-stat that rejects when z > 1.28
#
# n is sample size, m is number of simulations

d=array(0,c(m,1))
for (i in 1:m)
{
	y=rnorm(n,mean=0,sd=1)
      ts=trim2(y)
      t=sqrt(n)*mean(y)
	d[i]=(ts>1.28)-((t>1.28)-.1)
}
size=mean(d); se=sd(d)/sqrt(m)
return(c(size,se))
}
