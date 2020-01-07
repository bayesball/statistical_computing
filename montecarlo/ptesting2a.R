ptesting2a=function(lam,lam0,m)
{
# exercise 6.5 from Givens and Hoeting

# importance sampling using Poisson envelope
# with mean equal to lam0

t=array(0,c(m,1))
n=25
for (i in 1:m)
{
	s=rpois(1,n*lam0)
	fs=dpois(s,n*lam)
	gs=dpois(s,n*lam0)
	z=(s/n-2)/sqrt(2/n)
	t[i]=(z>=1.645)*fs/gs
}

return(c(mean(t),sd(t)/sqrt(m)))

}