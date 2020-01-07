ptesting3=function(lam,m)
{
# exercise 6.6 from Givens and Hoeting

# importance sampling using Poisson envelope
# with mean equal to H0 rejection threshold

t=array(0,c(m,1)); d=t
n=25
lam0=2.4653
for (i in 1:m)
{
	s=rpois(1,n*lam0)
	fs=dpois(s,n*lam)
	gs=dpois(s,n*lam0)
	z=(s/n-2)/sqrt(2/n)
	t[i]=(z>=1.645)*fs/gs
      d[i]=fs/gs
}

return(c(mean(t)/mean(d)))

}