adquad1=function(logpost,mom,iter,par)
{

ns=function(logpost,mom,par)
{
tq=c(-3.4362, -2.5327, -1.7567, -1.0366, -0.3429, 0.3429, 1.0366, 1.7567, 2.5327, 3.4362)
wet=c(1.0255, 0.8207, 0.7414, 0.7033, 0.6871, 0.6871, 0.7033, 0.7414, 0.8207, 1.0255)
con=sqrt(2)

mx=mom[1]; sx=mom[2]

x=mx+tq*sx*con
wx=wet*sx*con	
f=logpost(x,par)

abf=exp(f)*wx
spost=sum(abf); smx=sum(abf*x); smxx=sum(abf*x^2)
mx=smx/spost; sx=sqrt(smxx/spost-mx^2)
spost=log(spost)
stuff=list(int=spost,mom=c(mx,sx),x=x,f=f)

return(stuff)
}

for (i in 1:iter)
{
  stuff=ns(logpost,mom,par)
  mom=stuff$mom
}

return(stuff)
}