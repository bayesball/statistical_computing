riemann=function(lpost,a,b,n)
{
h=(b-a)/(n-1)
x=a+(0:(n-1))*h
int=h*sum(exp(lpost(x)))
return(int)
}
