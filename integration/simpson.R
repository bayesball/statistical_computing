simpson=function(lpost,a,b,n)
{
# n is number of subintervals
# want n even (n1 odd)

h=(b-a)/n
x=a+(0:n)*h
int=0
for (i in 1:(n/2))
{
int=int+exp(lpost(x[2*i-1]))+4*exp(lpost(x[2*i]))+exp(lpost(x[2*i+1]))
}
return(int*h/3)
}