sir=function(logf,gpar,iter,data)
{

# illustration of sir algorithm to simulate from distribution f
# using normal importance sampler with mean m and standard deviation s
# (alternatively can use t importance sampler with median m, scale s, d.f. v)
# iter = number of iterations
# logf = contains definition of log density f
# par = parameters of importance sampler, say m and s
# data = data used with function logf

rdisc=function(n,p)
{
q=cumsum(p)
r=runif(n); ones=1+0*r;
rindices=ones
for (i in 1:length(q))
	{
	rindices=rindices+(r>(q[i]*ones))
	}
return(rindices)
}

m=gpar[1]; s=gpar[2]
               
y=rnorm(iter,m,s)           # simulate iter values of g
fy=exp(logf(y,data))        # compute f(y)
gy=dnorm(y,m,s)             # compute g(y)
w=fy/gy                     # compute weights 
probs=w/sum(w)		    # convert to probabilities
x=y[rdisc(iter,probs)]           # resample to get samplel from f

return(x)

}