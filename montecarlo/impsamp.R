impsamp=function(logf,gpar,iter,data)
{

# illustration of importance sampling to compute the mean of distribution f
# using normal importance sampler with mean m and standard deviation s
# (alternatively can use t importance sampler with median m, scale s, d.f. v)
# iter = number of iterations
# logf = contains definition of log density f
# par = parameters of importance sampler, say m and s
# data = data used with function logf

m=gpar[1]; s=gpar[2]
               
y=rnorm(iter,m,s)           # simulate m values of g
fy=exp(logf(y,data))        # compute f(y)
gy=dnorm(y,m,s)             # compute g(y)
w=fy/gy                     # compute weights 
est=sum(w*y)/sum(w)         # importance sampling estimate at E(Y)
SEest=sqrt(sum((y-est)^2*w^2))/sum(w)   # Monte Carlo standard error of estimate

return(list(est=est,se=SEest,y=y,wt=w))

}