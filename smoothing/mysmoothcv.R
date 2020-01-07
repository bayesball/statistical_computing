mysmoothcv=function(x,y,k)
{
s=sort(x,index.return=T)
x=x[s$ix]
y=y[s$ix]
smooth=0*x
n=length(x)
for (i in 1:n)
{
ind=1:n
ind1=(ind>=max(c(i-(k-1)/2,1)))&(ind<=min(c(i+(k-1)/2,n))&(ind!=i))
smooth[i]=mean(y[ind1])
}

cv=sum((y-smooth)^2)/n

return(list(x=x,y=smooth,cv=cv))
}

