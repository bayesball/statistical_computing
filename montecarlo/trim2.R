trim2=function(y)
{
ind=(y>min(y))&(y<max(y))
y1=y[ind]
z=mean(y1)*sqrt(length(y1))
return(z)
}