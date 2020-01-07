wilcoxon=function(data)
{
# computes wilcoxon estimate for the median of a population
# estimate is the median of the averages {(x(i)+x(j))/2, i<=j}

n=length(data)
datam=matrix(data,c(n,1))

data1=datam%*%array(1,c(1,n)); data2=array(1,c(n,1))%*%t(datam)
avgs=(data1+data2)/2;
a=c()

for (i in 1:n)
{
  for (j in 1:i)
  {
     a=c(a,avgs[i,j])
  }
}

est=median(a)
return(est)

}  
