mc_wilcox_anti=function(iter)
{
# focus on estimating E(W)
# using sampling with antithetic random variables

n=10 		# sample of size 10

mn=array(0,c(iter,1))

for (i in 1:iter)
{
   u=runif(n); data1=-log(u); data2=-log(1-u)
   mn[i]=(wilcoxon(data1)+wilcoxon(data2))/2
}

return(c(mean(mn), sd(mn)/sqrt(iter)))
}

