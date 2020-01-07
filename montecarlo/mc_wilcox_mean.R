mc_wilcox_mean=function(iter)
{
# focus on estimating E(W)

n=10 		# sample of size 10

mn=array(0,c(iter,1))

for (i in 1:iter)
{
   u=runif(n); data=-log(u)
   mn[i]=wilcoxon(data)
}

return(c(mean(mn),sd(mn)/sqrt(iter)))
}

