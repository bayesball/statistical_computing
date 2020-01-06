newton1=function(f,f1,x,n)
{
output=c()
for (i in 1:n)
{
  x=x-f(x)/f1(x)
  output=rbind(output,c(i,x))
}
return(output)
}