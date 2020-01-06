newton2=function(g1,g2,x,nsteps,data)
{

for (i in 1:nsteps)
{
h=-solve(g2(x,data))
x=x+h%*%g1(x,data)
}

return(x)

}