# simple illustration of a density estimate with a
# normal kernel with bandwidth h

cskern1d <- function(x, data, h){
   n <- length(data)
   fhat <- 0 * x 
   for (i in 1:n){
	    fhat <- fhat + dnorm(x, data[i], h) / n
   }
  return(fhat)
}

# use sliders to demonstrate the effect of changing bandwidth

library(manipulate)
library(MASS)             # this package contains geyser data
x <- seq(30, 120, length=500)
manipulate({plot(x, cskern1d(x, geyser$waiting, h), type="l", 
              main=paste("Kernel estimate with h = ", h));
            rug(geyser$waiting)},
           h = slider(.1, 20))

# illustrate use of density function

d.normal <- density(geyser$waiting)
plot(d.normal)

# illustrate other kernels

d.rect <- density(geyser$waiting, kernel="rectangular")

d.triang <- density(geyser$waiting, kernel="triangular")

plot(d.normal, main="Three Choices for Kernel",
     xlab="")
lines(d.rect, col="red")
lines(d.triang, col="blue")
legend("topright", legend=c("normal", "rectangular", "triangular"),
       col=c("black", "red", "blue"), lty=1)
