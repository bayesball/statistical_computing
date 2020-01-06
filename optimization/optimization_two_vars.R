## --------------------------------------------------------
binreg <- function(beta, data){
  p <- beta[1] + data$x * beta[2]
  sum(data$y * log(p) + (1 - data$y) * log(1 - p))
}


## --------------------------------------------------------
binreg1 <- function(beta, data){
  p <- beta[1] + data$x * beta[2]
  array(c(sum((data$y - p) / (p * (1 - p))),
          sum((data$y - p) / (p * (1 - p)) * data$x)),
          c(2,1))
}


## --------------------------------------------------------
binreg2 <- function(beta, data){
  p <- beta[1] + data$x * beta[2]  
  u <- (p * (1 - p) * (- 1) - (data$y - p) * (1 - 2 * p)) / (p ^ 2 * (1 - p) ^ 2)
  array(c(sum(u), sum(u * data$x), 
          sum(u * data$x), sum(u * data$x ^ 2)),
        c(2, 2))
}


## --------------------------------------------------------
toy.data <- list(x=1:8, y=c(0,1,0,0,1,1,0,1))


## --------------------------------------------------------
newton2 <- function(g1, g2, x, nsteps, data){  
  x <- as.matrix(x, c(2, 1))
  d <- data.frame(Iteration=0, Par1=x[1], Par2=x[2])
  for (i in 1:nsteps){
    h <- -solve(g2(x, data))
    x <- x + h %*% g1(x, data)
    d <- rbind(d, data.frame(Iteration=i, Par1=x[1], Par2=x[2]))
  } 
  return(d) 
}


## --------------------------------------------------------
newton2(binreg1, binreg2, c(.2, 0), 8, toy.data)


## --------------------------------------------------------
glm(y ~ x, data=toy.data, 
    family=binomial("identity"),
    start=c(.2, 0))

