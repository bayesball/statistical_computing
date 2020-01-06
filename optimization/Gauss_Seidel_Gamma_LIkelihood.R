## --------------------------------------------------------
glikeA <- function(alpha, par) {
  # like of [alpha | beta]
  s <- par$data[1];
  logp <- par$data[2]
  n <- par$data[3]
  beta <- par$beta
  -n * alpha * log(beta) + alpha * logp - s / beta - n * lgamma(alpha)
}
glikeB <- function(beta, par){
  # like of [beta | alpha]
  s <- par$data[1]
  logp <- par$data[2]
  n <- par$data[3]
  alpha <- par$alpha
  -n * alpha * log(beta) + alpha * logp - s / beta - n * lgamma(alpha)
}


## --------------------------------------------------------
gammalikeGS <- function(start, iter, data){
  # uses functions glikeA and glikeB
  require(LearnBayes)
  alpha <- start[1]
  beta <- start[2]
  output <- matrix(0, iter + 1, 3)
  output[1, ] <- c(0, alpha, beta)
  for (i in 1:iter){
     par <- list(data=data, alpha=alpha)
     beta <- laplace(glikeB, beta, par)$mode

     par <- list(data=data, beta=beta)
     alpha <- laplace(glikeA, alpha, par)$mode

     output[i + 1, ] <- c(i, alpha, beta)
    }
output
}


## --------------------------------------------------------
y <- c(0.95, 0.22, 2.33, 2.42, 0.70, 0.41, 0.49, 
          0.73, 4.54, 1.08, 3.03, 1.85)
data <- c(sum(y), sum(log(y)), length(y))


## --------------------------------------------------------
gammalikeGS(c(1, 2), 10, data)

