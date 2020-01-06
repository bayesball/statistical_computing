simple.iteration <- function(g1, x0, iterations, delta=0.1){
  current <- x0
  stored.values <- vector(length = iterations + 1)
  stored.values[1] <- x0
  for (j in 1:iterations){
    xnew <- current + g1(current)
    stored.values[j + 1] <- xnew 
    if(abs(xnew - current) < delta) break
    current <- xnew               
  }
  return(data.frame(Iteration=1:(j + 1), 
                    Value=stored.values[1:(j + 1)]))
}