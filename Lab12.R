simulation<- function(xi, pi) {
  U <- runif(1)
  Current_probofsum <- 0
  
  for (i in seq_along(pi)) {
    Current_probofsum = Current_probofsum + pi[i]
    
    if (U <= Current_probofsum) {
      return(xi[i])
    }
  }
  
  return(xi[length(xi)])
}
xi <- c(1,2,3,4)
pi <- c(0.1,0.2,0.3,0.4)
output <-simulation(xi, pi)
cat("Result:", output, "\n")
