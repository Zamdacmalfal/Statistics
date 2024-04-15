ex9 = function(lambda, n)
{
  x=0:n
  probs=dpois(x, lambda)
  
  for(i in 0:n)
  {
    cat("P(X=",i,") =", probs[i+1], "\n")
  }
  
  plot(x, probs, type="l")
}