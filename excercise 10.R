ex10 = function(n, p)
{
  probs=dgeom(0:(n-1), prob=p)
  
  for(i in 0:n)
  {
    cat("P(X=",i,") =", probs[i], "\n")
  }
  
  barplot(probs, type="l")
}