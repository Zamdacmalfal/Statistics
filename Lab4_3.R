lambda_primul <- 4
lambda_al_doilea <- 12
prob_primul <- 3/4
prob_al_doilea <- 1 - prob_primul
numar_clienti <- 10000

decide_mecanic <- function() 
  {
  if (runif(1) <= prob_primul)
    return("primul")
  else
    return("al doilea")
  }
genereaza_timp_servire <- function(mecanic) 
  {
  if (mecanic == "primul") 
    return(rexp(1, rate = lambda_primul))
  else
    return(rexp(1, rate = lambda_al_doilea))
  }
timp_servire_total <- 0
for (i in 1:numar_clienti) 
  {
  mecanic <- decide_mecanic()
  timp_servire <- genereaza_timp_servire(mecanic)
  timp_servire_total <- timp_servire_total + timp_servire
  }
media_timp_servire <- timp_servire_total / numar_clienti
print(paste("Media timp de servire:", media_timp_servire))
