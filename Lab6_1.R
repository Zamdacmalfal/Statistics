interval_incredere <- function(fisier, sigma, nivel) {
  date <- read.table(fisier, header = TRUE)
  medie <- mean(date$greutate)
  n <- length(date$greutate)
  
  z <- qnorm(1 - (1 - nivel) / 2)
  eroare <- sigma / sqrt(n)
  margine <- z * eroare
  interval <- c(medie - margine, medie + margine)
  
  return(interval)
}

fisier <- "history.txt"
sigma <- 5
nivel <- 0.95
interval_95 <- interval_incredere(fisier, sigma, nivel)

cat("Intervalul de incredere de 95% pt. media populatiei este:\n")
print(interval_95)
