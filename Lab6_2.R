interval_incredere <- function(fisier, nivel_incredere) {
  date <- read.table(fisier, header = TRUE)
  media <- mean(date$zahar)
  deviatie_std <- sd(date$zahar)
  n <- length(date$zahar)
  if (nivel_incredere == 0.99) {
    z <- qnorm(1 - (1 - nivel_incredere) / 2)
  } else if (nivel_incredere == 0.95) {
    z <- qnorm(1 - (1 - nivel_incredere) / 2)
  } else {
    stop("Nivelul de incredere trebuie sa fie 0.99 sau 0.95")
  }
  margine <- z * deviatie_std / sqrt(n)
  interval <- c(media - margine, media + margine)
  return(interval)
}
fisier <- "history.txt"
interval_95 <- interval_incredere(fisier, 0.95)
interval_99 <- interval_incredere(fisier, 0.99)
cat("Intervalul de incredere de 95% pentru media nivelului de zahar:", interval_95, "\n")
cat("Intervalul de incredere de 99% pentru media nivelului de zahar:", interval_99, "\n")
