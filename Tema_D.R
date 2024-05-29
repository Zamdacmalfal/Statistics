probabilitati <- read.csv("probabilitati.csv", stringsAsFactors = FALSE)
medie <- mean(probabilitati$probabilitati)
dispersie <- 92.16
n <- length(probabilitati$probabilitati)
interval_95 <- c(medie - 1.96 * sqrt(dispersie / n), medie + 1.96 * sqrt(dispersie / n))
interval_99 <- c(medie - 2.576 * sqrt(dispersie / n), medie + 2.576 * sqrt(dispersie / n))
cat("Intervalul de încredere de 95% pentru punctajul mediu:", interval_95, "\n")
cat("Intervalul de încredere de 99% pentru punctajul mediu:", interval_99, "\n")


statistica <- read.csv("statistica.csv", header = FALSE, stringsAsFactors = FALSE, skip = 1)
colnames(statistica) <- c("Punctaje")
media <- mean(statistica$Punctaje)
deviatia_standard <- sd(statistica$Punctaje)
n <- length(statistica$Punctaje)
z_95 <- 1.96
z_99 <- 2.576

interval_95 <- c(media - z_95 * (deviatia_standard / sqrt(n)), media + z_95 * (deviatia_standard / sqrt(n)))
interval_99 <- c(media - z_99 * (deviatia_standard / sqrt(n)), media + z_99 * (deviatia_standard / sqrt(n)))

cat("Intervalul de încredere de 95% pentru punctajul mediu:", interval_95, "\n")
cat("Intervalul de încredere de 99% pentru punctajul mediu:", interval_99, "\n")


n <- 100
x <- 14
p0 <- 0.10
medie <- x / n
z <- (medie - p0) / sqrt(p0 * (1 - p0) / n)
#Val.critice
z_crit_95 <- qnorm(0.95)
z_crit_99 <- qnorm(0.99)

if (z > z_crit_95) {
  cat("Ip. nulă e respinsă la 5% nivel de semnificație.\n")
} else {
  cat("Ip. nulă nu poate fi respinsă la 5% nivel de semnificație.\n")
}

if (z > z_crit_99) {
  cat("Ip. nulă e respinsă la 1% nivel de semnificație.\n")
} else {
  cat("Ip. nulă nu e respinsă la 1% nivel de semnificație.\n")
}
