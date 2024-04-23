
life_expect <- read.csv("life_expect.csv", header = TRUE)
par(mfrow=c(1,2))  
hist(life_expect$female, breaks = 7, main = "Histograma Speranței de Viață - Femei", xlab = "Speranța de Viață", ylab = "Frecvență")
hist(life_expect$male, breaks = 7, main = "Histograma Speranței de Viață - Bărbați", xlab = "Speranța de Viață", ylab = "Frecvență")
