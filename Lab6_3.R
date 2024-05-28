n_total <- 150
n_defecte <- 20
procent_defecte <- n_defecte / n_total
procent_nul <- 0.1
z <- (procent_defecte - procent_nul) / sqrt(procent_nul * (1 - procent_nul) / n_total)
valoare_p <- 1 - pnorm(z)
cat("Statistica de test (z):", z, "\n")
cat("Valoare p:", valoare_p, "\n")
nivel_sem = 0.05
if (valoare_p < nivel_sem) {
  cat("Ipoteza nula este respinsa. Avem suficiente dovezi că procentul componentelor defecte > 10%.\n")
} else {
  cat("Nu avem suficiente dovezi că procentul componentelor defecte este > 10%.\n")
}
