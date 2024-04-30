test_LNM <- function(n, r) {
  # Generăm n variabile aleatoare Student cu parametrul r
  X <- rt(n, df = r)
  
  # Calculăm media acestor variabile
  mean_X <- mean(X)
  
  # Comparam media cu valoarea așteptată (0)
  if (abs(mean_X) < 1e-3) {
    cat(paste("LNM este verificată pentru n =", n, "și r =", r, ". Media:", mean_X, "\n"))
  } else {
    cat(paste("LNM nu este verificată pentru n =", n, "și r =", r, ". Media:", mean_X, "\n"))
  }
}

# Parametrii pentru n și r
n_values <- c(1000, 10000, 100000, 1000000)
r_values <- c(2, 3, 4, 5)

# Testăm LNM pentru fiecare combinație de n și r
for (n in n_values) {
  for (r in r_values) {
    test_LNM(n, r)
  }
}
