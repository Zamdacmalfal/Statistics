test_LNM <- function(n, r) {
  X <- rt(n, df = r)
  mean_X <- mean(X)
  if (abs(mean_X) < 1e-3) {
    cat(paste("LNM este verificată pentru n =", n, "și r =", r, ". Media:", mean_X, "\n"))
  } else {
    cat(paste("LNM nu este verificată pentru n =", n, "și r =", r, ". Media:", mean_X, "\n"))
  }
}
n_values <- c(1000, 10000, 100000, 1000000)
r_values <- c(2, 3, 4, 5)
for (n in n_values) {
  for (r in r_values) {
    test_LNM(n, r)
  }
}
