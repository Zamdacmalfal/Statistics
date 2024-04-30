test_TLC <- function(N, z, alpha, lambda) {
  n <- 50
  
  means <- rep(0, N)
  std_devs <- rep(0, N)
  
  for (i in 1:N) {
    X <- rgamma(n, shape = alpha, rate = lambda)
    means[i] <- mean(X)
    std_devs[i] <- sd(X)
  }
  
  ks_test <- ks.test((means - alpha/lambda) / (std_devs/sqrt(n)), "pnorm", mean = 0, sd = 1)
  
  cat(paste("Pentru N =", N, "și z =", z, ":"))
  if (ks_test$p.value < 0.05) {
    cat("\nTLC nu este verificată.")
  } else {
    cat("\nTLC este verificată.")
  }
  cat("\n")
}
N_values <- c(5000, 10000, 20000)
z_values <- c(-1.5, 0, 1.5)
alpha <- 2
lambda <- 3

for (N in N_values) {
  for (z in z_values) {
    test_TLC(N, z, alpha, lambda)
  }
}
