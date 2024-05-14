lambda <- 3
N <- 50000
u <- rexp(N, rate = lambda)
f_values <- exp(-2 * u^2)
estimated_value <- mean(f_values)
print(paste("Valoarea estimata:", estimated_value))
exact_value <- sqrt(pi / 8)
print(paste("Valoarea exacta:", exact_value))
relative_error <- abs(estimated_value - exact_value) / exact_value
print(paste("Eroarea relativa:", relative_error))
