f = function(x) {
  return(-2*x^2 + 5*x - 2)
}

estimate_integral_uniform_sampling <- function(n) {
  total_area <- 0
  width <- 2 / n
  for (i in 0:(n-1)) {
    x_left <- i * width
    x_right <- (i + 1) * width
    height <- f((x_left + x_right) / 2) 
    area <- height * width
    total_area <- total_area + area
  }
  return(total_area)
}

n <- 10000

exact_area <- integrate(f, 0, 2)$value
print(paste("Aria exacta:", exact_area))

relative_error <- abs(estimated_area - exact_area) / exact_area
print(paste("Errea Relativa:", relative_error))
