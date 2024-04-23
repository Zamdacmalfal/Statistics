outliers.mean <- function(esantion) {
  M <- mean(esantion)
  s <- sd(esantion)
  lower_bound <- M - 2*s
  upper_bound <- M + 2*s
  outliers <- esantion[esantion < lower_bound | esantion > upper_bound]
  return(outliers)
}

outliers.mean(c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17))


outliers.iqr <- function(esantion) {
  
  Q1 <- quantile(esantion, 0.25)
  Q3 <- quantile(esantion, 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- esantion[esantion < lower_bound | esantion > upper_bound]
  
  return(outliers)
}

outliers.iqr(c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17))