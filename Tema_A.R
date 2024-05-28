library(ggplot2)

calc_prob = function(lam, p, n, m, k) {
  x = seq(k, m)
  list(
    poisson = dpois(x, lam),
    geom = dgeom(x, p),
    binom = dbinom(x, n, p)
  )
}

plot_dist = function(prob, k, m) {
  x = seq(k, m)
  data = data.frame(
    x = rep(x, times = 3),
    prob = c(prob$poisson, prob$geom, prob$binom),
    dist = factor(rep(c("Poisson", "Geom", "Binom"), each = length(x)))
  )
  ggplot(data, aes(x, prob, fill = dist)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Funcții de Masă de Probabilitate", x = "k", y = "Probabilitate") +
    theme_minimal()
}

find_k0 = function(lam, prag) {
  k0 = 0
  cum_prob = 0
  while (cum_prob <= prag) {
    cum_prob = ppois(k0, lam)
    if (cum_prob > prag) {
      return(k0)
    }
    k0 = k0 + 1
  }
}

read_data = function(file) {
  data = read.csv(file, header = TRUE)
  freq_p = table(data$P)
  freq_s = table(data$S)
  p_abs = as.vector(freq_p)
  s_abs = as.vector(freq_s)
  p_rel = p_abs / sum(p_abs)
  s_rel = s_abs / sum(s_abs)
  mean_p = mean(data$P)
  mean_s = mean(data$S)
  list(data = data, p_abs = p_abs, s_abs = s_abs, p_rel = p_rel, s_rel = s_rel, mean_p = mean_p, mean_s = mean_s)
}

remove_out = function(data, col) {
  Q1 = quantile(data[[col]], 0.25)
  Q3 = quantile(data[[col]], 0.75)
  IQR = Q3 - Q1
  low = Q1 - 1.5 * IQR
  high = Q3 + 1.5 * IQR
  clean_data = data[data[[col]] >= low & data[[col]] <= high, ]
  clean_data
}

plot_freq = function(data, col) {
  ggplot(data, aes_string(x = col)) +
    geom_histogram(breaks = seq(1, 10, by = 1), col = "black", fill = "blue", alpha = 0.7) +
    labs(title = paste("Distribuția Frecvenței pentru", col), x = "Interval Scor", y = "Frecvență") +
    theme_minimal()
}

file = "note_PS.csv"
res = read_data(file)

clean_p = remove_out(res$data, 'P')
clean_s = remove_out(res$data, 'S')

plot_freq(clean_p, 'P')
plot_freq(clean_s, 'S')
