library(pracma)

mc_vol_torus = function(R, r, n) {
  vol_bbox = (2 * (R + r))^2 * (2 * r)
  x1 = runif(n, -R-r, R+r)
  x2 = runif(n, -R-r, R+r)
  x3 = runif(n, -r, r)
  inside = (x3^2 + (sqrt(x1^2 + x2^2) - R)^2) < r^2
  vol_est = vol_bbox * sum(inside) / n
  return(vol_est)
}

R = 10
r = 3
esantioane = c(10000, 20000, 50000)
V_exact = 180 * pi^2

rezultate_b1 = data.frame(Numar_Esantioane=integer(), Vol_Estim=double(), Eroare_Rel=double())

for (n in esantioane) {
  est = mc_vol_torus(R, r, n)
  eroare_rel = abs((est - V_exact) / V_exact)
  rezultate_b1 = rbind(rezultate_b1, data.frame(Numar_Esantioane=n, Vol_Estim=est, Eroare_Rel=eroare_rel))
}

print(rezultate_b1)

inside_triangle = function(x, y) {
  return(y >= 0 & y <= 2*x & y <= 6 - 3*x)
}

mc_aria_triunghi = function(n) {
  aria_bbox = 2 * 6
  x = runif(n, 0, 2)
  y = runif(n, 0, 6)
  inside = mapply(inside_triangle, x, y)
  aria_est = aria_bbox * sum(inside) / n
  return(aria_est)
}

n_esantioane = 20000
aria_estimata = mc_aria_triunghi(n_esantioane)
print(aria_estimata)

int_a = integral(function(x) (2*x - 1) / (x^2 - x - 6), -1, 1)
int_b = integral(function(x) (x + 4) / (x^(1/3) - 3^(1/3)), 3, 11)
int_c = integral(function(x) x * exp(-x^2), 0, Inf)

valori_exacte = c(log(3) - log(2), 61.2, 0.5)
estimari_numerice = c(int_a, int_b, int_c)
erori_relative = abs((estimari_numerice - valori_exacte) / valori_exacte)

rezultate_integrale = data.frame(
  Integrala = c("(a)", "(b)", "(c)"),
  Est_Numerica = estimari_numerice,
  Valoare_Exacta = valori_exacte,
  Eroare_Relativa = erori_relative
)

print(rezultate_integrale)

sim_crestere_retea = function(u_init, n, p, q, u_tinta, max_ani=100) {
  ani = 0
  u = u_init
  numar_utilizatori = c(u)
  
  while (u < u_tinta & ani < max_ani) {
    u_nou = rbinom(1, n, p)
    u_pleaca = rbinom(1, u, q)
    u = u + u_nou - u_pleaca
    numar_utilizatori = c(numar_utilizatori, u)
    ani = ani + 1
  }
  
  return(list(ani, numar_utilizatori))
}

u_init = 10000
n = 1000
p = 0.25
q = 0.01
u_tinta = 15000
max_ani = 100
num_sim = 1000

ani_pana_tinta = c()
numar_utilizatori_timp = list()

for (i in 1:num_sim) {
  res = sim_crestere_retea(u_init, n, p, q, u_tinta, max_ani)
  ani_pana_tinta = c(ani_pana_tinta, res[[1]])
  numar_utilizatori_timp[[i]] = res[[2]]
}

medie_ani_pana_tinta = mean(ani_pana_tinta)

ani_prag = 40.83
utilizatori_dupa_prag = sapply(numar_utilizatori_timp, function(cnts) ifelse(length(cnts) >= ani_prag, cnts[ani_prag], NA))
probabilitate_peste_prag = mean(utilizatori_dupa_prag >= u_tinta, na.rm=TRUE)

rezultate_b4 = data.frame(
  Medie_Ani_pana_Tinta = medie_ani_pana_tinta,
  Prob_Dupa_40_83_Ani = probabilitate_peste_prag
)

print(rezultate_b4)
