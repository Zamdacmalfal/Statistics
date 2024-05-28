# Generarea permutărilor aleatoare
generare_permutare = function(n) {
  U = runif(n)
  order(U)
}

# Compararea lexicografică a două cuvinte
compara_lexicografic = function(Wi, Wj) {
  Lij = min(nchar(Wi), nchar(Wj))
  for (l in 1:Lij) {
    if (substr(Wi, l, l) < substr(Wj, l, l)) {
      return(TRUE)
    } else if (substr(Wi, l, l) > substr(Wj, l, l)) {
      return(FALSE)
    }
  }
  if (nchar(Wi) < nchar(Wj)) {
    return(TRUE)
  } else if (nchar(Wi) > nchar(Wj)) {
    return(FALSE)
  }
  repeat {
    bit_nou_i = sample(c("0", "1"), 1)
    bit_nou_j = sample(c("0", "1"), 1)
    Wi = paste0(Wi, bit_nou_i)
    Wj = paste0(Wj, bit_nou_j)
    if (bit_nou_i < bit_nou_j) {
      return(TRUE)
    } else if (bit_nou_i > bit_nou_j) {
      return(FALSE)
    }
  }
}

# QuickSort randomizat
quicksort_randomizat = function(cuvinte) {
  if (length(cuvinte) <= 1) {
    return(cuvinte)
  }
  pivot_index = sample(1:length(cuvinte), 1)
  pivot = cuvinte[pivot_index]
  cuvinte = cuvinte[-pivot_index]
  mai_mici = cuvinte[sapply(cuvinte, function(cuvant) compara_lexicografic(cuvant, pivot))]
  mai_mari = cuvinte[!sapply(cuvinte, function(cuvant) compara_lexicografic(cuvant, pivot))]
  return(c(quicksort_randomizat(mai_mici), pivot, quicksort_randomizat(mai_mari)))
}

# Generarea și sortarea cuvintelor binare
generare_permutare_sortata = function(n, k) {
  cuvinte = replicate(n, paste0(sample(c("0", "1"), k, replace = TRUE), collapse = ""))
  cuvinte_sortate = quicksort_randomizat(cuvinte)
  order(cuvinte)[match(cuvinte, cuvinte_sortate)]
}

# Algoritmul pentru determinarea unei tăieturi de cardinal maxim într-un graf
max_taietura = function(G, n) {
  V = unique(c(G[,1], G[,2]))
  A = sample(V, n)
  B = setdiff(V, A)
  taietura = sum((G[,1] %in% A & G[,2] %in% B) | (G[,1] %in% B & G[,2] %in% A))
  return(taietura)
}

# Creșterea șanselor de a găsi o tăietură de cardinal cât mai mare
crestere_max_taietura = function(G, n, num_iterations) {
  max_taietura_valoare = 0
  for (i in 1:num_iterations) {
    taietura_curenta = max_taietura(G, n)
    if (taietura_curenta > max_taietura_valoare) {
      max_taietura_valoare = taietura_curenta
    }
  }
  return(max_taietura_valoare)
}

set.seed(42)

n = 5
print(generare_permutare(n))

k = 4
print(generare_permutare_sortata(n, k))

# Graful de exemplu
G = matrix(c(1,2, 1,3, 2,3, 2,4, 3,5, 4,5), byrow=TRUE, ncol=2)
n = 2
num_iterations = 100
print(crestere_max_taietura(G, n, num_iterations))
