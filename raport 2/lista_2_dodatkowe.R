library(tidyr)
library(dplyr)
library(xtable)
library(energy)

###########################################################################################
# Zadanie 1 *
# Napisz funkcje, która dla dwóch wektorów danych oblicza wartosc poziomu
# krytycznego (p-value) w tescie opartym na korelacji odległosci. Nastepnie dla 
# wygenerowanych danych zweryfikuj hipoteze o niezaleznosci przy uzyciu napisanej funkcji. 

# Obliczanie macierzy odległości
compute_distance_matrix <- function(v) {
  n <- length(v)
  dist_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      dist_matrix[i, j] <- abs(v[i] - v[j])
    }
  }
  return(dist_matrix)
}

# Średnie centrowanie macierzy
center_distance_matrix <- function(dm) {
  n <- nrow(dm)
  row_means <- colMeans(dm)
  col_means <- rowMeans(dm)
  grand_mean <- mean(dm)
  
  for (i in 1:n) {
    for (j in 1:n) {
      dm[i, j] <- dm[i, j] - row_means[i] - col_means[j] + grand_mean
    }
  }
  return(dm)
}

# Obliczanie korelacji odległości
calculate_distance_correlation <- function(X, Y) {
  n <- length(X)
  A <- center_distance_matrix(compute_distance_matrix(X))
  B <- center_distance_matrix(compute_distance_matrix(Y))
  
  A_B <- sum(A * B) / n^2
  A_A <- sum(A * A) / n^2
  B_B <- sum(B * B) / n^2
  
  result <- sqrt(A_B / sqrt(A_A * B_B))
  return(result)
}

# Funkcja testująca 
distance_correlation_test <- function(x, y, B = 1000) {
  observed_dcor <- calculate_distance_correlation(x, y)
  n <- length(x)
  permuted_dcor <- numeric(B)
  
  for (i in 1:B) {
    permuted_dcor[i] <- calculate_distance_correlation(x, sample(y))
  }
  
  p_value <- mean(abs(permuted_dcor) >= abs(observed_dcor))
  return(list(statistic = observed_dcor, p_value = p_value))
}

# Przykładowe dane
set.seed(123)
alpha <- 0.05
x <- rnorm(100)
y <- rnorm(100)

# Test
result <- distance_correlation_test(x, y)

# Wyniki testu z biblioteki energy
dcor_built <- dcor(x, y)
dcor_result <- dcor.test(x, y, R = 1000)

# Tworzenie tabeli z wynikami
results_table <- data.frame(
  Metoda = c("Własna", "Wbudowana", "Różnica"),
  d_korelacja = c(result$statistic, dcor_built, abs(dcor_built - result$statistic)),
  `p-wartość` = c(result$p_value, dcor_result$p.value, abs(dcor_result$p.value - result$p_value)),
  Odrzucenie_H0 = c(result$p_value < alpha, dcor_result$p.value < alpha, NA)
)

print(results_table)

xtable(results_table)
