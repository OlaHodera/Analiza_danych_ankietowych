library(ggplot2)
library(tidyr)
library(dplyr)
library(xtable)
library(reshape2)

data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))

colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

data <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                    labels = c("0-35", "36-45", "46-55", "56+")))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ III @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 7
# Zapoznaj sie z funkcja chisq.test z pakietu stats oraz assocplot z pakietu graphics.
library(stats)
help(chisq.test)

library(graphics)
help(assocplot)


##########################################################################################################
# Zadanie 8
# Korzystajac z chisq.test zweryfikuj hipoteze, ze zadowolenie z wynagrodzenia ˙
# w pierwszym badanym okresie nie zalezy od zajmowanego stanowiska. Przyjmij poziom ˙
# istotnosci 0.01. Stwórz wykres przy pomocy funkcji assocplot i dokonaj jego interpretacji.
# Wynik testu porównaj z wynikiem uzyskanym w zadaniu 6.

alpha <- c(0.01, 0.05)
table <- table(data$PYT_2, data$CZY_KIER)

test_chi2_p_val_correct <- chisq.test(table, correct=TRUE)$p.val # z poprawką (TU POPRAWKA NIE DZIALA)
test_chi2_p_val_no_correct <- chisq.test(table, correct=FALSE)$p.val # bez poprawką


for (i in 1:length(alpha)) {
  cat("• alpha = ", alpha[i], ", test chi2 z poprawką - ", ifelse(test_chi2_p_val_correct > alpha[i], "Nie zależy", "Zależy"), ", bo p-value =", test_chi2_p_val_correct, "\n")
  cat("• alpha = ", alpha[i], ", test chi2 bez poprawki - ", ifelse(test_chi2_p_val_no_correct > alpha[i], "Nie zależy", "Zależy"), ", bo p-value =", test_chi2_p_val_no_correct, "\n")
  
  
  # z zadania 6 (do porównania)
  fisher_test_3 <- fisher.test(table, conf.level = 1 - alpha[i])$p.value
  cat("• alpha = ", alpha[i], ", fisher test - ", ifelse(fisher_test_3 > alpha[i], "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_3, "\n\n")
  
}

assocplot(table, main="Zadowolenie z wynagrodzenia w zależności od stanowiska",
          xlab="Zadowolenie z wynagrodzenia", ylab="Stanowisko",
          col=c("skyblue", "pink"))

# źródło: https://www.geeksforgeeks.org/chi-square-test-in-r/


# Otrzymalismy troche inne p wartosci, ale ostatecznie wyszlo nam w obu przypadkach, że zalezy


##########################################################################################################
# Zadanie 9
# Zapoznaj sie z funkcja rmultinom z pakietu stats, a nastepnie korzystajac z niej
# przeprowadz symulacje w celu oszacowania mocy testu Fishera oraz mocy testu chi-kwadrat
# Pearsona, generujac dane z tabeli 2 × 2, w której p11 = 1/40, p12 = 3/40, p21 = 19/40,
# p22 = 17/40. Symulacje wykonaj dla n = 50, n = 100 oraz n = 1000.

# zapoznanie 
help(rmultinom)

set.seed(123)

p <- c(1/40, 3/40, 19/40, 17/40)

alpha <- c(0.01, 0.05)


test_power <- function(p, n, alpha, N = 500) {
  fisher_count <- 0
  chi2_count_correct <- 0
  chi2_count_no_correct <- 0
  
  for (i in 1:N) {
    X <- rmultinom(1, n, p)
    
    while (all(X == 0)) {
      X <- rmultinom(1, n, p)
    }
    
    fisher_p_val <- fisher.test(matrix(X, nrow = 2))$p.val
    chi2_p_val_correct <- chisq.test(matrix(X+0.00000001, nrow = 2), correct = TRUE)$p.val
    chi2_p_val_no_correct <- chisq.test(matrix(X+0.00000001, nrow = 2), correct = FALSE)$p.val
    
    if (fisher_p_val < alpha) {
      fisher_count <- fisher_count + 1
    }
    if (chi2_p_val_correct < alpha) { 
      chi2_count_correct <- chi2_count_correct + 1
    }
    if (chi2_p_val_no_correct < alpha) { 
      chi2_count_no_correct <- chi2_count_no_correct + 1
    }
  }
  
  return(c(fisher_count / N, chi2_count_correct / N, chi2_count_no_correct / N))
}


for (i in 1:length(alpha)) {
  results <- data.frame(
    test = c("Fisher", "Chi-2 z poprawką", "Chi-2 bez poprawki"),
    power_n_50 = test_power(p, 50, alpha[i]),
    power_n_100 = test_power(p, 100, alpha[i]),
    power_n_1000 = test_power(p, 1000, alpha[i])
  )
  
  print(results)
  cat("\n\n")
  
  
  table <- xtable(results)
  print(table)
  cat("\n\n")
}


##########################################################################################################
# Zadanie 10
# Napisz funkcje, która dla danych z tablicy dwudzielczej oblicza wartosc poziomu ´
# krytycznego w tescie niezaleznosci opartym na ilorazie wiarogodnosci. Korzystajac z napisanej ˙
# funkcji, wykonaj test dla danych z zadania 8.

alpha <- c(0.01, 0.05)
table <- table(data$PYT_2, data$CZY_KIER)

# dwudzielcza
table <- rbind(table, colSums(table))
table <- cbind(table, rowSums(table))
print(table)

xtable(table)

likehood_test <- function(x) {
  R <- dim(x)[1]
  C <- dim(x)[2]

  df <- (R - 2)*(C - 2) # stopnie swobody

  n <- x[R, C]
  
  lambda_part <- matrix(0, nrow = R - 1 , ncol = C - 1)
  for (i in 1:(R - 1)) {
    for (j in 1:(C - 1)) {
      n_i_plus <- x[i, C]
      n_plus_j <- x[R, j]
      n_i_j <- x[i, j]
      
      lambda_part[i, j] <-  ((n_i_plus * n_plus_j) / (n * n_i_j)) ^ n_i_j
    }
  }
  lambda <- prod(lambda_part)
  G2 <- -2 * log(lambda)
  
  p_val <- 1 - pchisq(G2, df = df)
  
  return(data.frame(G2 = G2, p_val = p_val))
}



result <- likehood_test(table)
print(result)
# print(xtable(result)) 


cat("\nWyniki:")
for (i in 1:length(alpha)) {
  cat('alpha =', alpha[i], ':', ifelse(result[1, 2] > alpha[i], "Niezalezne", "Zalezne"), ", p-wartość = ", result[1, 2], '\n')
}





