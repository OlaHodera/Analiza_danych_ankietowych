library(ggplot2)
library(tidyr)
library(dplyr)
library(xtable)
library(binom)
library(exact2x2)
library(gnm)


data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))

colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

data <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                    labels = c("0-35", "36-45", "46-55", "56+")))

data$CZY_ZADOW <- ifelse(data$PYT_2 %in% c("-2", "-1"), "NIE", "TAK")

data$CZY_ZADOW_2 <- ifelse(data$PYT_3 %in% c("-2", "-1"), "NIE", "TAK")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ I oraz II @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 1.
# Napisz funkcje, która zwraca p-wartosc w omówionym na wykładzie warunkowym
# tescie symetrii dla tabel 2×2.

symetric_test <- function(table) {
  y_1_2 <- table[1, 2]
  y_2_1 <- table[2, 1]
  
  n_star <- y_1_2 + y_2_1
  
  y_L <- min(y_1_2, n_star - y_1_2)
  y_P <- max(y_1_2, n_star - y_1_2)
  
  if (y_L == y_P) {
    return(1)
  }
  
  else {
  sum1 <- sum(sapply(0:y_L, function(i) dbinom(i, n_star, 1/2)))
  sum2 <- sum(sapply(y_P:n_star, function(i) dbinom(i, n_star, 1/2)))
  
  p_value <- sum1 + sum2
  
  return(p_value)
  }
}

example <- matrix(c(10, 15, 20, 5), nrow = 2, byrow = TRUE)

mcNemar <- mcnemar.exact(example)
symetric_p_val <- symetric_test(example)



result <- data.frame(
  "test" = c("McNemar z poprawka", "test warunkowy"),
  "p-wartość" = c(mcNemar$p.value, symetric_p_val)
)
result

xtable(result)

###############################################################################################################
# Zadanie 2.
# W tabeli 1 umieszczono dane dotyczace reakcji na lek po godzinie od jego
# przyjecia dla dwóch róznych leków przeciwbólowych stosowanych w migrenie. 
# Leki zostały zaaplikowane grupie pacjentów w dwóch róznych atakach bólowych.
# Na podstawie danych zweryfikuj hipoteze, ze leki te sa jednakowo skuteczne
# korzystajac z testu:
# • McNemara z poprawka na ciagłosc,
# • warunkowego (korzystajac z funkcji zadeklarowanej w zadaniu 1.).


# tabela z zadania
medicine_data <- matrix(c(1, 5, 2, 4), nrow = 2, byrow = TRUE)

rownames(medicine_data) <- c("Negatywna", "Pozytywna") # lek A
colnames(medicine_data) <- c("Negatywna", "Pozytywna") # lek B

reaction_table <- as.table(medicine_data)
print(reaction_table)

#_____________________________________________________________________________________________________________
# • test McNemara z poprawka na ciagłosc,
mcNemar <- mcnemar.exact(medicine_data) # mamy na starcie poziom ufnosci 0.95
cat("• p-wartosc dla testu McNemara:", mcNemar$p.value , "\n")


#_____________________________________________________________________________________________________________
# • test warunkowy (funkcja zadeklarowanej w zadaniu 1).
symetric_p_val <- symetric_test(medicine_data)
cat("• p-wartosc dla testu symetrii 2x2:", symetric_p_val  , "\n")

# widzimy ze p-val > 0.0 (w obu przypadkach)5 wiec nie mamy podstaw do odrzucenia H_0 o tym, że
# leki są jednakowo skuteczne

result <- data.frame(
  "test" = c("McNemar z poprawka", "test warunkowy"),
  "p-wartość" = c(mcNemar$p.value, symetric_p_val)
)
result

xtable(result)

###############################################################################################################
# Zadanie 3.
# Przeprowadz symulacje w celu porównania mocy testu Z i testu Z0 przedstawionych
# na wykładzie. Rozważ różne długości prób.


# Funkcja tworząca tablicę
table_making <- function(p_1, p_2, n) {
  X <- rbinom(n, 1, p_1)
  Y <- rbinom(n, 1, p_2)
  
  y <- table( factor(X, levels = c("0", "1")), 
              factor(Y, levels = c("0", "1")))
  
  y <- rbind(y, colSums(y))
  y <- cbind(y, rowSums(y))
  
  return(y)
}



# Funkcja przeprowadzająca testy Z i Z0
Z_Z_0_tests <- function(y) {
  nrows <- dim(y)[1]
  ncols <- dim(y)[2]
   
  n <- y[nrows, ncols]
  
  p <- y/n
  
  D <- (y[1, 2] - y[2, 1])/n
  
  # test Z
  p_1_plus <- p[1, ncols]
  p_plus_1 <- p[nrows, 1]
  sigma2 <- (p_1_plus*(1 - p_1_plus) +
             p_plus_1*(1 - p_plus_1) -
             2*(p[1, 1]*p[2, 2] - p[1, 2]*p[2, 1]) ) / n
  
  Z <- D/sqrt(sigma2)
  p_val <- 2*(1 - pnorm(abs(Z)))
  
  # test Z_0
  Z_0 <- (y[1, 2] - y[2, 1])/sqrt(y[1, 2] + y[2, 1])
  p_val_0 <- 2*(1 - pnorm(abs(Z_0)))
  
  return(list(Z = Z, p_val_Z = p_val, Z_0 = Z_0, p_val_Z_0 = p_val_0))
}

# Funkcja porównująca moc testów
compare_power <- function(p_1, p_2, n, alpha = 0.05, N = 1000) {
  power_Z <- numeric(length(p_2))  
  power_Z_0 <- numeric(length(p_2)) 
  
  for (i in 1:length(p_2)) {
    count_Z <- 0
    count_Z_0 <- 0
    
    for (k in 1:N) {
      table <- table_making(p_1, p_2[i], n)
      Z_Z_0 <- Z_Z_0_tests(table)
      p_val <- Z_Z_0$p_val_Z
      p_val_0 <- Z_Z_0$p_val_Z_0
      
      if (p_val < alpha) {
        count_Z <- count_Z + 1
      }
      if (p_val_0 < alpha) {
        count_Z_0 <- count_Z_0 + 1
      }
    }
    power_Z[i] <- count_Z / N
    power_Z_0[i] <- count_Z_0 / N
  }
  return(data.frame(p_2 = p_2, power_Z = power_Z, power_Z_0 = power_Z_0))
}

# Parametry
p_1 <- 0.5
p_2 <- seq(0.01, 0.99,  by=0.01)
n <- c(20, 50, 100, 1000)


for (i in 1:length(n)) {
  results <- compare_power(p_1, p_2, n[i])
  results$n <- n[i]
  
  p <- ggplot(results, aes(x = p_2)) +
    geom_line(aes(y = power_Z, color = "Moc testu Z")) +
    geom_line(aes(y = power_Z_0, color = "Moc testu Z0"), linetype = "dashed") +  
    labs(title = paste("Porównanie mocy testów dla n =", n[i]),
         x = "p_2",
         y = "Moc",
         color = "Test") +
    theme_minimal() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 16, hjust = 0.5)) 
  
  ggsave(paste("porownanie_mocy_testow_n", n[i], ".png", sep = ""), plot = p, width = 9, height = 5)
  
}


###############################################################################################################
# Zadanie 4.
# Dla danych dołaczonych do pierwszej listy zadan, na podstawie zmiennych
# CZY_ZADW oraz CZY_ZADW_2, zweryfikuj hipoteze, ze zadowolenie z wynagrodzenia w 
# pierwszym badanym okresie i po roku od pierwszego badania odpowiada modelowi symetrii.
# Czy na podstawie uzyskanych wników mozemy wnioskowac, ze poziom zadowolenia z 
# wynagrodzenia nie uległ zmianie? Przyjmij poziom istotnosci 0.05.

# alpha <- 0.05

table_content <- table(data$CZY_ZADOW, data$CZY_ZADOW_2)
# table_content
xtable(table_content)

# testy
mcNemar_test_with <- mcnemar.test(table_content, correct = TRUE)
mcNemar_test_without <- mcnemar.test(table_content, correct = FALSE)

sym_test <- symetric_test(table_content)

table_content <- rbind(table_content, colSums(table_content))
table_content <- cbind(table_content, rowSums(table_content))
Z_Z_0_test <- Z_Z_0_tests(table_content)

# wyniki
result <- data.frame(
  "test" = c("McNemar z poprawka", "McNemar bez poprawki", "Warunkowy", "Z", "Z_0"),
  "p-wartość" = c(mcNemar_test_with$p.value, mcNemar_test_without$p.value, sym_test, Z_Z_0_test$p_val_Z, Z_Z_0_test$p_val_Z_0)
)
result

xtable(result)
# raczej ulegl zmianie, bo p-val < 0.05

###############################################################################################################
# Zadanie 5.
# W korporacji, o której mowa w zadaniu 1 z listy 1, wdrozono pewne
# działania w celu poprawy komfortu pracy. Nastepnie badana grupe respondentów ponownie
# poproszono o odpowiedz na pytanie dotyczace oceny podejscia firmy do utrzymania równowagi
# miedzy zyciem zawodowym a prywatnym. W Tabeli 2 przedstawiono tablice dwudzielcza
# uwzgledniajac, a odpowiedzi na pytanie w obu tych okresach. Na podstawie danych zweryfikuj
# hipoteze, ze odpowiedzi w pierwszym badanym okresie i w drugim okresie odpowiadaja
# modelowi symetrii. Na podstawie wyników uwzyskanych przy weryfikacji hipotezy dotyczacej
# symetrii, sformułuj wniosek dotyczacy hipotezy, ze ocena podejscia firmy nie uległa zmianie. 

# alpha <- 0.05
# test bowkera to test mcnemara ale dla tabel wiekszych niz 2x2!

count <- c(10, 2, 1, 1, 0,
           0, 15, 1, 1, 0,
           1, 1, 32, 6, 0,
           0, 0, 1, 96, 3,
           1, 1, 0, 1, 26)

label <- c('-2', '-1', '0', '1', '2')

# test bowkera
quest_table <- matrix(count, nrow = 5, byrow = TRUE)

rownames(quest_table) <- label
colnames(quest_table) <- label

mcNemar_test_with <- mcnemar.test(quest_table, correct = TRUE)
mcNemar_test_without <- mcnemar.test(quest_table, correct = FALSE)
mcNemar_test_with$p.value
mcNemar_test_without$p.value
# NIE DZIALA I TAK MA BYC

# warto wyswietlic ze mamy df = 10
mcNemar_test_with 
mcNemar_test_without

# na podstawie przykładu z wykładu (najwiekszej wiarygodnosci)
quest_1 <- gl(5, 5, labels=label)
quest_2 <- gl(5, 1, labels=label)

comfort_data <- data.frame(quest_1, quest_2, count)

symmetry <- glm(count ~ Symm(quest_1, quest_2), data=comfort_data, family=poisson)

summary(symmetry)

x <- symmetry$deviance # odchylenie
r <- 10 # liczba stopni swobody
p_val_IW <- 1 - pchisq(x, r)



result <- data.frame(
  "test" = c("Bowker z poprawka", "Bowker bez poprawki", "IW"),
  "p-wartość" = c(mcNemar_test_with$p.value, mcNemar_test_without$p.value, p_val_IW)
)
result

xtable(result)
# WNIOSEK: p-val z IW > 0.05 wiec nie doszlo do zmiany

# gdy dodamy 0.00000001 cos dziala ale wynik jest raczej niepoprawny i to mocno
mcNemar_test_with <- mcnemar.test(quest_table + 0.00000001, correct = TRUE)
mcNemar_test_without <- mcnemar.test(quest_table + 0.00000001, correct = FALSE)
mcNemar_test_with$p.value
mcNemar_test_without$p.value

# warto wyswietlic ze df = 10
mcNemar_test_with 
mcNemar_test_without

result <- data.frame(
  "test" = c("Bowker z poprawka", "Bowker bez poprawki", "IW"),
  "p-wartość" = c(mcNemar_test_with$p.value, mcNemar_test_without$p.value, p_val_IW)
)
result 

xtable(result)

# tutaj dla testow bowkera wychodzi nam duze p-val > 0.05 wiec tu nie odrzucamy H_0
# raczej to jest niedokładne

# IW jest bardziej dokladny i niezawodny 

