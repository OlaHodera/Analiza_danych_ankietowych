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


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ III @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 6.
# W pewnym badaniu porównywano skutecznosc dwóch metod leczenia: Leczenie A
# to nowa procedura, a Leczenie B to stara procedura. Przeanalizuj dane przedstawione
# w Tabeli 3 (wyniki dla całej grupy pacjentów) oraz w Tabelach 4 i 5 (wyniki w podgrupach 
# ze wzgledu na dodatkowa zmienna) i odpowiedz na pytanie, czy dla danych wystepuje
# paradoks Simpsona.


# Funkcja obliczająca prawdopodobieństwo poprawy
calculate_improvement_probabilities <- function(data) {
  improvement_probabilities <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    improvement_probabilities[i] <- data[i, 2] / sum(data[i, 2:3])
  }
  
  return(improvement_probabilities)
}

# Funkcja wykonująca test asymptotyczny (test proporcji)
perform_prop_test <- function(data, alpha=0.05) {
  # H0 : prawdopodobieństwo pozytywnej reakcji na leczenie metodą A jest większe bądź
  # równe prawdopodobieństwu pozytywnej reakcji na leczenie metodą B
  improvement <- c(data[1, 2], data[2, 2])
  sum_data <- c(sum(data[1, 2:3]), sum(data[2, 2:3]))
  
  prop_with <- prop.test(improvement, 
                         sum_data, 
                         alternative = "less", conf.level = 1 - alpha, correct = TRUE)
  prop_without <- prop.test(improvement, 
                            sum_data, 
                            alternative = "less", conf.level = 1 - alpha, correct = FALSE)
  
  rejection_H0_with <- prop_with$p.value < alpha
  rejection_H0_without <- prop_without$p.value < alpha

  return(list(rejection_H0_with=rejection_H0_with, rejection_H0_without=rejection_H0_without))
}

# Tabela 3
total_data <- data.frame(
  Methoda = c("leczenie A", "leczenie B"),
  Poprawa = c(117, 177),
  Brak = c(104, 44)
)

total_data
# Tabela 4
comorbid_data <- data.frame(
  Methoda = c("leczenie A", "leczenie B"),
  Poprawa = c(17, 2),
  Brak = c(101, 36)
)

# Tabela 5
no_comorbid_data <- data.frame(
  Methoda = c("leczenie A", "leczenie B"),
  Poprawa = c(100, 175),
  Brak = c(3, 8)
)

# Obliczanie prawdopodobieństw poprawy dla każdej tabeli
total_improvement_probabilities <- calculate_improvement_probabilities(total_data)
comorbid_improvement_probabilities <- calculate_improvement_probabilities(comorbid_data)
no_comorbid_improvement_probabilities <- calculate_improvement_probabilities(no_comorbid_data)

# Test proporcji (chi2 test) dla każdej tabeli
total_test_result <- perform_prop_test(total_data)
comorbid_test_result <- perform_prop_test(comorbid_data)
no_comorbid_test_result <- perform_prop_test(no_comorbid_data)

# Wyniki
results <- data.frame(
  Grupa = c("Cała grupa", "Z chorobami współistniejącymi", "Bez chorób współistniejących"),
  Prawdopodobieństwo_poprawy_leczenie_A = c(total_improvement_probabilities[1], 
                                            comorbid_improvement_probabilities[1], 
                                            no_comorbid_improvement_probabilities[1]),
  Prawdopodobieństwo_poprawy_leczenie_B = c(total_improvement_probabilities[2], 
                                            comorbid_improvement_probabilities[2], 
                                            no_comorbid_improvement_probabilities[2]),
  Odrzucenie_H_0_z_poprawką = c(total_test_result$rejection_H0_with, 
                                comorbid_test_result$rejection_H0_with, 
                                no_comorbid_test_result$rejection_H0_with),
  Odrzucenie_H_0_bez_poprawki = c(total_test_result$rejection_H0_without, 
                                  comorbid_test_result$rejection_H0_without, 
                                  no_comorbid_test_result$rejection_H0_without)
)
# jak zmiescisz 
results
xtable(results)

# jak nie zmiescisz
transposed_results <- t(results)
colnames(transposed_results) <- results$Grupa
transposed_results <- transposed_results[-1, ]

transposed_results
xtable(transposed_results)


# WNIOSEK:
# Paradoks Simpsona występuje w tym zbiorze danych. (mamy zmiane z true na false)


