library(ggplot2)
library(tidyr)
library(dplyr)
library(xtable)
library(reshape2)


data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))

colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

data <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                  labels = c("0-35", "36-45", "46-55", "56+")))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ II @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 4
# Zapoznaj się z funkcją fisher.test z pakietu stats.
library(stats)
help(fisher.test)


# Zadanie 5
# Korzystając z testu Fishera, na poziomie istotności 0.05, zweryfikuj hipotezę, że zmienna PŁEC 
# i zmienna CZY_KIER są niezależne. Czy na poziomie istotności 0.05 możemy wnioskować, że prawdopodobieństwo
# tego, że na stanowisku kierowniczym pracuje kobieta jest równe prawdopodobieństwu, że na stanowsiku 
# kierowniczym pracuje mężczyzna?
alpha = 0.05

fisher_test <- fisher.test(data$PŁEĆ, data$CZY_KIER, conf.level = 1 - alpha)$p.value
cat("PŁEC a CZY_KIER - ", ifelse(fisher_test > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test, "\n")
# widzimy, że p-wartosc> 0.05 dlatego nie mamy podstaw aby odrzucić niezależność 


# Jeśli p-wartość jest większa niż 0.05, to oznacza, że nie ma wystarczająco dużo dowodów na odrzucenie hipotezy zerowej,
# czyli brak zależności między płcią a stanowiskiem kierowniczym. W takim przypadku, możemy stwierdzić, 
# że prawdopodobieństwo tego, że na stanowisku kierowniczym pracuje kobieta, jest równe prawdopodobieństwu,
# że pracuje tam mężczyzna.




# Zadanie 6
# Korzystając z testu Freemana-Haltona na poziomie istotności 0.05 zweryfikuj następujące hipotezy
alpha = 0.05
# _______________________________________________________________________________________
# •  zajmowanie stanowsiaka kierowniczego nie zależy od wieku (CZY_KIER oraz WIEK_KAT)
fisher_test_1 <- fisher.test(data$CZY_KIER, data$WIEK_KAT, conf.level = 1 - alpha)$p.value
cat("CZY_KIER a WIEK_KAT - ", ifelse(fisher_test_1 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_1, "\n")
# nie zalezy, bo p-value =  0.7823002 > 0.05

#______________________________________________________________________________________
# •  zajmowanie stanowiska kierowniczego nie zalezy od stazu pracy (CZY_KIER oraz STAZ)
fisher_test_2 <- fisher.test(data$CZY_KIER, data$STAŻ, conf.level = 1 - alpha)$p.value
cat("CZY_KIER a STAZ - ", ifelse(fisher_test_2 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_2, "\n")
# zalezy od stazu, poniewaz p-val = 6.537896e-05 < 0.05

#__________________________________________________________________________________________
# •  zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od zajmowanego stanowiska (PYT_2 oraz CZY_KIER)
fisher_test_3 <- fisher.test(data$PYT_2, data$CZY_KIER, conf.level = 1 - alpha)$p.value
cat("PYT_2 a CZY_KIER - ", ifelse(fisher_test_3 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_3, "\n")
# zalezy bo otrzymalismy  p-val =  0.04429734 < 0.05

#____________________________________________________________________________________________
# •  zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od stazu ( PYT_2 oraz STAZ)
fisher_test_4 <- fisher.test(data$PYT_2, data$STAŻ, conf.level = 1 - alpha)$p.value
cat("PYT_2 a STAZ - ", ifelse(fisher_test_4 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_4, "\n")
# zalezy bo p-val = 0.01069023 < 0.05

#____________________________________________________________________________________________
# •  zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od płci ( PYT_2 oraz PŁEC)
fisher_test_5 <- fisher.test(data$PYT_2, data$PŁEĆ, conf.level = 1 - alpha)$p.value
cat("PYT_2 a PLEC - ", ifelse(fisher_test_5 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_5, "\n")
# nie zalezy od płci, bo p-val = 0.4758086 > 0.05

#____________________________________________________________________________________________
# • zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od wieku (PYT_2 oraz WIEK_KAT)
fisher_test_6 <- fisher.test(data$PYT_2, data$WIEK_KAT, conf.level = 1 - alpha, workspace= 300000)$p.value
cat("PYT_2 a WIEK_KAT - ", ifelse(fisher_test_6 > alpha, "Nie zależy", "Zależy"), ", bo p-value =", fisher_test_6, "\n")
# nie zalezy od kategorii wielkowej, bo p-val =  0.319352 > 0.05



#źródło do wszystkiego: https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/

