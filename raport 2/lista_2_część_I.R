library(ggplot2)
library(tidyr)
library(dplyr)
library(xtable)
library(binom)
library(reshape2)

data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))

colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

data <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                    labels = c("0-35", "36-45", "46-55", "56+")))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ I @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 1
# W ankiecie przedstawionej na poprzedniej liscie pracownicy zostali poproszeni 
# o wyrazenie opinii na temat podejscia firmy do utrzymania równowagi miedzy zyciem 
# zawodowym a prywatnym. Wsród próbki 200 pracowników (losowanie proste ze zwracaniem)
# uzyskano wyniki:
#  • 14 pracowników - bardzo niezadowolonych,
#  • 17 pracowników - niezadowolonych,
#  • 40 pracowników - nie ma zdania,
#  • 100 pracowników - zadowolonych,
#  • 29 pracowników - bardzo zadowolonych,

# Na podstawie danych wyznacz przedział ufnosci dla wektora prawodobienstw opisujacego 
# stopien zadowolenia z podejscia firmy. Przyjmij poziom ufnosci 0.95.

n <- 200  
categories <- c("Bardzo niezadowolony", "Niezadowolony", "Nie ma zdania", "Zadowolony", "Bardzo zadowolony")
x <- c(14, 17, 40, 100, 29)  
alpha <- 0.05 

binom_ci_exact <- binom.confint(x, n, conf.level = 1 - alpha/5, methods = 'exact')
lower_ci_exact <- binom_ci_exact$lower
upper_ci_exact <- binom_ci_exact$upper

binom_ci_asymptotic <- binom.confint(x, n, conf.level = 1 - alpha/5, methods = 'asymptotic')
lower_ci_asymptotic <- binom_ci_asymptotic$lower
upper_ci_asymptotic <- binom_ci_asymptotic$upper


p <- x/n
df <- data.frame(categories, p, lower_ci_exact, upper_ci_exact, lower_ci_asymptotic, upper_ci_asymptotic)
print(df) 

table <- xtable(df) 
print(table) 


ggplot(df_long, aes(x = categories, y = p)) +
  geom_point(color = "black", size = 4) +
  geom_errorbar(aes(ymin = lower_ci_exact, ymax = upper_ci_exact, color = "Dokładny"), 
                width = 0.2, alpha = 0.5, linewidth = 1.5) +
  geom_errorbar(aes(ymin = lower_ci_asymptotic, ymax = upper_ci_asymptotic, color = "Asymptotyczny"), 
                width = 0.2, linetype = 'dashed', linewidth = 1, alpha = 0.7) +
  labs(title = "Przedział ufności dla prawdopodobieństwa zadowolenia",
       y = "Prawdopodobieństwo",
       x = "Stopień zadowolenia",
       color = "Legend") +
  scale_color_manual(values = c("blue", "deeppink"), 
                     labels = c("Dokładny", "Asymptotyczny"),
                     name = "Przedział ufności") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10.5),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
  )


##########################################################################################################
# Zadanie 2
# Napisz funkcje, która wyznacza wartosc poziomu krytycznego w nastepujacych testach:
#  • chi-kwadrat Pearsona
#  • chi-kwadrat najwiekszej wiarogodnosci ´
# słuzacych do weryfikacji hipotezy H0 : p = p0 przy hipotezie alternatywnej H0 : p ̸= p0 na
# podstawie obserwacji x wektora losowego X z rozkładu wielomianowego z parametrami n i p.


chi2_test <- function(x, p0, n = sum(x), alpha = 0.05, method = c("pearson", "likelihood")) {
  df <- length(x) - 1
  expected <- n*p0
  
  if (method == "pearson") {
    chi2 <- sum(((x - expected)^2) / expected)
    p_val <- 1 - pchisq(chi2, df = df)
  } 
  
  else if (method == "likelihood") {
    chi2 <- 2 * sum(x * log(x / expected))
    p_val <- 1 - pchisq(chi2, df = df)
  } 
  
  crit_val <- qchisq(1 - alpha, df)
  
  return(list(chi2 = chi2, p_val = p_val, crit_val = crit_val))
}

set.seed(123)
alpha <- 0.05
n <- 1000
p0 <- c(0.25, 0.50, 0.25)
x <- rmultinom(1, n, p0)

pearson_chi2_test <- chi2_test(x, p0, method = "pearson")
MLE_chi2_test <- chi2_test(x, p0, method = "likelihood")


test_results <- data.frame(
  Test = c("Chi2 Pearson", "Chi2 MLE"),
  Odrzucenie_H0 = c(ifelse(pearson_chi2_test$chi2 > pearson_chi2_test$crit_val, "Tak", "Nie"),
                    ifelse(MLE_chi2_test$chi2 > MLE_chi2_test$crit_val, "Tak", "Nie")),
  Statystyka_chi2 = c(pearson_chi2_test$chi2, MLE_chi2_test$chi2),
  p_wartość = c(pearson_chi2_test$p_val, MLE_chi2_test$p_val),
  Wartość_krytyczna = c(pearson_chi2_test$crit_val, pearson_chi2_test$crit_val)
)

print(test_results) 

xtable(test_results, caption = "Wyniki testów chi-kwadrat")


# źrodło: https://www.scribbr.com/statistics/chi-square-tests/


##########################################################################################################
# Zadanie 3
# Na podstawie danych z ankiety z poprzedniej listy zweryfikuj hipoteze, ze w grupie 
# pracowników zatrudnionwych w Dziale Kreatywnym rozkład odpowiedzi na pytanie dotyczace
# podejscia firmy do utrzymania równowagi miedzy zyciem zawodowym a prywatnym jest
# równomierny, tzn. jest jednakowe prawdopodobienstwo, ze pracownik zatrudniony w Dziale
# Kreatywnym jest udzielił odpowiedzi "zdecydowanie sie nie zgadzam", "nie zgadzam sie", "nie
# mam zdania", "zgadzam sie", "zdecydowanie sie zgadzam"na pytanie PYT_1. Przyjmij poziom
# istotnosci 0.05. Skorzystaj z funkcji napisanej w zadaniu 2.

alpha <- 0.05
data_DK <- subset(data, DZIAŁ == "DK")

table_DK <- table(data_DK$PYT_1)
p0 <- rep(1/5, 5)

pearson_chi2_test <- chi2_test(table_DK, p0, method = "pearson")
MLE_chi2_test <- chi2_test(table_DK, p0, method = "likelihood")

test_results <- data.frame(
  Test = c("Chi2 Pearson", "Chi2 MLE"),
  Odrzucenie_H0 = c(ifelse(pearson_chi2_test$chi2 > pearson_chi2_test$crit_val, "Tak", "Nie"),
                    ifelse(MLE_chi2_test$chi2 > MLE_chi2_test$crit_val, "Tak", "Nie")),
  Statystyka_chi2 = c(pearson_chi2_test$chi2, MLE_chi2_test$chi2),
  p_wartość = c(pearson_chi2_test$p_val, MLE_chi2_test$p_val),
  Wartość_krytyczna = c(pearson_chi2_test$crit_val, pearson_chi2_test$crit_val)
)

print(test_results) 

xtable(test_results, caption = "Wyniki testów chi-kwadrat")

