library(ggplot2)
library(tidyr)
library(dplyr)
library(kableExtra)
library(knitr)
library(likert)
library(reshape2)


data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))
colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

data$PYT_1 <- factor(data$PYT_1, levels = c(-2, -1, 0, 1, 2),
                     labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "nie mam zdania", "zgadzam się", "zdecydowanie się zgadzam"))
data$PYT_2 <- factor(data$PYT_2, levels = c(-2, -1, 1, 2),
                     labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie się zgadzam"))
data$PYT_3 <- factor(data$PYT_3, levels = c(-2, -1, 1, 2),
                     labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie się zgadzam"))


df <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                  labels = c("0-35", "36-45", "46-55", "56+")))

df$CZY_ZADOW <- ifelse(df$PYT_2 %in% c("zdecydowanie się nie zgadzam", "nie zgadzam się"), "NIE", 
                       "TAK")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ III i IV @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 6
# Napisz funkcję do wyznaczania realizacji przedziału ufności Cloppera-Pearsona. 
# Niech argumentem wejściowym będzie poziom ufności, liczba sukcesów i liczba prób
# lub poziom ufności i wektor danych (funkcja powinna obsługiwać oba przypadki).

intervals_clopper_pearson <- function(alpha, successes, trials = NULL) {
  
  if (is.null(trials)) {
    data <- successes
    successes <- sum(data == "TAK")
    trials <- length(data)
  }
  
  result_lower <- qbeta(alpha/2, successes, trials - successes + 1)
  result_upper <- qbeta(1 - (alpha/2), successes + 1, trials - successes)
  
  result <- data.frame(
    lower = result_lower,
    upper = result_upper
  )
  
  return(result)
}


alpha <- 0.05
trials <- 1000
sample_trial <- sample(c("TAK", "NIE"), trials, replace = TRUE)
successes <- sum(sample_trial == "TAK")

clopper_pearson_1 <- intervals_clopper_pearson(alpha, successes, trials)
clopper_pearson_2 <- intervals_clopper_pearson(alpha, sample_trial)

print(clopper_pearson_1)
print(clopper_pearson_2)

# Do celów porównawczych (tylko dla nas)
library(GenBinomApps)

ci <- clopper.pearson.ci(successes, trials, alpha = alpha, CI = "two.sided")
print(ci)



##########################################################################################################
# Zadanie 7
# Korzystając z funkcji napisanej w zadaniu 6. wyznacz realizacje przedziałów ufności 
# dla prawdopodobieństwa, że pracownik jest zadowolony z wynagrodzenia w pierwszym badanym
# okresie oraz w drugim badanym okresie. Skorzystaj ze zmiennych CZY_ZADW oraz CZY_ZADW_2
# (utwórz zmienną analogicznie jak w zadaniu 1.7. Przyjmij 1 - alpha = 0.95.

df$CZY_ZADOW_2 <- ifelse(df$PYT_3 %in% c("zdecydowanie się nie zgadzam", "nie zgadzam się"), "NIE", 
                       "TAK")

alph <- 0.05

clopper_pearson_PYT_3 <- intervals_clopper_pearson(alph, df$CZY_ZADOW_2)
clopper_pearson_PYT_2 <- intervals_clopper_pearson(alph, df$CZY_ZADOW)

print(clopper_pearson_PYT_3)
print(clopper_pearson_PYT_2)


ci_1 <- clopper.pearson.ci(sum(df$CZY_ZADOW_2 == "TAK"), length(df$CZY_ZADOW_2), alpha = alph, CI = "two.sided")
ci_2 <- clopper.pearson.ci(sum(df$CZY_ZADOW == "TAK"), length(df$CZY_ZADOW), alpha = alph, CI = "two.sided")
print(ci_1)
print(ci_2)


##########################################################################################################
# Zadanie 8
# Zapoznaj się z funkcjami rbinom z biblioteki stats oraz binom.confint z biblioteki binom
library(binom)

help(rbinom)
help(binom.confint)

##########################################################################################################
# Zadanie 9
# Przeprowadź symulacje, których celem jest porównanie prawdopodobieństwa pokrycia i długości
# przedziałów ufności Cloppera-Pearsona, Walda i trzeciego dowolnego typu zaimplementowanego
# w funkcji binom.confint. Rozważ 1 - alpha = 0.95, rozmiar próby n = {30, 100, 1000} 
# i różne wartości prawdopodobieństwa p. Wyniki umieść na wykresach i sformułuj wnioski, 
# które dla konkretnych danych ułatwią wybór konkretenego typu przedziału ufności.


calculate_cf <- function(alpha, n, p, M) {
  size <- length(p)
  
  n_cp <- numeric(size)
  n_wald <- numeric(size)
  n_wilson <- numeric(size)

  p_cp <- numeric(size)
  p_wald <- numeric(size)
  p_wilson <- numeric(size)
  
    for (k in 1:size) {
      length_cp <- numeric(M)
      length_wald <- numeric(M)
      length_wilson <- numeric(M)
      
      coverage_cp <- numeric(M)
      coverage_wald <- numeric(M)
      coverage_wilson <- numeric(M)
      
      for (i in 1:M) {
        sample_binom <- rbinom(n, 1, p[k])
        successes <- sum(sample_binom)
        
        clopper_pearson <- binom.confint(successes, n, conf.level = 1 - alpha, methods = "exact")
        wald <- binom.confint(successes, n, conf.level = 1 - alpha, methods = "asymptotic")
        wilson <- binom.confint(successes, n, conf.level = 1 - alpha, methods = "wilson")
        
        
        length_cp[i] <- clopper_pearson[, "upper"] - clopper_pearson[, "lower"]
        length_wald[i] <- wald[, "upper"] - wald[, "lower"]
        length_wilson[i] <- wilson[, "upper"] - wilson[, "lower"]
        
        coverage_cp[i] <- (clopper_pearson[,"lower"] <= p[k]) & (p[k] <= clopper_pearson[,"upper"])
        coverage_wald[i] <- (wald[,"lower"] <= p[k]) & (p[k] <= wald[,"upper"])
        coverage_wilson[i] <- (wilson[,"lower"] <= p[k]) & (p[k] <= wilson[,"upper"])

      }
      
      n_cp[k] <- mean(length_cp)
      n_wald[k] <- mean(length_wald)
      n_wilson[k] <- mean(length_wilson)

      p_cp[k] <- mean(coverage_cp)
      p_wald[k] <- mean(coverage_wald)
      p_wilson[k] <- mean(coverage_wilson)
      
    }
  
  result_n <- data.frame(
    "p" = p,
    "test Cloppera-Pearsona" = n_cp,
    "test Walda" = n_wald,
    "test Wilsona" =  n_wilson
  )
  
  result_p <- data.frame(
    "p" = p,
    "test Cloppera-Pearsona" =  p_cp,
    "test Walda" =  p_wald,
    "test Wilsona" =  p_wilson
  )
  
  return(list(result_n = result_n, result_p = result_p))
}



alpha <- 0.05
n <- c(30, 100, 1000)
p <- seq(0.01, 0.99, by = 0.01)
M <- 1000


results_30 <- calculate_cf(alpha, n[1], p, M)
results_100 <- calculate_cf(alpha, n[2], p, M)
results_1000 <- calculate_cf(alpha, n[3], p, M)


df_length_30 <- melt(results_30$result_n, id.vars = "p", variable.name = "Method", value.name = "Length")
df_coverage_30 <- melt(results_30$result_p, id.vars = "p", variable.name = "Method", value.name = "Coverage")
df_length_100 <- melt(results_100$result_n, id.vars = "p", variable.name = "Method", value.name = "Length")
df_coverage_100 <- melt(results_100$result_p, id.vars = "p", variable.name = "Method", value.name = "Coverage")
df_length_1000 <- melt(results_1000$result_n, id.vars = "p", variable.name = "Method", value.name = "Length")
df_coverage_1000 <- melt(results_1000$result_p, id.vars = "p", variable.name = "Method", value.name = "Coverage")



# wykresy średniej długości
ggplot(df_length_30, aes(x = p, y = Length, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Średnia długość Przedziałów dla n = 30", 
       y = "Średnia Długość", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_length_100, aes(x = p, y = Length, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Średnia długość Przedziałów dla n = 100", 
       y = "Średnia Długość", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_length_1000, aes(x = p, y = Length, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Średnia długość Przedziałów dla n = 1000", 
       y = "Średnia Długość", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))



# wykresy prawdopodobienstwa pokrycia (czy)
ggplot(df_coverage_30, aes(x = p, y = Coverage, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Prawdopodobieństwo Pokrycia Przedziałów dla n = 30", 
       y = "Prawdopodobieństwo Pokrycia", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_coverage_100, aes(x = p, y = Coverage, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Prawdopodobieństwo Pokrycia Przedziałów dla n = 100", 
       y = "Prawdopodobieństwo Pokrycia", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_coverage_1000, aes(x = p, y = Coverage, color = Method)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Prawdopodobieństwo Pokrycia Przedziałów dla n = 1000", 
       y = "Prawdopodobieństwo Pokrycia", 
       x = "Prawdopodobieństwo (p)",
       color = "Metoda") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))



