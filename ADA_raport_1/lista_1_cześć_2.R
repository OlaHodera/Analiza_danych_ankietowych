library(ggplot2)
library(tidyr)
library(dplyr)
library(kableExtra)
library(knitr)
library(likert)


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

# View(df)
head(df)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ II @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 2. Zapoznaj się z biblioteką likert i dostępnymi tam funkcjami summary oraz plot
# (wykresy typu "bar", "heat" oraz "density"), a następnie zilustruj odpowiedzi na pytanie 
# "Jak bardzo zgadzasz się ze stwierdzeniem, że firma pozwala na (...)?" (zmienna PYT_1)
# w całej badanej grupie oraz w podgrupach ze względu na zmienną CZY_KIER.

df$CZY_KIER <- factor(df$CZY_KIER, levels = c("Nie", "Tak"))
head(df)

#_______________________________ bez grupowania ______________________________________
# Przekształcenie podsumowania na ramkę danych
likert_pyt_1 <- likert(df[,"PYT_1", drop=FALSE])

summ_pyt_1 <- summary(likert_pyt_1)

# Wyświetlenie podsumowania w formie tabeli
kable(summ_pyt_1, 
      caption = "Podsumowanie odpowiedzi na pytanie PYT_1", 
      align = "c",
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


plot(likert_pyt_1, type = "bar", auto.key = list(columns = 2)) +
  labs(title = "Wykres słupkowy dla PYT_1") +
  guides(fill = guide_legend(title = "Odpowiedzi")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14,, hjust = 0.5))



plot(likert_pyt_1, type = "heat") + 
  labs(title = "Wykres typu Heatmap dla PYT_1") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))


plot(likert_pyt_1, type = "density") +
  theme_minimal() +
  labs(title = "Wykres typu Density") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5))


#_______________________________ z grupowaniem ______________________________________
likert_pyt_1_grouped <- likert(df[, "PYT_1", drop = FALSE], grouping = df$CZY_KIER)

summ_pyt_1_grouped <- summary(likert_pyt_1_grouped)
print(summ_pyt_1_grouped)

# Wyświetlenie podsumowania w formie tabeli
kable(summ_pyt_1_grouped, 
      caption = "Podsumowanie odpowiedzi na pytanie PYT_1", 
      align = "c",
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

plot(likert_pyt_1_grouped, type = "bar", auto.key = list(columns = 2)) +
  labs(title = "Wykres słupkowy dla PYT_1") +
  guides(fill = guide_legend(title = "Odpowiedzi")) + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# pozostałe wykresy nie działaja !!!!!!!!



##########################################################################################################
# 3.Zapoznaj się z funkcją sample z biblioteki stats, a następnie wylosuj próbkę o liczności 
# 10% wszystkich rekordów z pliku "ankieta.csv" w dwóch wersjach: ze zwracaniem oraz bez zwracania.
# Liczność próbki (10% wszystkich rekordów)
sample_size <- round(0.1 * nrow(df))

# losowanie ze zwracaniem 
with_returning <- sample(1:nrow(df), size = sample_size, replace = TRUE)


# losowanie bez zwracania 
without_returning <- sample(1:nrow(df), size = sample_size, replace = FALSE)

# Wyświetlenie wylosowanych próbek
kable(with_returning, 
      caption = "Wylosowana próbka ze zwracaniem", 
      align = "c",
      col.names = c("Próbka"),
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

kable(without_returning, 
      caption = "Wylosowana próbka bez zwracania", 
      align = "c",
      col.names = c("Próbka"),
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

##########################################################################################################
# 4. Zaproponuj metodę symulowania zmiennych losowych z rozkładu dwumianowego.
# Napisz funkcję do generowania realizacji, a następnie zaprezentuj jej działanie porównując wybrane teoretyczne i
# empiryczne charakterystyki dla przykładowych wartości paramertów rozkładu: n i p.

generate_binomial <- function(n, p, size) {
  result <- numeric(size)
  for (i in 1:size) {
    successes <- 0
    for (j in 1:n) {
      if (runif(1) < p) {
        successes <- successes + 1
      }
    }
    result[i] <- successes
  }
  return(result)
}


n <- 1000
p_values <- c(0.1, 0.5, 0.9)
size <- 1000

empirical_p <- numeric(length(p_values))

theoretical_mean <- numeric(length(p_values))
theoretical_variance <- numeric(length(p_values))
empirical_mean <- numeric(length(p_values))
empirical_variance <- numeric(length(p_values))

for (i in 1:length(p_values)) {
  p <- p_values[i]
  
  empirical_binomial <- generate_binomial(n, p, size)
  empirical_p[i] <- mean(empirical_binomial) / n
  
  theoretical_mean[i] <- n * p
  theoretical_variance[i] <- n * p * (1 - p)
  
  empirical_mean[i] <- mean(empirical_binomial)
  empirical_variance[i] <- var(empirical_binomial)

}

results_binomial <- data.frame(
  "Charakterystyka" = c("Teoretyczna średnia", "Empiryczna średnia", "Teoretyczna wariancja", "Empiryczna wariancja"),
  "p = 0.1" = c(theoretical_mean[1], empirical_mean[1], theoretical_variance[1], empirical_variance[1]),
  "p = 0.5" = c(theoretical_mean[2], empirical_mean[2], theoretical_variance[2], empirical_variance[2]),
  "p = 0.9" = c(theoretical_mean[3], empirical_mean[3], theoretical_variance[3], empirical_variance[3])
)


kable(results_binomial, 
      caption = "Porównanie empirycznych statystyk z teoretycznymi", 
      align = "c",
      col.names = c("Charakterystyka", "p = 0.1", "p = 0.5", "p = 0.9"),
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


results_comparison <- data.frame(
  "Teoretyczne" = p_values,
  "Empiryczne" = empirical_p
)

ggplot(results_comparison, aes(x = factor(0:(length(Teoretyczne) - 1)))) +
  geom_bar(aes(y = Empiryczne, fill = "Empiryczna"), stat = "identity", width = 0.05) +
  geom_bar(aes(y = Teoretyczne, fill = "Teoretyczna"), stat = "identity", width = 0.02, alpha = 0.5) +
  geom_point(aes(y = Empiryczne, color = "Empiryczna"), size = 6) +
  geom_point(aes(y = Teoretyczne, color = "Teoretyczna"), size = 4, alpha = 0.5) +
  scale_fill_manual(values = c("Teoretyczna" = "red", "Empiryczna" = "skyblue"), name = "") +
  scale_color_manual(values = c("Teoretyczna" = "red", "Empiryczna" = "skyblue"), name = "") +
  labs(x = "X", y = "Prawdopodobieństwo") +
  ggtitle("Porównanie teoretycznych i empirycznych prawdopodobieństw\n rozkładu dwumianowego") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


sorted_empirical <- sort(empirical_p)

ggplot() +
  geom_step(data = data.frame(x = c(0, 0, 1, 2, 3), y = c(0, sorted_empirical, max(sorted_empirical))), aes(x, y, color = "Empiryczna"),
            , linetype = "solid", size = 1.5) +
  geom_step(data = data.frame(x = c(0, 0, 1, 2, 3), y = c(0, p_values, max(sorted_empirical))), aes(x, y, color = "Teoretyczna"),
            , linetype = "dashed", size = 1) +
  labs(x = "X", y = "Dystrybuanta", 
       title = "Porównanie teoretycznej i empirycznej dystrybuanty dla p") +
  scale_x_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(name = " ", values = c("skyblue", "red"), labels = c("Empiryczna", "Teoretyczna")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))



##########################################################################################################
# 5. Zaproponuj metodę symulowania wektorów losowych z rozkładu wielomianowego.
# Napisz funkcję do generowania realizacji, a następnie zaprezentuj jej działanie porównując wybrane teoretyczne
# i empiryczne charakterystyki dla przykładowych wartości paramertów rozkładu: n i p.

generate_multinomial <- function(n, p) {
  result <- numeric(length(p))
  remaining_trials <- n
  prob_sum <- 1

  for (i in 1:(length(p) - 1)) {
    if (prob_sum != 0) {
      result[i] <- rbinom(1, remaining_trials, p[i]/prob_sum)
    } else {
      result[i] <- 0
    }
    remaining_trials <- remaining_trials - result[i]
    prob_sum <- prob_sum - p[i]
  }

  result[length(p)] <- remaining_trials
  return(result)
}


p_values <- list(
  c(0.1, 0.3, 0.4, 0.2),
  c(0.5, 0.3, 0.2),
  c(0.1, 0.1, 0.3, 0.2, 0.3)
)

n <- 10000

results_df <- data.frame()


for (p in p_values) {
  sample <- generate_multinomial(n, p)

  empirical_p <- sample / n

  result <- data.frame(
    Theoretical_p = as.character(p),
    Empirical_p = as.character(empirical_p)
  )

  results_df <- rbind(results_df, result)
}

kable(results_df,
      caption = "Wylosowana próbka bez zwracania",
      align = "c",
      col.names = c("Teoretyczne p", "Empiryczne p"),
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


