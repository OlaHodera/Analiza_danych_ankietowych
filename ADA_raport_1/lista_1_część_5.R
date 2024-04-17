
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

df$CZY_ZADOW_2 <- ifelse(df$PYT_3 %in% c("zdecydowanie się nie zgadzam", "nie zgadzam się"), "NIE", 
                         "TAK")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ V @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 10
# Zapoznaj się z funkcjami binom.test oraz prop.test z bibliotek stats.

library(stats)
help(binom.test) # test dokładny
help(prop.test) # test asymptotyczny
# correcter = TRUE - poprawione



##########################################################################################################
# Zadanie 11
# Dla danych z pliku 'ankieta.csv', korzystając z funkcji z zadania 10, przyjmując 1 - alpha = 0.95,
# zweryfikuj następujące hipotezy i sformułuj wnioski:

# ogolnie dla czesci z tych zadan
females <- sum(df$PŁEĆ == "K")
males <- sum(df$PŁEĆ == "M")
length_sex <- c(females, males)

all_rows <- nrow(df) 
   
# 1.Prawdopodobieństwo, że w firmie pracuje kobieta wynosi 0.5.
binom_test <- binom.test(females, all_rows, p=0.5, alternative = "two.sided", conf.level = 0.95)
prop_test_1 <- prop.test(females, all_rows, p=0.5, alternative = "two.sided", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(females, all_rows, p=0.5, alternative = "two.sided", conf.level = 0.95, correct = FALSE)

print(binom_test$p.value)
print(prop_test_1$p.value)
print(prop_test_2$p.value)

#nie dziala, p = 0.35!!, p-value mniejsze niz poziom istotnosci
#  p-value < 0.05!





# 2.Prawdopodbieństwo, że pracownik jest zadowolony ze swojego wynagrodzenia w pierwszym badanym
# okresie jest większe bądź równe 0.7.
content_1 <- sum(df$CZY_ZADOW == "YES")


binom_test <- binom.test(content_1, all_rows, p=0.7, alternative = "greater", conf.level = 0.95)
prop_test_1 <- prop.test(content_1, all_rows, p=0.7, alternative = "greater", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(content_1, all_rows, p=0.7, alternative = "greater", conf.level = 0.95, correct = FALSE)


print(binom_test$p.value)
print(prop_test_1$p.value)
print(prop_test_2$p.value)
# p-value = 1 > 0.05


content_2 <- sum(df$CZY_ZADOW_2 == "YES")

binom_test <- binom.test(content_2, all_rows, p=0.7, alternative = "greater", conf.level = 0.95)
prop_test_1 <- prop.test(content_2, all_rows, p=0.7, alternative = "greater", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(content_2, all_rows, p=0.7, alternative = "greater", conf.level = 0.95, correct = FALSE)

print(binom_test$p.value)
print(prop_test_1$p.value)
print(prop_test_2$p.value)
# p-value = 1 > 0.05




# 3.Prawdopodobieństwo, że kobieta pracuje na stanowisku menedżerskim jest równe prawdopodobieństwu,
# że mężczyzna pracuje na stanowisku menedżerskim.
manager <- filter(df, CZY_KIER == "Tak")
female_manager <- sum(manager$PŁEĆ == "K")
male_manager <- sum(manager$PŁEĆ == "M")

fem_male_manager <- c(female_manager, male_manager)

prop_test_1 <- prop.test(fem_male_manager, length_sex, alternative = "two.sided", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(fem_male_manager, length_sex, alternative = "two.sided", conf.level = 0.95, correct = FALSE)

print(prop_test_1$p.value)
print(prop_test_2$p.value)
# p-value > 0.05




# 4.Prawdopodobieństwo, że kobieta jest zadowolona ze swojego wynagrodzenia w pierwszym badanym
# okresie jest równe prawdopodobieństwu, że mężczyzna jest zadowolony ze swojego wynagrodzenia
# w pierwszym badanym okresie.
content <- filter(df, CZY_ZADOW == "TAK")
female_content <- sum(content$PŁEĆ == "K")
male_content <- sum(content$PŁEĆ == "M")

fem_male_content <- c(female_content, male_content)

prop_test_1 <- prop.test(fem_male_content, length_sex, alternative = "two.sided", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(fem_male_content, length_sex, alternative = "two.sided", conf.level = 0.95, correct = FALSE)

print(prop_test_1$p.value)
print(prop_test_2$p.value)
# p-value > 0.05



# 5.Prawdopodobieństwo, że kobieta pracuje w dziale obsługi kadrowo-płacowej jest większe lub
# równe prawdopodobieństwu, że mężczyzna pracuje w dziale obsługi kadrowo-płacowej.
HR <- filter(df, DZIAŁ == "HR")
female_HR <- sum(HR$PŁEĆ == "K")
male_HR <- sum(HR$PŁEĆ == "M")

fem_male_HR <- c(female_HR, male_HR)

prop_test_1 <- prop.test(fem_male_HR, length_sex, alternative = "greater", conf.level = 0.95, correct = TRUE)
prop_test_2 <- prop.test(fem_male_HR, length_sex, alternative = "greater", conf.level = 0.95, correct = FALSE)

print(prop_test_1$p.value)
print(prop_test_2$p.value)
# p-value > 0.05!!

##########################################################################################################
# Zadanie 12
# Wyznacz symulacyjnie moc testu dokładnego oraz moc testu asymptotycznego w przypadku weryfikacji
# hipotezy zerowej H_0: p = 0.9 przeciwko H_1: p =/= 0.9 przyjmując wartość 1 - alpha = 0.95.
# Uwzględnij różne wartości alternatyw i różne rozmiary próby. Sformułuj wnioski.

calculate_power <- function(H_0, alpha, n, p, M) {
  size <- length(p)
  
  result_binom <- numeric(size)
  result_prop_1 <- numeric(size)
  result_prop_2 <- numeric(size)
  
  for (k in 1:size) {
    rejection_binom <- 0
    rejection_prop_1 <- 0
    rejection_prop_2 <- 0
    
    for (i in 1:M) {
      sample_binom <- rbinom(n, 1, p[k])
      summ <- sum(sample_binom)
    
      binom <- binom.test(summ, n, p=H_0, alternative = "two.sided", conf.level = 1 - alpha)
      prop_1 <- prop.test(summ, n, p=H_0, alternative = "two.sided", conf.level = 1 - alpha, correct = TRUE)
      prop_2 <- prop.test(summ, n, p=H_0, alternative = "two.sided", conf.level = 1 - alpha, correct = FALSE)
      
      binom_pv <- binom$p.value
      prop_pv_1 <- prop_1$p.value
      prop_pv_2 <- prop_2$p.value
      
      if (binom_pv < alpha) {
        rejection_binom <- rejection_binom + 1
      }
      if (prop_pv_1 < alpha) {
        rejection_prop_1 <- rejection_prop_1 + 1
      }
      if (prop_pv_2 < alpha) {
        rejection_prop_2 <- rejection_prop_2 + 1
      }
      
    }
    result_binom[k] <- rejection_binom/M
    result_prop_1[k] <- rejection_prop_1/M
    result_prop_2[k] <- rejection_prop_2/M
  }
  return(list(result_binom = result_binom, result_prop_1 = result_prop_1, result_prop_2 = result_prop_2))
}


alpha <- 0.05
H_0 <- 0.9
n <- c(30, 100, 1000)
p <- seq(0.01, 0.99, by = 0.01)
M <- 1000

result_30 <- calculate_power(H_0, alpha, n[1], p, M)
result_100 <- calculate_power(H_0, alpha, n[2], p, M)
result_1000 <- calculate_power(H_0, alpha, n[3], p, M)


df_30 <- data.frame(p = p, moc_binom = result_30$result_binom, moc_prop_1 = result_30$result_prop_1, moc_prop_2 = result_30$result_prop_2)
df_100 <- data.frame(p = p, moc_binom = result_100$result_binom, moc_prop_1 = result_100$result_prop_1, moc_prop_2 = result_100$result_prop_2)
df_1000 <- data.frame(p = p, moc_binom = result_1000$result_binom, moc_prop_1 = result_1000$result_prop_1, moc_prop_2 = result_1000$result_prop_2)



ggplot(df_30, aes(x = p)) +
  geom_line(aes(y = moc_binom, color = "Test dokładny\n"), linewidth = 0.8) +
  geom_line(aes(y = moc_prop_1, color = "Test asymptotyczny\n z poprawką\n"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = moc_prop_2, color = "Test asymptotyczny\n bez poprawki\n"), linetype = "dotted", linewidth = 1.5) +
  labs(title = "Moc testów dla różnych testów dla n = 30",
       x = "Prawdopodobieństwo (p)",
       y = "Moc",
       color = "Typ testu") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_100, aes(x = p)) +
  geom_line(aes(y = moc_binom, color = "Test dokładny\n"), linewidth = 0.8) +
  geom_line(aes(y = moc_prop_1, color = "Test asymptotyczny\n z poprawką\n"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = moc_prop_2, color = "Test asymptotyczny\n bez poprawki\n"), linetype = "dotted", linewidth = 1.5) +
  labs(title = "Moc testów dla różnych testów dla n = 100",
       x = "Prawdopodobieństwo (p)",
       y = "Moc",
       color = "Typ testu") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


ggplot(df_1000, aes(x = p)) +
  geom_line(aes(y = moc_binom, color = "Test dokładny\n"), linewidth = 0.8) +
  geom_line(aes(y = moc_prop_1, color = "Test asymptotyczny\n z poprawką\n"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = moc_prop_2, color = "Test asymptotyczny\n bez poprawki\n"), linetype = "dotted", linewidth = 1.5) +
  labs(title = "Moc testów dla różnych testów dla n = 1000",
       x = "Prawdopodobieństwo (p)",
       y = "Moc",
       color = "Typ testu") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))


