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


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ IV i V @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 8. 
# Przyjmujac model log-liniowy [123] dla zmiennych opisanych w zadaniu 7 oszacuj
# prawdopobiebienstwa:
# • ze osoba pracujaca na stanowisku kierowniczym jest zdecydowanie zadowolona ze 
# swojego wynagrodzenia;
# • ze osoba o stazu pracy krótszym niz rok pracuje na stanowisku kierowniczym; 
# • ze osoba o stazu pracy powyzej trzech lat nie pracuje na stanowisku kierowniczym.
# Jakie byłyby oszacowania powyzszych prawdopodobienstw przy załozeniu modelu [12 23]?


new_data <- xtabs(~ STAŻ + PYT_2 + CZY_KIER, data=data)
new_data

# dane - przedstawienie w tablicy
ftable(new_data, row.vars = c("PYT_2", "CZY_KIER"))

# liczności brzegowe
addmargins(new_data)

# zmieniamy format danych
new_data<- as.data.frame(as.table(new_data))
new_data[,-4] <- lapply(new_data[,-4], relevel, ref = 1) #ustawiamy referencje na "1"
new_data

# MODELE
# ℓijk = λ + λ(1)i + λ(2)j + λ(3)k + λ(12)ij + λ(13)ik + λ(23)jk + λ(123)ijk 
model_123 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ + 
                        (CZY_KIER*PYT_2) + 
                        (CZY_KIER*STAŻ) + 
                        (PYT_2*STAŻ) +
                        (CZY_KIER*PYT_2*STAŻ), data = new_data, family = poisson)
summary(model_123)

# ℓijk = λ + λ(1)i + λ(2)j + λ(3)k + λ(12)ik + λ(23)jk 
model_12_23 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ + 
                          (CZY_KIER*PYT_2) + 
                          (PYT_2*STAŻ), data = new_data, family = poisson)

summary(model_12_23)

# dopasowanie 
fitted_123 = fitted(model_123)
fitted_12_23 = fitted(model_12_23)

# porownanie
data_freq <- cbind(new_data, fitted_123, fitted_12_23) 
data_freq 


statistics <- data.frame(
  model = c("[123]", "[12 23]"),
  odchylenie = c(deviance(model_123), deviance(model_12_23)),
  p_wartość = c(1 - pchisq(deviance(model_123), df = df.residual(model_123)),
                1 - pchisq(deviance(model_12_23), df = df.residual(model_12_23)))
)

statistics 
xtable(statistics)

#_______________________________OSZACOWANIE PRAWDOPODOBIENSTW______________________________
# 1. Osoba pracująca na stanowisku kierowniczym jest zdecydowanie zadowolona ze swojego wynagrodzenia
# P(PYT_2 = 2 | CZY_KIER = Tak) = P(PYT_2 = 2 i CZY_KIER = Tak)/P(CZY_KIER = Tak)

prob1_numerator <- sum(data_freq$Freq[data_freq$CZY_KIER == "Tak" & data_freq$PYT_2 == "2"])
prob1_denominator <- sum(data_freq$Freq[data_freq$CZY_KIER == "Tak"])

prob1 <- prob1_numerator / prob1_denominator


prob1_model_123_numerator <- sum(data_freq$fitted_123[data_freq$CZY_KIER == "Tak" & data_freq$PYT_2 == "2"])
prob1_model_123_denominator <- sum(data_freq$fitted_123[data_freq$CZY_KIER == "Tak"])

model_123_prob1 <- prob1_model_123_numerator/prob1_model_123_denominator


prob1_model_12_23_numerator <- sum(data_freq$fitted_12_23[data_freq$CZY_KIER == "Tak" & data_freq$PYT_2 == "2"])
prob1_model_12_23_denominator <- sum(data_freq$fitted_12_23[data_freq$CZY_KIER == "Tak"])

model_12_23_prob1 <- prob1_model_12_23_numerator/prob1_model_12_23_denominator

#___________________________________________________________________________________________________
# 2. Osoba o stażu pracy krótszym niż rok pracuje na stanowisku kierowniczym
#  P(CZY_KIER = Tak | STAŻ = 1) = P(CZY_KIER = Tak i STAŻ = 1)/P(STAŻ = 1)

prob2_numerator <- sum(data_freq$Freq[data_freq$CZY_KIER == "Tak" & data_freq$STAŻ == "1"])
prob2_denominator <- sum(data_freq$Freq[data_freq$STAŻ == "1"])

prob2 <- prob2_numerator / prob2_denominator

prob2_model_123_numerator <- sum(data_freq$fitted_123[data_freq$CZY_KIER == "Tak" & data_freq$STAŻ == "1"])
prob2_model_123_denominator <- sum(data_freq$fitted_123[data_freq$STAŻ == "1"])

model_123_prob2 <- prob2_model_123_numerator/prob2_model_123_denominator


prob2_model_12_23_numerator <- sum(data_freq$fitted_12_23[data_freq$CZY_KIER == "Tak" & data_freq$STAŻ == "1"])
prob2_model_12_23_denominator <- sum(data_freq$fitted_12_23[data_freq$STAŻ == "1"])

model_12_23_prob2 <- prob2_model_12_23_numerator/prob2_model_12_23_denominator
#___________________________________________________________________________________________________
# 3. Osoba o stażu pracy powyżej trzech lat nie pracuje na stanowisku kierowniczym
# P(CZY_KIER = Nie | STAŻ = 3) = P(CZY_KIER = Nie i STAŻ = 3)/P(STAŻ = 3)

prob3_numerator <- sum(data_freq$Freq[data_freq$CZY_KIER == "Nie" & data_freq$STAŻ == "3"])
prob3_denominator <- sum(data_freq$Freq[data_freq$STAŻ == "3"])

prob3 <- prob3_numerator / prob3_denominator

prob3_model_123_numerator <- sum(data_freq$fitted_123[data_freq$CZY_KIER == "Nie" & data_freq$STAŻ == "3"])
prob3_model_123_denominator <- sum(data_freq$fitted_123[data_freq$STAŻ == "3"])

model_123_prob3 <- prob3_model_123_numerator/prob3_model_123_denominator


prob3_model_12_23_numerator <- sum(data_freq$fitted_12_23[data_freq$CZY_KIER == "Nie" & data_freq$STAŻ == "3"])
prob3_model_12_23_denominator <- sum(data_freq$fitted_12_23[data_freq$STAŻ == "3"])

model_12_23_prob3 <- prob3_model_12_23_numerator/prob3_model_12_23_denominator


# wyniki prawdopodobienstw
results_summary <- data.frame(
  Scenariusz = c("P(PYT_2 = 2 | CZY_KIER = Tak)", "P(CZY_KIER = Tak | STAŻ = 1)", "P(CZY_KIER = Nie | STAŻ = 3)"),
  Z_danych = c(prob1, prob2, prob3),
  Model_123 = c(model_123_prob1, model_123_prob2, model_123_prob3),
  Model_12_23 = c(model_12_23_prob1, model_12_23_prob2, model_12_23_prob3)
)

results_summary
xtable(results_summary)

##########################################################################################################
# Zadanie 9. 
# Dla danych wskazanych w zadaniu 7 zweryfikuj nastepujace hipotezy:
#   • zmienne losowe CZY_KIER, PYT_2 i STAZ sa wzajemnie niezalezne;
#   • zmienna losowa PYT_2 jest niezalezna od pary zmiennych CZY_KIER i STAZ;
#   • zmienna losowa PYT_2 jest niezalezna od zmiennej CZY_KIER, przy ustalonej
#     wartosci zmiennej STAZ.


# przyjmujemy alpha = 0.05

#________________________________________________________________________________________________________
# • zmienne losowe CZY_KIER, PYT_2 i STAZ sa wzajemnie niezalezne;
# H_0: M_0 = [1 2 3]
model_1_2_3 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ, data = new_data, family = poisson)

# H_1: M = [123] - wyliczony w 8
model_123 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ + 
                        (CZY_KIER*PYT_2) + 
                        (CZY_KIER*STAŻ) + 
                        (PYT_2*STAŻ) +
                        (CZY_KIER*PYT_2*STAŻ), data = new_data, family = poisson)
anova_test_1 <- anova(model_1_2_3, model_123)
anova_p_val_1 <- 1 - pchisq(anova_test_1$Deviance[2], df = anova_test_1$Df[2])
anova_p_val_1 
# p_val < 0.05 wiec odrzucamy H_0

# H_1: M = [12 23 31]
model_12_23_31 <- glm(Freq ~ (CZY_KIER + PYT_2 + STAŻ)^2, data = new_data, family = poisson)
anova_test_2 <- anova(model_1_2_3, model_12_23_31)
anova_p_val_2 <- 1 - pchisq(anova_test_2$Deviance[2], df = anova_test_2$Df[2])
anova_p_val_2
# p_val < 0.05 wiec odrzucamy H_0

# odrzucamy wszedzie hipoteze o niezaleznosci!
#________________________________________________________________________________________________________
# • zmienna losowa PYT_2(2) jest niezalezna od pary zmiennych CZY_KIER(1) i STAZ(3);
# H_0: M_0 = [12 3] 


model_1_23 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ +
                         (CZY_KIER*STAŻ), data = new_data, family = poisson)
# H_1: M = [123]
anova_test_3 <- anova(model_1_23, model_123)
anova_p_val_3 <- 1 - pchisq(anova_test_3$Deviance[2], df = anova_test_3$Df[2])
anova_p_val_3
# p_val > 0.05 wiec nie mamy podstaw do odrzucenia H_0

# H_1: M = [12 23 31]
anova_test_4 <- anova(model_1_23, model_12_23_31)
anova_p_val_4 <- 1 - pchisq(anova_test_4$Deviance[2], df = anova_test_4$Df[2])
anova_p_val_4

# p-val < 0.05, odrzucamy H_0
#________________________________________________________________________________________________________
# • zmienna losowa PYT_2 jest niezalezna od zmiennej CZY_KIER, przy ustalonej
#   wartosci zmiennej STAZ.

# H_0: M_0 = [12 23]
model_12_23 <- glm(Freq ~ CZY_KIER + PYT_2 + STAŻ +
                     (CZY_KIER*STAŻ) + 
                     (PYT_2*STAŻ), data = new_data, family = poisson)

# H_1: M = [123]
p_val_1 <- 1 - pchisq(deviance(model_12_23) - deviance(model_123), 
                      df = df.residual(model_12_23) - df.residual(model_123))
p_val_1
# p_val > 0.05 wiec nie mamy podstaw do odrzucenia H_0


# H_1: M = [12 23 31]
p_val_2 <- 1 - pchisq(deviance(model_12_23) - deviance(model_12_23_31), 
                      df = df.residual(model_12_23) - df.residual(model_12_23_31))
p_val_2
# p_val > 0.05 wiec nie mamy podstaw do odrzucenia H_0



# WYNIKI
table1 <- data.frame(
  Hipoteza = "CZY_KIER, PYT_2 i STAZ są wzajemnie niezależne",
  Model = c("[123]", "[12 23 31]"),
  p_value = c(anova_p_val_1, anova_p_val_2),
  Decyzja = c(ifelse(anova_p_val_1 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"),
              ifelse(anova_p_val_2 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"))
)


table2 <- data.frame(
  Hipoteza = "PYT_2 jest niezależna od pary CZY_KIER i STAZ",
  Model = c("[123]", "[12 23 31]"),
  p_value = c(anova_p_val_3, anova_p_val_4),
  Decyzja = c(ifelse(anova_p_val_3 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"),
              ifelse(anova_p_val_4 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"))
)


table3 <- data.frame(
  Hipoteza = "PYT_2 jest niezależna od CZY_KIER przy ustalonej wartości STAZ",
  Model = c("[123]", "[12 23 31]"),
  p_value = c(p_val_1, p_val_2),
  Decyzja = c(ifelse(p_val_1 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"),
              ifelse(p_val_2 < 0.05, "Odrzucamy H_0", "Nie odrzucamy H_0"))
)



table1

table2

table3



xtable(table1)


xtable(table2)


xtable(table3)



  
