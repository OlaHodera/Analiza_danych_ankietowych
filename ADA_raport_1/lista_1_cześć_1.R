library(ggplot2)
library(tidyr)
library(dplyr)
library(kableExtra)
library(vcd)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ I @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 1. Wczytaj dane i przygotuj je do analizy. Zadbaj o odpowiednie typy zmiennych, zweryfikuj
# czy przyjmują wartości zgodne z powyższym opisem, zbadaj czy nie występują braki w danych.

# wczytanie danych
data <- read.csv("ankieta.csv", fileEncoding = "Latin1", sep=";", na=c(""))

# zmiana nazw na zgodne z poleceniem
colnames(data) <- c('DZIAŁ','STAŻ','CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PŁEĆ', 'WIEK')

# zbadanie czy dane maja odpowiednie typy danych
types <- sapply(data, class)
types_df <- data.frame(Zmienna = names(types), Typ = types, row.names = NULL)

kable(types_df, 
      caption = "Typy zmiennych w kolumnach", 
      align = "c",
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


# zbadanie wystepowania braku danych
colSums(is.na(data))
# mamy - brakow danych wszedzie

# ________________________________ dodatki _________________________________________

data$PYT_1 <- factor(data$PYT_1, levels = c(-2, -1, 0, 1, 2),
                   labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "nie mam zdania", "zgadzam się", "zdecydowanie się zgadzam"))
data$PYT_2 <- factor(data$PYT_2, levels = c(-2, -1, 1, 2),
                   labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie się zgadzam"))
data$PYT_3 <- factor(data$PYT_3, levels = c(-2, -1, 1, 2),
                   labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie się zgadzam"))

View(data)


##########################################################################################################
# 2. Utwórz zmienną WIEK_KAT przeprowadzając kategoryzację zmiennej WIEK korzystając
# z nastąpujących przedziałów: do 35 lat, między 36 a 45 lat, między 46 a 55 lat, powyżej 55 lat.

# Podział wieku na odpowiednie grypy wiekowe
df <- mutate(data, WIEK_KAT = cut(WIEK, breaks = c(0, 35, 45, 55, max(WIEK)),
                                  labels = c("0-35", "36-45", "46-55", "56+")))
View(df) 
head(df)


##########################################################################################################
# 3. Sporządź tablice liczności dla zmiennych: DZIAŁ, STAŻ, CZY_KIER, PŁEĆ, WIEK_KAT.

generate_frequency_table <- function(df, variable) {
  # Tworzenie tablicy licznosci
  result <- table(df[[variable]])
  
  # Wyświetlenie informacji o brakujących danych
  kable(result, 
        caption = paste("Tabela liczności dla zmienne", variable), 
        align = "c",
        col.names = c(variable, "Ilość występowań"),
        booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
}

generate_frequency_table(df, "DZIAŁ")
generate_frequency_table(df, "STAŻ")
generate_frequency_table(df, "CZY_KIER")
generate_frequency_table(df, "PŁEĆ")
generate_frequency_table(df, "WIEK_KAT")


##########################################################################################################
# 4. Sporządź wykresy kołowe oraz wykresy słupkowe dla zmiennych: PYT\_1 oraz PYT\_2.
#_______________________________ PYT_1 ______________________________________
ggplot(df, aes(x = PYT_1)) +
  geom_bar(fill = "#0072B2", color = "black", alpha = 0.7) +  
  labs(title = "Wykres słupkowy dla zmiennej PYT_1", x = "PYT_1", y = "Liczba wystąpień") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data = df, aes(x = "", fill = PYT_1)) +
  geom_bar(color = "white", position="fill", alpha = 0.75) +
  coord_polar("y", start=pi/2) +
  scale_fill_viridis_d(option = "plasma", na.value = "gray50") +
  labs(title = "Rozkład odpowiedzi dla zmiennej PYT_1", fill = "Odpowiedzi") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#_______________________________ PYT_2 ______________________________________
ggplot(df, aes(x = PYT_2)) +
  geom_bar(fill = "#0072B2", color = "black", alpha = 0.7) +  
  labs(title = "Wykres słupkowy dla zmiennej PYT_2", x = "PYT_2", y = "Liczba wystąpień") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 15, hjust = 0.5))

ggplot(data = df, aes(x = "", fill = PYT_2)) +
  geom_bar(color = "white", position="fill", alpha = 0.75) +
  coord_polar("y", start=pi/2) +
  scale_fill_viridis_d(option = "plasma", na.value = "gray50") +
  labs(title = "Rozkład odpowiedzi dla zmiennej PYT_2", fill = "Odpowiedzi") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



##########################################################################################################
# 5. Sporządź tablice wielodzielcze dla par zmiennych: PYT\_1 i DZIAŁ, PYT\_1 i STAŻ, 
# PYT\_1 i CZY_KIER}, PYT_1 i PŁEĆ oraz PYT_1 i WIEK_KAT.

generate_multiplication_table <- function(df, variable_1, variable_2) {
  # Tworzenie tablicy wielodzielczej
  result <- table(df[[variable_1]], df[[variable_2]])
  
  # Wyświetlenie tabeli z odpowiednimi opcjami stylizacji
  kable(result,
        caption = paste("Tabela wielodzielcza dla zmiennych", variable_1, "i", variable_2),
        align = "c",
        booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
}

generate_multiplication_table(df, "PYT_1", "DZIAŁ")
generate_multiplication_table(df, "PYT_1", "STAŻ")
generate_multiplication_table(df, "PYT_1", "CZY_KIER")
generate_multiplication_table(df, "PYT_1", "PŁEĆ")
generate_multiplication_table(df, "PYT_1", "WIEK_KAT")


##########################################################################################################
# 6. Sporządź tablicę wielodzielczą dla pary zmiennych: PYT_2 i PYT_3.

# zwykła wielodzielcza
generate_multiplication_table(df, "PYT_2", "PYT_3")


# wielodzielcza z sumami
generate_multiplication_table_2 <- function(df, variable_1, variable_2) {
  # Tworzenie tablicy licznosci
  result <- table(df[[variable_1]], df[[variable_2]])
  
  # Policzenie sumy dla każdego wiersza
  row_sum <- rowSums(result)
  
  # Policzenie sumy dla każdej kolumny
  col_sum  <- colSums(result)
  
  # Dodanie sum do tabeli jako dodatkowych wierszy i kolumn
  sum_table <- cbind(result, "Suma" = row_sum)
  sum_table <- rbind(sum_table, "Suma" = c(col_sum, ""))
  
  # Zwrócenie tabeli
  kable(sum_table, 
        caption = paste("Tabela liczności dla zmiennych", variable_1, "i", variable_2), 
        align = "c",
        booktabs = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position"))
  
}

generate_multiplication_table_2(df, "PYT_2", "PYT_3")


##########################################################################################################
# 7. Utwórz zmienną CZY_ZADOW na podstawie zmiennej PYT_2 łącząc kategorie
# "nie zgadzam się" i "zdecydowanie się nie zgadzam" oraz "zgadzam się" i "zdecydowanie się zgadzam".

# Tworzenie zmiennej CZY_ZADOW na podstawie zmiennej PYT_2
df$CZY_ZADOW <- ifelse(df$PYT_2 %in% c("zdecydowanie się nie zgadzam", "nie zgadzam się"), "NIE", 
                       "TAK")

head(df)

##########################################################################################################
# 8. Korzystając z funkcji mosaic z biblioteki vcd, sporządź wykresy mozaikowe odpowiadające parom zmiennych:
# CZY_ZADOW i DZIAŁ, CZY_ZADOW i STAŻ, CZY_ZADOW i CZY_KIER, CZY_ZADOW i PŁEĆ oraz CZY_ZADOW i WIEK_KAT.
# Czy na podstawie uzyskanch wykresów można postawić pewne hipotezy dotyczące realicji między powyższymi zmiennymi?
# Spróbuj sformułować kilka takich hipotez.

mosaic( ~ CZY_ZADOW + DZIAŁ, data = df, highlight = TRUE)
mosaic( ~ CZY_ZADOW + STAŻ, data = df, highlight = TRUE)
mosaic( ~ CZY_ZADOW + CZY_KIER, data = df, highlight = TRUE)
mosaic( ~ CZY_ZADOW + PŁEĆ, data = df, highlight = TRUE)
mosaic( ~ CZY_ZADOW + WIEK_KAT, data = df, highlight = TRUE)


