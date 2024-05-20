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

data$CZY_ZADOW <- ifelse(data$PYT_2 %in% c("-2", "-1"), "NIE", 
                       "TAK")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ CZĘŚĆ I @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Zadanie 11
# Przeprowadzone wsród brytyjskich mezczyzn badanie trwajace 20 lat wykazało, ze
# odsetek zmarłych (na rok) z powodu raka płuc wynosił 0,00140 wsród osób palacych papierosy
# i 0,00010 wsród osób niepalacych. Odsetek zmarłych z powodu choroby niedokrwiennej
# serca wynosił 0,00669 dla palaczy i 0,00413 dla osób niepalacych. Opisz zwiazek pomiedzy
# paleniem papierosów a smierci ˛a z powodu raka płuc oraz zwiazek pomiedzy paleniem 
# papierosów a smierci ˛a z powodu choroby serca. Skorzystaj z róznicy proporcji, ryzyka
# wzglednego i ilorazu szans. Zinterpretuj wartosci. Zwiazek której pary zmiennych jest
# silniejszy?

deaths_data <- matrix(c(0.00140, 0.00010, 0.00669, 0.00413), nrow=2, byrow=TRUE)
colnames(deaths_data) <- c("Palący", "Niepalący")
rownames(deaths_data) <- c("Rak płuc", "Choroba serca")
print(deaths_data)


diff_proportion <- deaths_data[, "Palący"] - deaths_data[, "Niepalący"]
# diff_proportion <- deaths_data[, "Niepalący"] - deaths_data[, "Palący"] 

RR <- deaths_data[, "Palący"] / deaths_data[, "Niepalący"]

chance_1 <- deaths_data[, "Palący"] / (1 - deaths_data[, "Palący"])
chance_2<- deaths_data[, "Niepalący"] / (1 - deaths_data[, "Niepalący"])
OR <- chance_1 / chance_2


result <- data.frame(
  Różnica_proporcji = diff_proportion,
  RR = RR,
  OR = OR
)                                                                                                       


print(result)

xtable(result)

# z RR: proporcja osob palących wsrod osob majacych raka pluc  jest 14 razy wieksza niz
# proporcja osob niepalacych
# proporcja osob z choroba serca tylko 1.619855 razy wieksza

# Z OR: szansa na wystąpienie raka pluc dla osoby palacej jest 14.018226 razy wieksza
# niz dla niepalacej
# dla osob z choroba serca szansa jest 1.624029 razy wieksza


# Wnioski:
# Związek między paleniem papierosów a śmiercią z powodu raka płuc jest znacznie 
# silniejszy niż związek między paleniem papierosów a śmiercią z powodu choroby serca, 
# co wynika z wyższych wartości ryzyka względnego i ilorazu szans dla raka płuc.


##########################################################################################################
# Zadanie 12
# Tabela 1 przedstawia wyniki dotyczące śmiertelności kierowców i pasażerów w wypadkach 
# samochodowych na Florydzie w 2008 roku, w zależności od tego, czy osoba miała zapięty 
# pas bezpieczeństwa czy nie.

car_crash <- matrix(c(1085, 55623, 703, 441239), nrow=2, byrow=TRUE)
colnames(car_crash) <- c("Śmiertelny", "Nieśmiertelny")
rownames(car_crash) <- c("Bez pasów", "Z pasami")
print(car_crash)
# ___________________________________________________________________________________________

# a)
# Oszacuj warunkowe prawdopodobieństwo śmierci w wypadku ze względu na drugą zmienną, tj.
# dla kierowców i pasażerów, który użyli pasa bezpieczeństwa oraz dla kierowców i pasażerów, 
# który nie użyli pasa bezpieczeństwa.

prob_death_no_seatbelt <- car_crash["Bez pasów", "Śmiertelny"] / sum(car_crash["Bez pasów", ])

prob_death_seatbelt <- car_crash["Z pasami", "Śmiertelny"] / sum(car_crash["Z pasami", ])

# Wyniki
cat("Warunkowe prawdopodobieństwo śmierci bez pasów:", prob_death_no_seatbelt, "\n")
cat("Warunkowe prawdopodobieństwo śmierci z pasami:", prob_death_seatbelt, "\n")

# ____________________________________________________________________________________________

# b)
# Oszacuj warunkowe prawdopodobieństwo użycia pasa bezpieczeństwa ze względu na drugą zmienną,
# tj. dla kierowców i pasażerów ze śmiertelnymi obrażeniami oraz dla kierowców i pasażerów, 
# którzy przeżyli wypadek.

prob_seatbelt_fatal <- car_crash["Z pasami", "Śmiertelny"] / sum(car_crash[ , "Śmiertelny"])

prob_seatbelt_nonfatal <- car_crash["Z pasami", "Nieśmiertelny"] / sum(car_crash[ , "Nieśmiertelny"])

# Wyniki
cat("Warunkowe prawdopodobieństwo użycia pasa bezpieczeństwa w wypadkach śmiertelnych:", prob_seatbelt_fatal, "\n")
cat("Warunkowe prawdopodobieństwo użycia pasa bezpieczeństwa w wypadkach nieśmiertelnych:", prob_seatbelt_nonfatal, "\n")


# ____________________________________________________________________________________________

# c)
# Jaki jest najbardziej naturalny wybór dla zmiennej objaśnianej w tym badaniu? Dla takiego
# wyboru wyznacz i zinterpretuj różnicę proporcji, ryzyko względne oraz iloraz szans.
# Dlaczego wartości ryzyka względnego i ilorazu szans przyjmują zbliżone wartości?

# objasniania to (chyba) z pasami (X to czy ma pasy)

diff_proportion_a <-  prob_death_no_seatbelt - prob_death_seatbelt

RR_a <-  prob_death_no_seatbelt / prob_death_seatbelt

chance_1_a <- prob_death_no_seatbelt  / (1 - prob_death_no_seatbelt )
chance_2_a <- prob_death_seatbelt  / (1 - prob_death_seatbelt)
OR_a <- chance_1_a / chance_2_a


# objasniana to chyba smiertelny ( Y to smiertelny)

diff_proportion_b <-   prob_seatbelt_nonfatal - prob_seatbelt_fatal

RR_b <-  prob_seatbelt_nonfatal / prob_seatbelt_fatal

chance_1_b <- prob_seatbelt_nonfatal  / (1 - prob_seatbelt_nonfatal)
chance_2_b <- prob_seatbelt_fatal  / (1 - prob_seatbelt_fatal )
OR_b <- chance_1_b / chance_2_b

# bardziej naturalne jest a)


result <- data.frame(
  Przypadek = c("Z pasami vs Bez pasów (śmierć)", "Śmiertelne vs Nieśmiertelne (pasy)"),
  Różnica_proporcji = c(diff_proportion_a, diff_proportion_b),
  RR = c(RR_a, RR_b),
  OR = c(OR_a, OR_b)
)


print(result)

xtable(result)

# Wyjaśnienie podobieństwa wartości RR i OR ( z chatu)
# Wartości RR i OR są zbliżone, gdy prawdopodobieństwo zdarzenia (w tym przypadku śmierci) jest małe.
# Wynika to z matematycznych właściwości ilorazu szans i ryzyka względnego, gdzie iloraz szans zbliża 
# się do ryzyka względnego w miarę zmniejszania się prawdopodobieństwa zdarzenia.


##########################################################################################################
# Zadanie 13
# Oblicz wartości odpowiednich miar współzmienności (współczynnik tau lub współczynnik gamma) 
# dla zmiennych:

library(DescTools)

help("GoodmanKruskalTau")
help("GoodmanKruskalGamma")

# gamma od - 1 do 1, 0 oznacza sile niezaleznosc, 1 to doskonala zaleznosc
# tau od 0 do 1, 0 oznacza sile niezaleznosc, 1 to doskonala zaleznosc
# ____________________________________________________________________________________________

# • zadowolenie z wynagrodzenia w pierwszym badanym okresie i zajmowane stanowisko,
# !! mamy zmienne nominalne - nie maja naturalnego porzadku (np. jak tutaj (TAK i NIE))
# dlatego skorzystamy z tau
table_1 <- table(data$CZY_ZADOW, data$CZY_KIER)
GoodmanKruskalTau(table_1, direction = "row")
# 0.0004091802  bliskie 0 wiec niezalezne (siła zaleznosci jest bardzo mala)

GoodmanKruskalTau(table_1, direction = "column")
# ____________________________________________________________________________________________

# • zadowolenie z wynagrodzenia w pierwszym badanym okresie i staz pracy, 
# !! mamy zmienne porządkowe - maja naturalny porzadek (np. u nas liczby)
# stad skorzystamy z gammy
table_2 <- table(data$PYT_2, data$STAŻ)
GoodmanKruskalGamma(table_2)
# 0.09084262 mala siła zaleznosci

# ____________________________________________________________________________________________

# • zajmowane stanowisko i staz pracy
# mamy mieszanke, wiec sprowadzimy staz do postaci nominalnej i uzyjemy tau
data$STAŻ_CZY_DŁUGI <- ifelse(data$STAŻ <= 2, "Nie", "Tak")

table_3 <- table(data$CZY_KIER, data$STAŻ_CZY_DŁUGI)
GoodmanKruskalTau(table_3, direction = "row")
# niewielka zaleznosc pomiedzy danymi
GoodmanKruskalTau(table_3, direction = "column")

##########################################################################################################
# Zadanie 14
# Na podstawie informacji przedstawionych na wykładzie napisz własną funkcję do 
# przeprowadzania analizy korespondencji. Funkcja powinna przyjmować jako argument
# tablicę dwudzielczą i zwracać obliczone wartości odpowiednich wektorów i macierzy,
# współrzędnych punktów oraz odpowiedni wykres. Korzystając z napisanej funkcji wykonaj
# analizę korespondencji dla danych dotyczących zadowolenia z wynagrodzenia w pierwszym
# badanym okresie i stażu pracy.

table <- table(data$PYT_2, data$STAŻ)
table <- rbind(table, colSums(table))
table <- cbind(table, rowSums(table))
print(table)

correspondence_analysis <- function(x, plot_type = "principal-principal") {
  nrows <- dim(x)[1]
  ncols <- dim(x)[2]
  
  n <- x[nrows, ncols]
  
  p <- x / n
  P <- p[1:(nrows - 1), 1:(ncols - 1)]
  
  r <- p[1:(nrows - 1), ncols]
  c <- p[nrows, 1:(ncols - 1)]
  
  D_r <- diag(r)
  D_c <- diag(c)
  
  R <- solve(D_r) %*% P
  C <- P %*% solve(D_c)
  
  
  A <- solve(sqrt(D_r)) %*% (P - r %*% t(c)) %*% solve(sqrt(D_c))
  svd_A <- svd(A)
  U <- svd_A$u
  V <- svd_A$v
  Gamma <- svd_A$d
  
  F <- solve(sqrt(D_r)) %*% U %*% diag(Gamma)
  G <- solve(sqrt(D_c)) %*% V %*% diag(Gamma)
  
  F_std <- solve(sqrt(D_r)) %*% U 
  G_std <- solve(sqrt(D_c)) %*% V
  
  lambda <- sum(Gamma^2)
  
  epsilon <- (Gamma^2) / lambda
  
  result <- list(F_principal = F, 
                 G_principal = G,
                 F_standard = F_std,
                 G_standard = G_std,
                 Inercja_całkowita = lambda, 
                 Inercja_główna = epsilon)
  
  if (plot_type %in% c("principal-principal", "standard-standard", "standard-principal", "principal-standard")) {
    data_F <- data.frame(Dimension1 = F[, 1], Dimension2 = F[, 2], label = rownames(x)[1:(nrows-1)], group = "F")
    data_G <- data.frame(Dimension1 = G[, 1], Dimension2 = G[, 2], label = colnames(x)[1:(ncols-1)], group = "G")
    data_F_std <- data.frame(Dimension1 = F_std[, 1], Dimension2 = F_std[, 2], label = rownames(x)[1:(nrows-1)], group = "F_std")
    data_G_std <- data.frame(Dimension1 = G_std[, 1], Dimension2 = G_std[, 2], label = colnames(x)[1:(ncols-1)], group = "G_std")
    
    if (plot_type == "principal-principal") {
      data <- rbind(data_F, data_G)
      title <- "Analiza Korespondencji (principal-principal) - własna implementacja"
    } else if (plot_type == "standard-standard") {
      data <- rbind(data_F_std, data_G_std)
      title <- "Analiza Korespondencji (standard-standard) - własna implementacja"
    } else if (plot_type == "standard-principal") {
      data <- rbind(data_F_std, data_G)
      title <- "Analiza Korespondencji (standard-principal) - własna implementacja"
    } else if (plot_type == "principal-standard") {
      data <- rbind(data_F, data_G_std)
      title <- "Analiza Korespondencji (principal-standard) - własna implementacja"
    }
    
    plot <- ggplot(data, aes(x = Dimension1, y = Dimension2, color = group, label = label, shape = group)) +
      geom_point(size = 3) +
      geom_text(size = 4.5, hjust = -0.4, vjust = -0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Wymiar 1", y = "Wymiar 2", title = title) +
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5)) +  
      xlim(c(min(data$Dimension1) - 0.1, max(data$Dimension1) + 0.1)) +
      ylim(c(min(data$Dimension2) - 0.1, max(data$Dimension2) + 0.1)) +
      scale_color_manual(values = c("F" = "blue", "G" = "red", "F_std" = "blue", "G_std" = "red")) + 
      scale_shape_manual(values = c(16, 17)) +
      guides(color = "none", shape = "none")
    
    print(plot)
    
    filename <- sprintf("%s.png", plot_type)
    ggsave(filename = filename, plot = plot, device = "png", height = 6, width = 9)
  }
  
  
  return(result)
}


plot_types <- c("principal-principal", "standard-standard", "standard-principal", "principal-standard")

for (plot_type in plot_types) {
  result <- correspondence_analysis(table, plot_type = plot_type)
  print(result)
}


# porównanie z wbudowana
library(ca) 
help(ca)

ca_built <- ca(table(data$PYT_2, data$STAŻ))
ca_built

plot(ca_built , main="Analiza korespondencji - wbudowana")



