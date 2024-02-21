getwd()
library(readxl)
#### Odczyt  danych z pliku arkusza oraz ich obrobka ####
daneraw <- read_excel("tabl46_produkcja_sprzedana_przemyslu.xlsx")
dane <- daneraw[-c(1:3),]
dane <- dane[-2,]
dane0 <- dane[,1]
dane1 <- dane[,8]
dane2 <- dane[,10]
danew <- data.frame(dane0, dane1, dane2)
colnames(danew) <- c("Miesiac", "Przetworstwo przemyslowe", "Produkcja artykulow spozywczych")
danew <- danew[-1,]
danew[,2] <- as.numeric(danew[,2])
danew[,3] <- as.numeric(danew[,3])
danew



#### Cecha nr 1 Przetworstwo przemyslowe ####



#### Minimum C1 ####
minimumc1 <- min(danew$`Przetworstwo przemyslowe`)
minimumc1

#### Maskimum C1####
maksimumc1 <- max(danew$`Przetworstwo przemyslowe`)
maksimumc1

#### Rozstep C1####
rozstepc1 <- maksimumc1 - minimumc1
rozstepc1

#### Srednia C1####

sredniac1 <- mean(danew$`Przetworstwo przemyslowe`)
sredniac1

#### Mediana C1####  

medianac1 <- median(danew$`Przetworstwo przemyslowe`)
medianac1

#### Odchylenie standardowe C1####

odchyleniec1 <- sd(danew$`Przetworstwo przemyslowe`)
odchyleniec1

#### Kwantyle C1####

kwartylec1 <- quantile(danew$`Przetworstwo przemyslowe`)
kwartylec1

#### Wspolczynnik zmiennosci C1####

wszmiennoscic1 <- (odchyleniec1/sredniac1) * 100
wszmiennoscic1

#### Wariancja C1####

wariancjac1 <- var(danew$`Przetworstwo przemyslowe`)
wariancjac1


#### Moment centralny rzedu 3 C1####

library(moments)
M3C1 <- moment(danew$`Przetworstwo przemyslowe`, order=3, central=TRUE)
M3C1


#### Wspolczynnik asymetrii ####

wsp_asymetriic1 <- skewness(danew$`Przetworstwo przemyslowe`)
wsp_asymetriic1

#### Graficzna reprezentacja danych C1####


#### Wykres paskowy ####
library(graphics)
stripchart(danew$`Przetworstwo przemyslowe`)

#### Histogram ####
(hist(danew$`Przetworstwo przemyslowe`, breaks=13))

#### Dystrybuanta ####
plot(ecdf(danew$`Przetworstwo przemyslowe`))


#### Wykres pudełkowy ####
boxplot(danew$`Przetworstwo przemyslowe`)


#### Hipotezy C1####

#hipoteza 1 Średnia wartość jest równa 102000
t.test(danew$`Przetworstwo przemyslowe`,mu = 100000)


# hipoteza 2 dane mają rozkład normalny
shapiro.test(danew$`Przetworstwo przemyslowe`)




#### Cecha nr 2 Produkcja artykulow spozywczych ####




#### Minimum C2####
minimumc2 <- min(danew$`Produkcja artykulow spozywczych`)
minimumc2

#### Maskimum C2####
maksimumc2 <- max(danew$`Produkcja artykulow spozywczych`)
maksimumc2

#### Rozstep C2####
rozstepc2 <- maksimumc2 - minimumc2
rozstepc2

#### Srednia C2####

sredniac2 <- mean(danew$`Produkcja artykulow spozywczych`)
sredniac2

#### Mediana C2####  

medianac2 <- median(danew$`Produkcja artykulow spozywczych`)
medianac2

#### Odchylenie standardowe C2####

odchyleniec2 <- sd(danew$`Produkcja artykulow spozywczych`)
odchyleniec2

#### Kwantyle C2####

kwartylec2 <- quantile(danew$`Produkcja artykulow spozywczych`)
kwartylec2

#### Wspolczynnik zmiennosci C2####

wszmiennoscic2 <- (odchyleniec2/sredniac2) * 100
wszmiennoscic2

#### Wariancja C2####

wariancjac2 <- var(danew$`Produkcja artykulow spozywczych`)
wariancjac2

#### Moment centralny rzedu 3 C2####

library(moments)

M3C2 <- moment(danew$`Produkcja artykulow spozywczych`, order=3, central=TRUE)
M3C2

#### Wspolczynnik asymetrii ####

wsp_asymetriic2 <- skewness(danew$`Produkcja artykulow spozywczych`)
wsp_asymetriic2

#### Graficzna reprezentacja danych C2####


#### Wykres paskowy ####
library(graphics)
stripchart(danew$`Produkcja artykulow spozywczych`)

#### Histogram ####
(hist(danew$`Produkcja artykulow spozywczych`, breaks=13))

#### Dystrybuanta ####
plot(ecdf(danew$`Produkcja artykulow spozywczych`))


#### Wykres pudełkowy ####
boxplot(danew$`Produkcja artykulow spozywczych`)


#### Hipotezy C2####

#hipoteza 1 Średnia wartość jest równa 19000
t.test(danew$`Produkcja artykulow spozywczych`,mu = 19000)


# hipoteza 2 dane mają rozkład normalny
shapiro.test(danew$`Produkcja artykulow spozywczych`)
