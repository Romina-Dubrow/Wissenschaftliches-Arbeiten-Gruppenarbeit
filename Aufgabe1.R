## Aufgabe 1

## Erstellen eines Dataframes mit der Spalte ID fuer 100 Beobachtungen

sim <- data.frame(ID = 1:100) 


# a)
# Alter

## Fuer Nachvollziehbarkeit wird fuer jede Teilaufgabe ein seed verwendet

set.seed(0957) 

## Simulation des Alters mittels Zufallszahlen der Normalverteilung. Zusaetlich wird das Alter
## auf einen ganzzahligen Wert gerundet.

Alter <- round(rnorm(100, mean = 25, sd = 2)) 

## Hinzufuegen zum Dataframe in neuer Spalte

sim$Alter <- Alter


# b)
# Studienfach

## Erstellen eines Vektors mit allen moeglichen Faechern

faecher <- c("Statistik", "Data Science", "Informatik", "Mathematik")

## Zufaelliges Ziehen aus Vektor faecher mit W'keits-Vektor prob und Umwandeln in Faktor

set.seed(1003)
sim$Studienfach <- as.factor(sample(faecher, 100, prob = c(0.35, 0.35, 0.2, 0.1), replace = TRUE))



# c)
# Interesse an Mathematik

## Auspraegung des Interesse (wobei 1 gering und 7 sehr stark)

interesse <- 1:7

## Erstellung von Wahrscheinlichkeitsvektoren zu den einzelnen Faechern

pm_stat <- c(0, 0.05, 0.1, 0.2, 0.35, 0.2, 0.1)  ## W'keits-Vektor Statistik
pm_data <- c(0.05, 0.15, 0.2, 0.25, 0.2, 0.15, 0)  ## W'keits-Vektor Data Science
pm_info <- c(0.1, 0.15, 0.3, 0.2, 0.15, 0.1, 0) ## ... Informatik
pm_mathe <- c(0, 0, 0.05, 0.15, 0.3, 0.3, 0.2) ## ... Mathematik

## Schachtelung mehrerer ifelse-Konstruktionen zur Simulation des Interesses an Mathematik
## und anschliessendes zufaelliges Ziehen nach Fach und dazugehoerigen W'keitsvektoren

set.seed(1015)
IntM <- ifelse(sim$Studienfach == "Statistik", 
          sample(interesse, sum(sim$Studienfach == "Statistik"), prob = pm_stat, replace = T),
          ifelse(sim$Studienfach == "Data Science",
              sample(interesse, sum(sim$Studienfach == "Data Science"), prob = pm_data, replace = T),
              ifelse(sim$Studienfach == "Informatik",
                  sample(interesse, sum(sim$Studienfach == "Informatik"), prob = pm_info, replace = T),
                  sample(interesse, sum(sim$Studienfach == "Mathematik"), prob = pm_mathe, replace = T))))
sim$"Intersse an Mathematik" <- IntM


# d)
# Interesse an Programmieren

## Wahrscheinlichkeitsvektoren analog zu c)

pi_stat <- c(0, 0.1, 0.2, 0.4, 0.2, 0.1, 0)  ## W'keits-Vektor Statistik
pi_data <- c(0, 0.05, 0.1, 0.2, 0.3, 0.2, 0.15)  ## W'keits-Vektor Data Science
pi_info <- c(0, 0, 0.05, 0.1, 0.25, 0.3, 0.3) ## ... Informatik
pi_mathe <- c(0.2, 0.25, 0.25, 0.15, 0.1, 0.05, 0) ## ... Mathematik

## Schachtelung mehrerer ifelse-Konstruktionen und anschliessendes zufaelliges Ziehen
## nach Fach und dazugehoerigen W'keitsvektoren. Vektor interesse aus c)

set.seed(1022)
IntP <- ifelse(sim$Studienfach == "Statistik", 
          sample(interesse, sum(sim$Studienfach == "Statistik"), prob = pi_stat, replace = T),
          ifelse(sim$Studienfach == "Data Science",
              sample(interesse, sum(sim$Studienfach == "Data Science"), prob = pi_data, replace = T),
              ifelse(sim$Studienfach == "Informatik",
                  sample(interesse, sum(sim$Studienfach == "Informatik"), prob = pi_info, replace = T),
                  sample(interesse, sum(sim$Studienfach == "Mathematik"), prob = pi_mathe, replace = T))))
sim$"Intersse an Programmieren" <- IntP


# e)
# Mathe - LK

## Schachtelung mehrerer ifelse-Konstruktionen und anschliessendes zufaelliges Ziehen
## nach Fach und dazugehoerigen W'keitsvektoren, bzw. Festlegen der Antwort bei spezifischen
## Angaben zu Fach und Interesse an Mathematik, sowie Umwandeln in Faktor

set.seed(1034)
MLK <-ifelse(sim$`Intersse an Mathematik` > 5, "Ja",
        ifelse(sim$Studienfach == "Mathematik", "Ja",
          ifelse(sim$Studienfach == "Statistik", 
            sample(c("Ja", "Nein"), sum(sim$Studienfach == "Statistik" & sim$`Intersse an Mathematik` <= 5),
                   prob = c(0.3, 0.7), replace = T),
            ifelse(sim$Studienfach == "Data Science",
              sample(c("Ja", "Nein"), sum(sim$Studienfach == "Data Science" & sim$`Intersse an Mathematik` <= 5),
                    prob = c(0.2, 0.8), replace = T),
              sample(c("Ja", "Nein"), sum(sim$Studienfach == "Informatik" & sim$`Intersse an Mathematik` <= 5),
                     prob = c(0.1, 0.9), replace = T)))))
sim$"Mathe-LK" <- as.factor(MLK)



## Exportieren des simulierten Datensatzes als csv-Datei

write.csv(sim, "Datensatz.csv", row.names = FALSE)
