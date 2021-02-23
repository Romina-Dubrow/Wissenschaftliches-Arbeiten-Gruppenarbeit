#### Analyse des Datensatzes 

zahlenspezialisten <- read.csv("Datensatz.csv", stringsAsFactors = TRUE) # Einladen der Zeichenketten als Faktoren erleichtert folgende Arbeit

str(zahlenspezialisten)

attach(zahlenspezialisten)

### bivariater Zusammenhang von Studienfach und Mathe-LK beschrieben durch Cramér- und Pearson-Kontingenzkoeffizienten (Funktion c) )

zus_kat(Studienfach, Mathe.LK) 

# es scheint deutliche Abhängigkeiten zu geben


Daten <- read.csv("Datensatz.csv")

### Kategorisierung der Daten mithilfe von Quantilen (Funktion e) )

## Studienfach und Interesse:
Fach<-split(Daten,Daten$Studienfach)

for (i in 1:4) {
  print("Studienfach")
  print(Fach[[i]]$Studienfach[1])
  print("Mathematikinteresse")
  e(Fach[[i]]$Intersse.an.Mathematik)
  print("Studienfach")
  print(Fach[[i]]$Studienfach[1])
  print("Programmierinteresse")
  e(Fach[[i]]$Intersse.an.Programmieren)
}

## Mathe LK (ja/nein) und Interesse:
LK <- split(Daten,Daten$Mathe.LK)
for (i in 1:2) {
  print("Mathe LK?")
  print(LK[[i]]$Mathe.LK[1])
  print("Mathematikinteresse")
  e(LK[[i]]$Intersse.an.Mathematik)
  print("Mathe LK?")
  print(LK[[i]]$Mathe.LK[1])
  print("Programmierinteresse")
  e(LK[[i]]$Intersse.an.Programmieren)
}

# Visualisierung
katVis(Daten$Intersse.an.Mathematik, Daten$Intersse.an.Programmieren, Daten$Mathe.LK)
## je höher das Interesse.an.Mathematik ist, desto eher Mathe.LK
## bei Interesse an Mathe>=6 haben alle Mathe.LK==Ja
## zwischen Interesse.an.Programmieren und Mathe.LK besteht kein deutlicher Zusammenhang

katVis(Daten$Intersse.an.Mathematik, Daten$Intersse.an.Programmieren, Daten$Studienfach)
## Studienfach==Mathematik hat Interesse.an.Mathematik>=5 (mit zwei Ausreißern bei 3) und Interesse.an.Programmieren<5
## Studienfach==Statistik hat Interesse.an.Mathematik>=4 (mit zwei Aureißern bei 3) und Interesse.an.Programmieren zw. 2 und 6
## Studienfach==Informatik hat Interesse.an.Mathematik zw. 2 und 5 und Interesse.an.Programmieren>4
## Studienfach==Data Science hat Interesse.an.Mathematik zw. 1 und 6 und Interesse.an.Programmieren zw. 2 und 7

katVis(Daten$Intersse.an.Mathematik, Daten$Intersse.an.Programmieren, Daten$Mathe.LK, Daten$Studienfach)
## gesteigertes Interesse an Mathe und geringeres Interesse an Prog spricht für Mathematik oder Statistik, mit Mathe-LK für Mathematik
## gesteigertes Interesse an Mathe und mittleres Interesse an Prog spricht für Statistik, vor allem mit Mathe-LK
## mittleres Interesse an Mathe und gesteigertes Interesse an Prog spricht für Informatik, eher ohne Mathe-LK
## gleich großes Interesse an Mathe und Prog spricht für Data Science, eher ohne Mathe-LK

