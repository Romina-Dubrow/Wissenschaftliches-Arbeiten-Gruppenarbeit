### Analyse des Datensatzes 

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

