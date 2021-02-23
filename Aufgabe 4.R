#### Analyse des Datensatzes 

zahlenspezialisten <- read.csv("Datensatz.csv", stringsAsFactors = TRUE) # Einladen der Zeichenketten als Faktoren erleichtert folgende Arbeit

str(zahlenspezialisten)

attach(zahlenspezialisten)

### bivariater Zusammenhang von Studienfach und Mathe-LK beschrieben durch Cramér- und Pearson-Kontingenzkoeffizienten (Funktion c) )

zus_kat(Studienfach, Mathe.LK) 
#Nominales Merkmal:
# Kontingenzkoeffizienten...
#...nach Cramér: 0.4147429
#...nach Pearson: 0.5059301 , korrigiert: 0.7154932 

# es scheint einen deutlichen Zusammenhang zwischen dem Studienfach und der Belegung vom Mathe-LK zu geben 


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


