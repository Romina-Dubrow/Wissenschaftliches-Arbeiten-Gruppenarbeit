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


Daten <- zahlenspezialisten

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


deskr_kat(Studienfach)
#Nominales Merkmal
# Modalwert: Statistik 
# Phi-Streuungsmaß: 0.8125

## Statistik wird am häufigsten studiert. Die Studeinfächer sind nicht
# gleichverteilt, es gibt jedoch eine hohe Streuung

deskr_kat(Mathe.LK)
#Nominales Merkmal
# Modalwert: Ja 
# Phi-Streuungsmaß: 0.98

## Die meiste gaben an, dass sie Mathe LK hatten. Ob der Mathe LK belegt wurde
# oder nicht scheint gleichverteilt zu sein.

Intresse_Mathe <- ordered(Intersse.an.Mathematik)
Intresse_Prog <- ordered(Intersse.an.Programmieren)

deskr_kat(Intresse_Mathe)
#Ordinales Merkmal
# Modalwert: 5 
# Median: 5 
# Quantile: 
#   0%:   1 
#   25%:  3 
#   50%:  5 
#   75%:  5 
#   100%: 7 
# Phi-Streuungsmaß: 0.694051

## Sowohl der Median als auch der Modalwert von Interesse an Mathematik liegen 
# bei 5 von 7, auch das 75%-Quantil beträgt 5. Die Daten streuen mittel stark

deskr_kat(Intresse_Prog)
#Ordinales Merkmal
# Modalwert: 4 
# Median: 4 
# Quantile: 
#   0%:   1 
#   25%:  3 
#   50%:  4 
#   75%:  6 
#   100%: 7 
# Phi-Streuungsmaß: 0.822884

## Der Median und Modalwert von Intresse an Programmieren betragen 4, welches
# unter den Werten von Intresse an Mathematik liegt. Die Daten streuen hier 
# deutlich stärker als bei den Daten zur Intresse an Mathematik
