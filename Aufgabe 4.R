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


deskr_d(Mathe.LK, Alter)
# Deskriptive Statistiken: 
#   ------------------------
#   Arithm. Mittel (Ja): 24.92157 
# Arithm. Mittel (Nein): 25.20408 
# Median (Ja): 25 
# Median (Nein): 25 
# Varianz (Ja): 4.713725 
# Varianz (Nein): 5.957483 
# Std.abw.(Ja): 2.171112 
# Std.abw.(Nein): 2.440796 
# Range (Ja): 20 30 
# Range (Nein): 21 32 
# Quantile : 
#   0% (für Ja):   20 
# 25%:  24 
# 50%:  25 
# 75%:  26 
# 100%: 30 
# 0% (für Nein):   21 
# 25%:  23 
# 50%:  25 
# 75%:  27 
# 100%: 32 
# 
# ------------------------
#   y
# x     20  21  22  23  24  25  26  27  28  29  30  32 Sum
# Ja     1   0   7   3  10  16   3   4   2   4   1   0  51
# Nein   0   3   4   7   4   8   9   8   1   3   1   1  49
# Sum    1   3  11  10  14  24  12  12   3   7   2   1 100

## das arithmetische Mittel und der Median des Alters sind in beiden Gruppen (Mathe-LK ja/nein) in etwa gleich (um 25)
## allerdings zeigen Varianz, Standardabweichung und Range größere Werte für Mathe-LK==nein, da streuen die Werte also mehr
## die Quantile zeigen ebenfalls eine breitere Streuung für Mathe-LK==nein an, das lässt sich auch anhand der Tabelle und des Plots erkennen
### besonders der Interquartilsabstand (75Q-25Q) zeigt das: für ja 26-24=2 und für nein 27-23=4

# für die Interessen-Variablen als metrische Variablen, keine Interpretation von arithm. Mittel, Varianz und Stand.Abw.
deskr_d(Mathe.LK, Intersse.an.Mathematik)
# Deskriptive Statistiken: 
#   ------------------------
#   Arithm. Mittel (Ja): 5.039216 
# Arithm. Mittel (Nein): 3.734694 
# Median (Ja): 5 
# Median (Nein): 4 
# Varianz (Ja): 1.478431 
# Varianz (Nein): 1.32398 
# Std.abw.(Ja): 1.215908 
# Std.abw.(Nein): 1.150643 
# Range (Ja): 2 7 
# Range (Nein): 1 5 
# Quantile : 
#   0% (für Ja):   2 
# 25%:  4 
# 50%:  5 
# 75%:  6 
# 100%: 7 
# 0% (für Nein):   1 
# 25%:  3 
# 50%:  4 
# 75%:  5 
# 100%: 5 
# 
# ------------------------
#   y
# x       1   2   3   4   5   6   7 Sum
# Ja     0   1   7   6  15  19   3  51
# Nein   2   6  10  16  15   0   0  49
# Sum    2   7  17  22  30  19   3 100

## das Interesse an Mathematik liegt im Median für Mathe-LK==ja eine Stufe höher als bei Mathe-LK==nein
## die Range ist bei Mathe-LK==ja um 1 größer und Minimum und Maximum liegen ein bzw. zwei Stufen höher
## die Quantile zeigen eine fast gleiche Streuung bei unterschiedlicher Lage (vgl. Range)


deskr_d(Mathe.LK, Intersse.an.Programmieren)
# Deskriptive Statistiken: 
#   ------------------------
#   Arithm. Mittel (Ja): 3.960784 
# Arithm. Mittel (Nein): 4.591837 
# Median (Ja): 4 
# Median (Nein): 5 
# Varianz (Ja): 2.678431 
# Varianz (Nein): 2.913265 
# Std.abw.(Ja): 1.636591 
# Std.abw.(Nein): 1.706829 
# Range (Ja): 1 7 
# Range (Nein): 2 7 
# Quantile : 
#   0% (für Ja):   1 
# 25%:  3 
# 50%:  4 
# 75%:  5 
# 100%: 7 
# 0% (für Nein):   2 
# 25%:  3 
# 50%:  5 
# 75%:  6 
# 100%: 7 
# 
# ------------------------
#   y
# x      1   2   3   4   5   6   7 Sum
# Ja     5   6   6  16   6  11   1  51
# Nein   0   8   6   9  10   7   9  49
# Sum    5  14  12  25  16  18  10 100

## das Interesse an Programmieren liegt im Median für Mathe-LK==ja eine Stufe unter dem für Mathe-LK==nein
## die Range ist bei Mathe-LK==ja um 1 größer, das Minimum liegt bei 1 bzw. 2 und das Maximum ist bei beiden 7, 
## sehr große Streuung bei beiden Gruppen
## bei Mathe-LK==nein ist der Interquartilsabstand 6-3=3 größer als bei ja mit 5-3=2

