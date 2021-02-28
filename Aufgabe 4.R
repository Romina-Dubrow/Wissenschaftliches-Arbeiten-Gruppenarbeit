#### Analyse des Datensatzes 

Daten <- read.csv("Datensatz.csv", stringsAsFactors = TRUE) # Einladen der Zeichenketten als Faktoren erleichtert folgende Arbeit


attach(Daten)

length(ID) # [1] 100
## Daten von 100 Personen

deskr(Alter)
# Deskriptive Statistiken: 
# ------------------------
# Arithm. Mittel: 25.06 
# Median: 25 
# Varianz: 5.289293 
# Std.abw.: 2.299846 
# Range: 20 32 
# Quantile: 
#   0%:   20 
#   25%:  23.5 
#   50%:  25 
#   75%:  26.5 
#   100%: 32 
# ------------------------

## Die Studierenden sind 20 bis 32 Jahre alt, bei einem Durchschnittsalter von 
# ca. 25. Die Standardabweichung beträgt ungefähr 2.3.


table(Studienfach)
# Studienfach
# Data Science   Informatik   Mathematik    Statistik 
#           30           20           15           35 

deskr_kat(Studienfach)
#Nominales Merkmal
# Modalwert: Statistik 
# Phi-Streuungsmass: 0.8125

## Statistik wird am haeufigsten studiert, Data Science am zweithaeufigsten 
# und Mathematik am wenigsten. Die Studeinfaecher sind nicht
# gleichverteilt, es gibt jedoch eine hohe Streuung

table(Mathe.LK)
# Mathe.LK
#  Ja Nein 
#  51   49 

deskr_kat(Mathe.LK)
#Nominales Merkmal
# Modalwert: Ja 
# Phi-Streuungsmass: 0.98

## Die meiste gaben an, dass sie Mathe LK hatten (51), 49 hatten keinen Mathe LK
# Ob der Mathe LK belegt wurde oder nicht scheint gleichverteilt zu sein.

Intresse_Mathe <- ordered(Intersse.an.Mathematik)
Intresse_Prog <- ordered(Intersse.an.Programmieren)

table(Intersse.an.Mathematik)
# Intersse.an.Mathematik
# 1  2  3  4  5  6  7 
# 2  7 17 22 30 19  3 

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
# Phi-Streuungsmass: 0.694051

## Sowohl der Median als auch der Modalwert von Interesse an Mathematik liegen 
# bei 5 von 7, auch das 75%-Quantil betraegt 5. Die Daten streuen mittel stark
# Die extremen Werte "1" und "7" wurden nur 5 mal genannt

table(Intersse.an.Programmieren)
# Intersse.an.Programmieren
# 1  2  3  4  5  6  7 
# 5 14 12 25 16 18 10

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
# Phi-Streuungsmass: 0.822884

## Der Median und Modalwert von Intresse an Programmieren betragen 4, welches
# unter den Werten von Intresse an Mathematik liegt. Die Daten streuen hier 
# deutlich staerker als bei den Daten zur Intresse an Mathematik
# Die extremen Werte "1" und "sieben"7" wurden hier 15 mal genannt

### bivariater Zusammenhang von Studienfach und Mathe-LK beschrieben durch Cramér- und Pearson-Kontingenzkoeffizienten (Funktion c) )

zus_kat(Studienfach, Mathe.LK) 
#Nominales Merkmal:
# Kontingenzkoeffizienten...
#...nach Cramér: 0.4147429
#...nach Pearson: 0.5059301 , korrigiert: 0.7154932 

# es scheint einen deutlichen Zusammenhang zwischen dem Studienfach und der Belegung vom Mathe-LK zu geben, wobei der korrigierte Pearson-Koeffizient
# mehr dafuer spricht

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
## allerdings zeigen Varianz, Standardabweichung und Range groeßere Werte für Mathe-LK==nein, da streuen die Werte also mehr
## die Quantile zeigen ebenfalls eine breitere Streuung für Mathe-LK==nein an, das laesst sich auch anhand der Tabelle und des Plots erkennen
### besonders der Interquartilsabstand (75Q-25Q) zeigt das: fuer ja 26-24=2 und für nein 27-23=4

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

## das Interesse an Mathematik liegt im Median für Mathe-LK==ja eine Stufe hoeher als bei Mathe-LK==nein
## die Range ist bei Mathe-LK==ja um 1 groesser und Minimum und Maximum liegen ein bzw. zwei Stufen hoeher
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
## die Range ist bei Mathe-LK==ja um 1 groesser, das Minimum liegt bei 1 bzw. 2 und das Maximum ist bei beiden 7, 
## sehr grosse Streuung bei beiden Gruppen
## bei Mathe-LK==nein ist der Interquartilsabstand 6-3=3 groesser als bei ja mit 5-3=2

detach(Daten)

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
# Studienfach                              Studienfach
  # Data Science                             Data Science
  # Mathematikinteresse                      Programmierinteresse
  # Quantilbasierte Kategorisierung:         Quantilbasierte Kategorisierung:
  # ------------------------                 ------------------------
  #  Niedrig: 1 2                             Niedrig: 2 3   
  # Mittel: 3 4                               Mittel: 4 5
  # Hoch: 5 6                                 Hoch: 6 7 
  # ------------------------                  ------------------------
  
  ## Mathematikinteresse: sehr stark nicht angegeben, ansonsten alles gleichmaessig verteilt
  ## Programmierinteresse: sehr gering nicht angegeben, ansonsten alles gleichmaessig verteilt.
  # Interpretation: Interesse variiert stark zwischen den Studenten. 
  #                 In der Data Science wird viel programmiert also waere geringes Programmierinteresse von Nachteil
  #                 Bei einem sehr hohen Mathematikinteresse studiert man vielleicht eher Statistik als Data Science?
    
   
  #Studienfach                               Studienfach
  #Informatik                                Informatik
  #Mathematikinteresse                       Programmierinteresse
  #Quantilbasierte Kategorisierung:          Quantilbasierte Kategorisierung: 
  # ------------------------                  ------------------------
  # Niedrig: 2 3                              Niedrig: 4 5 
  # Mittel:                                   Mittel: 6 
  # Hoch: 4 5                                 Hoch: 7 
  #------------------------                   ------------------------
    
  
  ## Mathematikinteresse: keine starken Auspraegungen (1,6,7) also alle Befragten geben an mittleres Mathematikinteresse zu haben
  ## Weder besonders hoch noch besonders niedrig
  ## Programmierinteresse: keine geringen Auspraegungen (1,2,3), also ein niedriges Programmierinteresse ist unter Informatikstudierenden
  #  schon 4,5 (Bei Gleichverteilung wäre das aber schon mittel bis hoch), 75% geben hier an ein sehr hohes Programmierinteresse zu haben
  #  also 6 oder 7. 
  #  Interpretation: Fuer den Studiengang Informatik braucht man ein hohes Programmierinteresse. Also ergibt das schon Sinn.
  #                  Mathematikinteresse scheint in diesem Fall nicht unbedingt von Noeten zu sein.
  
  
  #Studienfach                               Studienfach
  #Mathematik                                Mathematik
  #Mathematikinteresse                       Programmierinteresse
  #Quantilbasierte Kategorisierung:          Quantilbasierte Kategorisierung: 
  # ------------------------                  ------------------------
  # Niedrig: 3 5                             Niedrig: 1 
  # Mittel:                                  Mittel: 2 3
  # Hoch: 6 7                                Hoch: 4
  #------------------------                   ------------------------
  
  
 
  ## Mathematikinteresse: Hauptsaechlich hohes Interesse. 3 (also bei Gleichverteilung fast mittleres Interesse) ist hier
  #                       als geringster Wert angegeben. 
  ## Programmierinteresse:  kleiner 5, die meisten haben 2,3 also eher niedriges Programmierinteresse
  ## Interpretation: Fuer ein Mathematikstudium ist Interesse in diesem Bereich natuerlich von Vorteil 
  #                  deshalb sind die Werte auch so hoch
  #                  Programmierinteresse braucht man als Mathestudent nicht unbedingt, nur wenn man 
  #                  Informatik als Nebenfach nehmen sollte (Aber es scheint so als waere das hier nicht beachtet worden)
   
  #Studienfach                               Studienfach
  #Statistik                                 Statistik
  #Mathematikinteresse                       Programmierinteresse
  #Quantilbasierte Kategorisierung:          Quantilbasierte Kategorisierung: 
  # ------------------------                  ------------------------
  # Niedrig: 3 4                             Niedrig: 2 3
  # Mittel: 5                                Mittel: 4
  # Hoch: 6 7                                Hoch: 5 6
  #------------------------                   ------------------------   
  
  
  ## Mathematikinteresse: Schwaches Interesse gibt es nicht. Ansonsten ist das Interesse relativ gleichmaessig verteilt
  ## Programmierinteresse: Sehr starkes/schwaches Interesse gibt es nicht. Ansonsten gleichmaessig verteilt
  # Interpretation: Statistikstudenten haben mindestens mittleres Interesse an Mathematik. Im Studiengang braucht man das auch.
  #                 Programmierinteresse variiert relativ stark und ist weder extrem stark noch schwach, Programmierung wird benoetigt, 
  #                 aber ist nicht der groesste Teil des Studiums. (Aber auch hier kann man Informatik als Nebenfach waehlen)


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
#Mathe LK?                                  Mathe LK?
 # Ja                                        Ja
 # Mathematikinteresse                       Programmierinteresse
 # Quantilbasierte Kategorisierung:          Quantilbasierte Kategorisierung:
 #  ------------------------                  ------------------------
 # Niedrig: 2 3 4                             Niedrig: 1 2 3 
 # Mittel: 5                                  Mittel: 4 
 #Hoch: 6 7                                   Hoch: 5 6 7 
 # ------------------------                   ------------------------
  
  #Mathematikinteresse: Kein sehr geringes Interesse, Hohes Mathematikinteresse tritt haeufiger auf
  #Programmierinteresse: Sehr gleichmaeßig verteilt wie bei einer Gleichverteilung
  # Interpretation: Menschen mit Mathe LK haben auch hoeheres Interesse an Mathematik, Programmierinteresse scheint
  #                 keine Abhaengigkeit zu haben
  
    
  #Mathe LK?                                  Mathe LK?
  # Nein                                      Nein
  # Mathematikinteresse                       Programmierinteresse
  # Quantilbasierte Kategorisierung:          Quantilbasierte Kategorisierung:
  #  ------------------------                  ------------------------
  # Niedrig: 1 2 3                            Niedrig: 2 3 
  # Mittel: 4                                 Mittel: 4 5
  # Hoch: 5                                   Hoch:  6 7 
  # ------------------------                   ------------------------  
 

#Mathematikinteresse: Das hoechste Interesse ist hier 5
#                     Schwaches Interesse kommt sehr haeufig vor
#Programmierinteresse: kein sehr niedriges programmierinteresse. Sonst ueberall ungefaehr gleichviel
# Interpretation: Menschen, ohne Mathe-LK haben haeufig kein hohes Interesse an Mathematik (sonst haetten sie ja Mathe LK gewaehlt)
#                 Man hat ein etwas hoeheres Programmierinteresse wenn man keinen Mathe Lk hat, als wenn man Mathe LK hat?
 


# Visualisierung
katVis(Daten$Intersse.an.Mathematik, Daten$Intersse.an.Programmieren, Daten$Mathe.LK)
## je hoeher das Interesse.an.Mathematik ist, desto eher Mathe.LK
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
## gleich grosses Interesse an Mathe und Prog spricht für Data Science, eher ohne Mathe-LK




# Fazit:
# Daten von 100 Studierenden.
# Die Studierenden im Datensatz sind 20 bis 32 Jahre alt (Durchschnitt 25, Standardabweichung 2.3).
# Die Studienfaecher sind nicht gleichverteilt. Statistik tritt am haeufigsten auf, Mathe am seltesten.
# Ob die Studierenden Mathe LK hatten oder nicht scheint gleichverteilt.
# Das arithmetische Mittel und der Median des Alters bei Studierenden, die Mathe LK hatten, ist
# aehnlich zu dem der Studierenden, die keinen Mathe LK hatten. Trotzdem streut das Alter bei den Studierenden ohne Mathe LK staerker.
# Mathematikinteresse war im Median und Modalwert 5, Programmierinteresse 4.
# Programmierinteresse streut stärker als Mathematikinteresse.
# Studierende, die Mathe LK hatten, haben ein hoeheres Mathematikinteresse.
# Studierende mit Mathe LK hatten ein gleichverteiltes Programmierinteresse (scheint keinen Zusammenhang zu haben).
# Programmierinteresse ist bei Studierenden ohne Mathe-LK staerker als bei Studierenden mit Mathe LK.
# Es gibt einen deutlichen Zusammenhang zwischen der Wahl des Studienfachs und der Belegung vom Mathe LK.
# Mathematikinteresse nach Studienfaechern: Mathe > Statistik > Data Science > Informatik.
# Programmierinteresse nach Studienfaechern: Informatik > Data Science > Statistik > Mathe.

