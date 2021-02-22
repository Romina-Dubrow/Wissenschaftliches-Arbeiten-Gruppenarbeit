Gruppenarbeit GitHub Repository Wissenschaftliches Arbeiten <br>
Einführung: 

Gruppenmitglieder:

Leia Betting, Romina Dubrow, Kathrin Henkenherm, Anna Herdick, Tim Ritter und Luca Sauer 

Kontakt: leia.betting\@tu.dortmund.de, romina.dubrow\@tu-dortmund.de, kathrin.henkenherm\@tu-dortmund.de, ... , luca.sauer@tu-dortmund.de

--------------------------------------------------------------------------------

### Aktueller Status: <br>
Aufgabe 1 erledigt <br>
Aufgabe 2 erledigt <br>
Aufgabe 3 erledigt <br>
Aufgabe 4 in Bearbeitung <br>
Aufgabe 5 in Bearbeitung 

--------------------------------------------------------------------------------

### Inhalt: <br>
**Aufgabe 1** (bearbeitet von Luca Sauer und Tim Ritter) <br>

Erstellen eines Datensatzes 

**Aufgabe 2** (bearbeitet von Luca Sauer und Tim Ritter) <br>

Hochladen des Datensatzes in csv Format

**Aufgabe 3** (bearbeitet von allen Gruppenmitgliedern) <br>

Erstellung eines R-Skripts mit verschiedenen Funktionen zur Analyse des Datensatzes <br>
Erstellung eines R-Skripts mit Helferfunktionen für das erste Skript

**Aufgabe 4** (bearbeitet von Leia Betting, Romina Dubrow, Kathrin Henkenherm und Anna Herdick) <br>

Analyse des Datensatzes

**Aufgabe 5** (bearbeitet von allen Gruppenmitgliedern) <br>

Diskussion der Ergebnisse und Erklärung der vorliegenden Zusammenhänge

--------------------------------------------------------------------------------

Es wurde R und R Studio in diesen Versionen genutzt:  
 R Studio 1.2.5042 
 R 4.0.3
 
--------------------------------------------------------------------------------

### Dateienliste: <br>

**Aufgabe1-Github.R**:    R-Skript zur Simulation eines Datensatzes von 100 Beobachtungen zu den Variablen _Alter_, _Studienfach_, _Interesse an Mathematik_, _Interesse an Programmieren_ und _Mathe-LK_. <br>

**Aufgabe3.R**:          Funktionen zur deskreptiven Datenanalyse <br>  

**Datensatz.csv**:        Datensatz aus Aufgabe 1 in csv Format <br>

**Hilfsfunktionen.R**:   Verschiedene Hilfsfunktionen für Aufgabe 3<br>

--------------------------------------------------------------------------------

### Funktionen: 

#### ``deskr(x,...)``
Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische Variablen berechnet und ausgibt. <br>
Die Funktion erwartet die Eingabe eines Vektors von metrischen Daten und berechnet dessen arithmetisches Mittel, den Median, die Varianz, die Standardabweichung, die Range, sowie die 0, 0.25, 0.5, 0.75 und 1 Quantile und gibt die Maßzahlen anschließend aus. Über das "..."-Argument kann beispielsweise mittels na.rm = TRUE der Umgang mit fehlenden Werten an die Funktionen der Maßzahlen durchgereicht werden.

Beispiele:

``deskr(iris\$Sepal.Length)`` <br>
``deskr(c(NA, 1:10), na.rm = TRUE)``  (mit NA im Datenvektor und weitergereichtem Argument na.rm)


#### deskr_kat(x,...): 
Eine Funktion, die verschiedene geeignete deskriptive Statistiken für kategoriale Variablen berechnet und ausgibt.

Beispiele:

str(housing\$Freq) (Kardinal)<br>
str(housing\$Sat)  (Ordinal) <br>
str(housing\$Type) (Nominal)

deskr_kat(housing\$Freq)<br>
deskr_kat(housing\$Sat)<br>
deskr_kat(housing\$Type)<br>

#### zus_kat(x,y)

#### deskr_d(x,y)

#### e(x,Ordnung=TRUE,...)
Eine Funktion, die eine Variable (ordinal oder metrisch) quantilbasiert in die drei Kategorien "Niedrig", "Mittel" und "Hoch" einordnet. 
Die Funktion erwartet die Variable als Argument x, die den Datentyp character, factor, numeric oder integer hat. Wenn x nicht den Datentyp Faktor beinhaltet, wird die Variable in einen geordneten Faktor umgewandelt. Wenn der Faktor noch nicht geordnet ist, kann man zusätzlich das Argument Ordnung=FALSE angeben. Dann kann mit dem Argument ord die gewünschte Ordnung des Faktors angegeben werden und die Variable x wird mit der Hilfsfunktion Faktor_ordnen(x,ord) in einen geordneten Faktor umgewandelt.

Beispiel: <br>
e(Cars93$Cylinders,Ordnung = FALSE, ord = levels(Cars93$Cylinders)) 

#### katVis(x,y,z,w=NULL):
Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt. <br>
Die Funktion erwartet als Eingabe drei oder vier Vektoren der gleichen Länge, die in einem Streudiagramm mittels ggplot2-Funktionen dargestellt werden. Dabei werden die Eingaben so sortiert, dass die Variable mit der geringsten Anzahl Kategorien an letzter Stelle steht, sodass sie, bei drei Eingaben, die Farbe der Punkte bestimmt und bei vier Eingaben die Form der Punkte. Die Punkte werden zusätzlich zur besseren Erkennbarkeit leicht um die eigentlichen Werte gestreut. Für das Vertauschen der Eingaben wird die Hilfsfunktion swap verwendet.

Beispiele:<br>
x <- factor(sample(rep(letters[1:4], 10), 30))<br>
y <- ordered(sample(rep(1:7, 10), 30))<br>
z <- ordered(sample(rep(1:7, 10), 30))<br>
w <- factor(sample(rep(0:1, 100), 30))<br>

katVis(x,y,z,w)<br>
katVis(x,y,z)<br>
katVis(w,y,z,x)<br>
katVis(w,z)<br>

--------------------------------------------------------------------------------

### Hilfsfunktionen:

#### phi_str(x):

Berechnet das Phi-Streuungsmaß für einen Faktor

#### swap(x,y):
Vertauscht die Werte der Variable x mit denen der Variable y

#### Faktor_ordnen(x,ord)

#### phi(x,y)

#### chi(x,y)

#### cramer(x,y)

#### pears(x,y)

#### pears.korr(x,y)

#### yule(x,y)

#### null(x,y)
