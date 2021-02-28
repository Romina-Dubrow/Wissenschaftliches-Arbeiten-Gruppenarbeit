###### Aufgabe 3 ########
# Funktionen

#a)

deskr <- function(x, ...){
  cat(" Deskriptive Statistiken: \n",    
  "------------------------\n",
  "Arithm. Mittel:", mean(x, ...), "\n",    ## Jeweils benannte Ausgabe der Masszahlen auf die Konsole
  "Median:", median(x, ...), "\n",          ## abgetrennt durch Zeilenumbruch
  "Varianz:", var(x, ...), "\n",
  "Std.abw.:", sd(x, ...), "\n",
  "Range:", range(x, ...), "\n",
  "Quantile: \n")
  for(i in 1:5){                            ## for-Schleife, um die Quantile jeweils mit richtiger Benennung auszugeben
    p <- c("0%:  ", "25%: ", "50%: ", "75%: ", "100%:")
    cat("  ", p[i] , quantile(x, type = 2, ...)[i], "\n")
  }
  cat(" ------------------------\n")
}


#b)

deskr_kat <- function(x, ...){
  if(is.factor(x)){               ## Ist Eingabe ein Faktor?
    
    if(is.ordered(x)){            ## Ordinal
      
      cat("Ordinales Merkmal\n",
          "Modalwert:", levels(x)[which.max(table(x))], "\n",
          "Median:", levels(x)[quantile(x, type = 1, ...)[3]], "\n",
          "Quantile:", "\n")
      for (i in 1:5) {
        p <- c("0%:  ", "25%: ", "50%: ", "75%: ", "100%:")
        cat("  ", p[i] , levels(x)[quantile(x, type=1, ...)[i]], "\n")
      }
      cat("Phi-Streuungsmaß:", phi_str(x,...))
    }
    else{                         ## Nominal
      
      cat("Nominales Merkmal\n",
          "Modalwert:", levels(x)[which.max(table(x))], "\n",
          "Phi-Streuungsmaß:", phi_str(x))
    }
  }
  else stop("Funktion erwartet Faktor")
}


#c) Eine Funktion, die geeignete deskriptive bivariate Statistiken 
#   für den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# alle kategoriellen Daten muessen hierfuer Faktoren und ordinale Daten zusaetzlich
# bereits richtig in R geordnet sein (kann mithilfe der Hilfsfunktion Faktor_ordnen() manuell geschehen



zus_kat <- function(x, y){
  if(is.factor(x) && is.factor(y)){               ## Ist Eingabe ein Faktor?
    if(is.ordered(x) && is.ordered(y)){           ## Ordinale Daten
      library(GoodmanKruskal)             
      x <- as.numeric(x)
      y <- as.numeric(y)
      cat("Ordinales Merkmal:\n",
          "Rangkorrelationskoeffizienten nach...\n",
          "...Spearman:", cor(x,y, method = "spearman"), 
          "\n ...Kendall:", cor(x,y, method = "kendall"), 
          "\n ...Goodman und Kruskal:", GKtau(x,y)$tauxy, "\n")
    }
    else{                                         ## Nominale Daten
      cat("Nominales Merkmal:\n",
          "Kontingenzkoeffizienten...\n")
      if(length(levels(x)) == 2 && length(levels(y)) == 2){     ## beide dichotom?
        cat(if(null(x,y) == FALSE){"...nach Yule:"}, if(null(x,y) == FALSE){yule(x,y)}, "\n...Phi:", phi(x,y))
      } 
      else{
        cat("...nach Cramér:", cramer(x,y))
      } 
      cat("\n...nach Pearson:", pears(x,y), ", korrigiert:", pears.korr(x,y), "\n")
    }
  }
  else stop("Funktion erwartet Faktor")
}

# Testdaten:
# ordinal
o <- c("langweilig", "ok", "spannend") 
a <- ordered(sample(o, 10, replace = TRUE), levels = o) 
p <- c("Professor", "Doktor", "M. Sc.", "B. Sc.")
b <- ordered(sample(p, 10, replace = TRUE), levels = rev(p)) 
zus_kat(a,b)
# nominal dichotom
c <- factor(sample(c(rep("vergeben", 3), "single"), 10, replace = TRUE))
d <- factor(sample(c(rep("happy", 3), "sad"), 10, replace = TRUE))
zus_kat(c,d)
# nominal
e <- factor(sample(c(rep("vergeben", 3), "single"), 10, replace = TRUE))
f <- factor(sample(c("Professor", "Doktor", "M. Sc.", "B. Sc."), 10, replace = TRUE))
zus_kat(e,f)


#d)
## Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt

a <- read.csv("Datensatz.csv")

# x = dichotom
# y = metrisch

deskr_d <- function(x, y){
  cat(" Deskriptive Statistiken: \n",
      "------------------------\n",
      "Arithm. Mittel (Ja):", mean(y[x == "Ja"]), "\n",
      "Arithm. Mittel (Nein):", mean(y[x == "Nein"]), "\n",
      "Median (Ja):", median(y[x == "Ja"]), "\n",
      "Median (Nein):", median(y[x == "Nein"]), "\n",
      "Varianz (Ja):", var(y[x == "Ja"]), "\n",
      "Varianz (Nein):", var(y[x == "Nein"]), "\n",
      "Std.abw.(Ja):", sd(y[x == "Ja"]), "\n",
      "Std.abw.(Nein):", sd(y[x == "Nein"]), "\n",
      "Range (Ja):", range(y[x == "Ja"]), "\n",
      "Range (Nein):", range(y[x == "Nein"]), "\n",
      "Quantile : \n")
  for(i in 1:5){
    p <- c("0% (für Ja):  ", "25%: ", "50%: ", "75%: ", "100%:")
    cat("  ", p[i] , quantile(y[x == "Ja"])[i], "\n")
  }
  cat("\n",
  for(i in 1:5){
    p <- c("0% (für Nein):  ", "25%: ", "50%: ", "75%: ", "100%:")
    cat("  ", p[i] , quantile(y[x == "Nein"])[i], "\n")
  })
  cat(" ------------------------\n")
  plot(x,y)
  addmargins(table(x,y))
}
deskr_d(a$Mathe.LK,a$Intersse.an.Mathematik)



#e)


e <- function(x, Ordnung=TRUE,...){
  
     if(is.character(x)){ # wenn es einfach nur ein Vektor ohne geordnete Inhalte ist
    
        x <- factor(x,ordered=TRUE, levels=unique(x))}      
  
  
      if(is.numeric(x)|is.integer(x)){ # Wenn es ein Vektor metrischer Art ist
    
         x <- factor(x,ordered=TRUE)} # ich habe levels = x rausgenommen damit Zahlen sich
                                      # egal wie der Vektor angeordnet ist nach ihrer groesse
                                      # ordnen
      
    
    if(is.factor(x)){ #wenn es ein Faktor ist
      
       if(Ordnung){ # geordneter Faktor
        x <-  ordered(x[order(x)]) } # Jetzt ist der Faktor auch, wenn im Vektor die Werte vorher anders angeordnet waren, in der richtigen Reihenfolge
    
       else { # ungeordneter Faktor, man musste oben die gewuenschte Ordnung des Faktors angeben
       x <- Faktor_ordnen(x,...)   # ord = Vektor mit gewuenschter Ordnung des Faktors
        }
    
    }
       
  
  
     geordnet <- as.numeric(x) 
     g_4 <- quantile(geordnet,type = 1)[2] #untere quantilgrenze
     g4  <- quantile(geordnet,type = 1)[4] #obere quantilgrenze
     
     q_4 <- x[geordnet <= g_4] # unteres quantil
     q4 <- x[geordnet >= g4] # oberes quantil
     box <- x[geordnet > g_4 & geordnet < g4] # mittlere werte
    
  
 # ergebnis <- list("niedrig"=q_4,"mittel"=box,"hoch"=q4)
 # return(ergebnis)
  
    cat(" Quantilbasierte Kategorisierung: \n",
      "------------------------\n",
      "Niedrig:", as.character(unique(q_4)), "\n",
      "Mittel:", as.character(unique(box)), "\n",
      "Hoch:", as.character(unique(q4)), "\n",
      " ------------------------\n")
  
}





#f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier kategorialen Variablen erstellt
library(ggplot2)
#Aufruf mit optionalem vierten Element
katVis <- function(x, y, z, w=NULL){
  # Variablen umsortieren, damit der Plot uebersichtlich bleibt (Variable mit den wenigsten Leveln für color)
  if(which.min(c(nlevels(x), nlevels(y), nlevels(z)))!=3 & nlevels(x)<nlevels(y)){swap(x,z)}
  if(which.min(c(nlevels(x), nlevels(y), nlevels(z)))!=3 & nlevels(y)<nlevels(x)){swap(y,z)}
  # fuer drei Variablen
  if(is.null(w)){
    ggplot(data.frame(x,y,z)) + geom_jitter(aes(x,y,color=z), height=0.2, width=0.2) + scale_color_brewer(palette="Set1")
  }
  # fuer vier Variablen
  else{
    # umsortieren, Variable mit wenigsten Leveln fuer shape
    if(which.min(c(nlevels(x), nlevels(y), nlevels(z), nlevels(w)))!=4){swap(z,w)}
    ggplot(data.frame(x,y,z,w)) + geom_jitter(aes(x,y,color=z, shape=w), height=0.2, width=0.2) + scale_color_brewer(palette="Set1")
  }
}






