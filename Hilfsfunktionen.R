## Hilfsfunktionen

## Phi-Streuungsmaß:

phi_str <- function(x){
  if(!is.factor(x)) stop("Funktion erwartet Faktor")
  
  ## Häufigkeitsverteilung der Levels:
  K <- length(levels(x)) 
  hk <- numeric(K)
  l <- length(x)
  for (i in 1:K) {
    s <- sum(x == levels(x)[i])
    hk[i] <- s/l
  }
  
  ## Berechnung des Phi-Streuungsmaß
  phi_min <- 2*(1-hk[which.max(table(x))])
  phi_max <- sum(abs(hk-1/K))
  
  phi_min/(phi_min+phi_max)
}


# Hilfsfunktion swap: Vertauschen zweier Variablen
swap <- function(x,y) {
  eval( parse( text = paste(
    "swap_unique_var_a<-", substitute(x), ";",
    substitute(x), "<-", substitute(y), ";",
    substitute(y), "<-swap_unique_var_a") ), env=parent.frame() )
}


### Hilfsfunktion fuer ungeordnete Faktoren Aufgabe e) (wird auch für b) und c) benötigt! :))

Faktor_ordnen <- function(x,ord){
 x <- factor(x,ordered=TRUE, levels=ord)
 return(x)
}

## Hilfsfunktionen fuer Kontingenzkoeffizienten Aufgabe c)

# Phi-Koeffizient

phi <- function(x,y){
  t <- table(x,y)
  (t[1,1] * t[2,2] - t[1,2] * t[2,1])/sqrt((t[1,1] + t[1,2]) * (t[2,1] + t[2,2])
                                           * (t[1,1] + t[2,1]) * (t[1,2] + t[2,2]))
}


# Cramér

cramer <- function(x,y){
sqrt(summary(table(x,y))$statistic/(length(x) * min(length(levels(x)), length(levels(y)))))
}

# Pearson

pears <- function(x,y){
  sqrt(summary(table(x,y))$statistic/(summary(table(x,y))$statistic + length(x)))
}

pears.korr <- function(x,y){
  pears(x,y) * sqrt(min(length(levels(x)), length(levels(y)))/(min(length(levels(x)), length(levels(y))) - 1))
}

# Yule 

yule <- function(x,y){
  t <- table(x,y)
  (t[1,1] * t[2,2] - t[1,2] * t[2,1])/(t[1,1] * t[2,2] + t[1,2] * t[2,1])
}


# Nullzeilenueberpruefung 

null <- function(x,y){
  z <- ifelse(addmargins(table(x,y))[,length(levels(y)) + 1] != 0, FALSE, TRUE)
  s <- ifelse(addmargins(table(x,y))[length(levels(x)) + 1, ] != 0, FALSE, TRUE)
  if(any(c(z,s))) TRUE else FALSE
}

