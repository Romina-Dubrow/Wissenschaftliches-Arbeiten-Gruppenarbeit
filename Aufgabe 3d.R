## Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer
## den Zusammengang zwischen einer metrischen und einer
## dichotomen Variablen berechnet und ausgibt


# x = dichotom
# y = metrisch

deskr <- function(x, y){
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
deskr(a$Mathe.LK,a$Intersse.an.Mathematik)

