---
title: "Bruchpunktmodelle"
author: "Volker Schmid"
date: "17. Juli 2017"
output:
  beamer_presentation:
    keep_tex: false
    toc: true
    slide_level: 2
vignette: >
  %\VignetteIndexEntry{Bruchpunktmodelle}
  %\VignetteEngine{knitr::rmarkdown}
---



# Bruchpunktmodelle
## Bruchpunktmodelle

[//]: Mögliche Erweiterung: Diskrete Markovketten, eventuell Epidemisches Modell zeigen

* Beim Bruchpunktmodell ändert sich ein Parameter eines Modells an einem Punkt sprungartig
* In der Regel Anwendung auf Zeitreihen
* Theoretisch diskrete oder stetige Zeit möglich

Beispiele:

* Aktienkurse
* Schlafdaten
* Krankheiten mit Schüben
* Stärke von Sportmannschaften
* Zeitreihen von Krankheitsfällen (Bruchpunkt: Ausbruch einer Epidemie)

## Beispiel: Unfälle in englischen Kohlebergwerken {.allowframebreaks}

[//]: Ursprünglicher Autor: Michael Höhle, Daten siehe data(coal, package="boot")

Der Datensatz {\texttt coal.txt} enthält die jährliche Anzahl von Unfällen in englischen Kohlebergwerken wärend der Jahre
1851-1962 (insgesamt 112 Jahre). Ein Plot der Daten zeigt einen
deutlichen Rückgang der Unfälle ab etwa 1900.


```r
data("coal",package="bayeskurs")
```

```
## Warning in data("coal", package = "bayeskurs"): data set 'coal' not found
```











