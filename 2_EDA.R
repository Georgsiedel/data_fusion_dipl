library(dplyr)
library(writexl)
library(reshape2)
library(tidyr)
library(forcats)
library(ggplot2)
library(readr)
library(gridExtra)
library(grid)
library(ggridges)
library(ggthemes)
library(tidyverse)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

sachsen_sel <- read.csv(paste(daten,"/ergebnisse/1_Vorbereitung_teil1.csv", sep = ""), row.names = NULL, sep = ";")


##### 2. EDA und entsprechende Analyse bestimmter Größen

### 2.1 Analyse Datum im Jahr

#Datum auf 1 bis 365 Jahrestage umwandeln
#sachsen_sel$Datum <- sachsen_sel$Datum - 43465

# 2.1.1 Balkendiagramm relative Häufigkeit Unfalltyp über Jahreswochen normiert

#sachsen_sel$Datum <- cut_width(sachsen_sel$Datum, 7, boundary = 1)
#ggplot(data = sachsen_sel, aes(x = Datum, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.1.2 Dichtefunktion Unfalltypen über Jahr (kontinuierlich), normiert und transparent

#ggplot(data = sachsen_sel, aes(x = Datum, y = AccidentType, color = AccidentType, fill = AccidentType)) +
#geom_density_ridges(alpha = 0.3, scale = 50)

### 2.2 Analyse Uhrzeit

#Uhrzeit von Skala 0 bis 1 auf Skala 0Uhr bis 24Uhr umwandeln
#Numerische Angabe notwendig, Feld musste in Excel manuell von Komma auf Punkt als Kommazeichen umgestellt werden
#as.numeric_version(sachsen_sel$Zeit) 
#sachsen_sel$Zeit <- sachsen_sel$Zeit * 24

# 2.2.1 Balkendiagramm relative Häufigkeit Unfalltyp über Uhrzeit normiert, diskret nach halben Stunden

#sachsen_sel$Zeit <- cut_interval(sachsen_sel$Zeit, 48, boundary = 0, center = 0.25)
#ggplot(data = sachsen_sel, aes(x = Zeit, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.2.2 Dichtefunktion Unfalltypen über Uhrzeit (kontinuierlich), normiert und transparent

#ggplot(data = sachsen_sel, aes(x = Zeit, y = AccidentType, color = AccidentType, fill = AccidentType)) +
#geom_density_ridges(alpha = 0.3, scale = 1000)

### 2.3 Analyse Wochentag

# 2.3.1 Balkendiagramm relative Häufigkeit Unfalltyp über Uhrzeit normiert, diskret nach halben Stunden

#ggplot(data = sachsen_sel, aes(x = WoTag, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

### 2.4 Analyse aller Beteiligten Fahrzeugarten

# 2.4.1 Balkendiagramm zur Analyse von Gemeinsamkeiten bei großen Beteiligten Fahrzeugen

#sachsen_sel$dickekarren <- with(sachsen_sel, ifelse(Truck == "1", "Truck", ifelse(Bus == "1", "Bus", ifelse(Tram == "1", "Tram", ifelse(Train == "1", "Train", "0")))))
#ggplot(data = sachsen_sel, aes(x = dickekarren, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
#sachsen_sel$dickekarren <- select(sachsen_sel, -dickekarren)

# 2.4.2 Balkendiagramm zur Analyse von Gemeinsamkeiten bei Zweirädern und Fußgängern

#sachsen_sel$kleineteilnehmer <- with(sachsen_sel, ifelse(Pedestrian == "1", "Pedestrian", ifelse(Bicycle == "1", "Bicycle", ifelse(Motorcycle == "1", "Motorcycle", "0"))))
#ggplot(data = sachsen_sel, aes(x = kleineteilnehmer, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
#sachsen_sel$kleineteilnehmer <- select(sachsen_sel, -kleineteilnehmer)

### 2.5 Analyse von Eigenschaften des Unfallortes

# 2.5.1 Balkendiagramm zur Analyse von Gemeinsamkeiten bei bestimmten Unfallorten

#sachsen_sel$unfallort <- with(sachsen_sel, ifelse(Driveway == "1", "Driveway", ifelse(Slope == "1", "Slope", ifelse(Curve == "1", "Curve", ifelse(Crosswalk == "1", "Crosswalk", ifelse(Roundabout == "1", "Roundabout", "0"))))))
#ggplot(data = sachsen_sel, aes(x = unfallort, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
#sachsen_sel$dickekarren <- select(sachsen_sel, -unfallort)

### 2.6 Analyse des Straßentyps

# 2.6.1 Balkendiagramm zur Analyse des Straßentyps

#ggplot(data = sachsen_sel, aes(x = RoadType, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

### 2.7 Analyse von Eigenschaften von Unfallursachen

# 2.7.1 Balkendiagramm zur Analyse von Eigenschaften von Unfallursachen (Konditionen)

#sachsen_sel$unfallursache <- with(sachsen_sel, ifelse(Slippery == "1", "Slippery", ifelse(Darkness == "1", "Darkness", ifelse(BadView == "1", "BadView", ifelse(BadWeather == "1", "BadWeather", ifelse(EnvironmentalReason == "1", "EnvironmentalReason", "0"))))))
#ggplot(data = sachsen_sel, aes(x = unfallursache, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
#sachsen_sel$dickekarren <- select(sachsen_sel, -unfallursache)

#2.7.2 Balkendiagramm zur Analyse von Eigenschaften von Unfallursachen (Hindernisse)

#sachsen_sel$unfallursache <- with(sachsen_sel, ifelse(Animal == "1", "Animal", ifelse(ObstacleOnRoad == "1", "ObstacleOnRoad", ifelse(ObstacleOffRoad == "1", "ObstacleOffRoad", ifelse(Tree == "1", "Tree", "0")))))
#ggplot(data = sachsen_sel, aes(x = unfallursache, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
#sachsen_sel$dickekarren <- select(sachsen_sel, -unfallursache)

### 2.8 Analyse Altersstruktur

# 2.8.1 Balkendiagramm relative Häufigkeit Unfalltyp über Altersgruppen normiert, diskret nach 5 Lebensjahren

#ggplot(data = sachsen_sel, aes(x = Age1, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.8.2 Dichtefunktion Unfalltypen über Uhrzeit (kontinuierlich), normiert und transparent

#ggplot(data = sachsen_sel, aes(x = Age1, y = AccidentType, color = AccidentType, fill = AccidentType)) +
#geom_density_ridges(alpha = 0.3, scale = 1000)

### 2.9 Analyse Schadensgrösse

# 2.9.1 Balkendiagramm zur Analyse von Unfalltoten

#ggplot(data = sachsen_sel, aes(x = Fatalities, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.9.2 Balkendiagramm zur Analyse von Schwerverletzten

#ggplot(data = sachsen_sel, aes(x = SeriousInjuries, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.9.3 Balkendiagramm zur Analyse von Leichtverletzten

#ggplot(data = sachsen_sel, aes(x = LightInjuries, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.9.4 Balkendiagramm zur Analyse von Beteiligtenanzahl

#ggplot(data = sachsen_sel, aes(x = Involved, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.9.5 Balkendiagramm zur Analyse des Unfallschadens

#sachsen_sel$Damage <- cut_width(sachsen_sel$Damage, 500, boundary = 0)
#ggplot(data = sachsen_sel, aes(x = Damage, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

# 2.9.6 Testfunktion "Unfallschwere"

#sachsen_sel$severity <- with(sachsen_sel, ifelse(Damage < 20000, Damage, 20000))
#sachsen_sel$severity <- sachsen_sel$severity + with(sachsen_sel, ifelse(LightInjuries < 7, LightInjuries * 2000, 12000))
#sachsen_sel$severity <- sachsen_sel$severity + with(sachsen_sel, ifelse(SeriousInjuries < 7, SeriousInjuries * 5000, 30000))
#sachsen_sel$severity <- sachsen_sel$severity + (sachsen_sel$Fatalities * 20000)

#sachsen_sel$severity <- cut_width(sachsen_sel$severity, 1000, boundary = 0)

#ggplot(data = sachsen_sel, aes(x = severity, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

### 2.10 Fahrtrichtung

# 2.10.1 Balkendiagramm

#ggplot(data = sachsen_sel, aes(x = Fahrtrichtung, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")

### 2.11 Unfallart
#2.11.1 Balkendiagramm

#ggplot(data = sachsen_sel, aes(x = CollisionType, color = AccidentType, fill = AccidentType)) +
#geom_bar(mapping = aes(), position = "fill")
