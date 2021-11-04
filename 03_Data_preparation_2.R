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

##### 3. Datensatzvorbereitung auf Basis der EDA in Punkt 2

### 3.1 Datum

sachsen_sel$Datum <- sachsen_sel$Datum - 43465

# siehe Plotanalyse, 3 Teile, Winter, Sommerpause und Rest. Sommerpause Sommerferien und die Woche vorher. Winterzeit die letzen 2 und die ersten 5 Wochen.
# Diese Einteilung kann nicht unüberprüft für andere Jahre übernommen werden, da dort die Wintertage oder Sommerferienzeiten die Statistik beeinflussen.
sachsen_sel$Datum <- fct_explicit_na(cut(sachsen_sel$Datum, breaks = c(0,35,182,231,350,366), labels = c("Winterzeit","Sonstige","Sommerpause","Sonstige","Winterzeit")), "keine Angabe")

#Encoding zu 2 Dummy-Spalten für 3 Klassen, Sonstige werden weggelassen wegen redundanter Information.
sachsen_sel$Winterzeit <- with(sachsen_sel, ifelse(Datum == "Winterzeit", "1", "0"))
sachsen_sel$Sommerpause <- with(sachsen_sel, ifelse(Datum == "Sommerpause", "1", "0"))
sachsen_sel <- select(sachsen_sel, -Datum)

### 3.2 Tageszeit

#Kommas mussten manuell in Punkte umgewandelt werden in der CSV
as.numeric_version(sachsen_sel$Zeit) 
sachsen_sel$Zeit <- sachsen_sel$Zeit * 24

sachsen_sel$Zeit <- fct_explicit_na(cut(sachsen_sel$Zeit, breaks = c(0,4,5.5,6.5,13.5,18,20.5,24), labels = c("0-4","4-5.30","5.30-6.30","6.30-13.30","13.30-18","18-20.30","20.30-24")), "keine Angabe")

#Encoding zu 6 Dummy-Spalten für 7 Klassen (zur Weggelassenen gehört dann auch die Null dazu).
sachsen_sel$abnulluhr <- with(sachsen_sel, ifelse(Zeit == "0-4", "1", "0"))
sachsen_sel$abvieruhr <- with(sachsen_sel, ifelse(Zeit == "4-5.30", "1", "0"))
sachsen_sel$abfuenfuhrdreissig <- with(sachsen_sel, ifelse(Zeit == "5.30-6.30", "1", "0"))
sachsen_sel$absechsuhrdreissig <- with(sachsen_sel, ifelse(Zeit == "6.30-13.30", "1", "0"))
sachsen_sel$abdreizehnuhrdreissig <- with(sachsen_sel, ifelse(Zeit == "13.30-18", "1", "0"))
sachsen_sel$abachtzehnuhr <- with(sachsen_sel, ifelse(Zeit == "18-20.30", "1", "0"))
sachsen_sel <- select(sachsen_sel, -Zeit)

### 3.3 Wochentag

#Encoding zu 2 Dummy-Spalten für 3 Klassen
sachsen_sel$Samstag <- with(sachsen_sel, ifelse(WoTag == "Sa", "1", "0"))
sachsen_sel$Sonntag <- with(sachsen_sel, ifelse(WoTag == "So", "1", "0"))
sachsen_sel <- select(sachsen_sel, -WoTag)

### 3.4 bis 3.7

#Variablen werden so behalten

### 3.8 Altersstruktur

#Eindeutige Identifikation zwei besonderer gruppen bei Age1, siehe Splits und Plot
#Age2 sehr ähnlich mit vernachlässigbaren Schnittmengen, daher kombination mit logischem oder

as.character(sachsen_sel$Age1)
as.character(sachsen_sel$Age2)
          
sachsen_sel$Kind <- with(sachsen_sel, ifelse(Age1 == "1" | Age2 == "1" | Age1 == "2" | Age2 == "2" | Age1 == "3" | Age2 == "3" | Age1 == "4" | Age2 == "4" | Age1 == "5" | Age2 == "5" | Age1 == "6" | Age2 == "6" | Age1 == "7" | Age2 == "7" | Age1 == "8" | Age2 == "8" | Age1 == "9" | Age2 == "9" | Age1 == "10" | Age2 == "10" | Age1 == "11" | Age2 == "11" | Age1 == "12" | Age2 == "12" | Age1 == "13" | Age2 == "13" | Age1 == "14" | Age2 == "14", "1", "0"))
sachsen_sel$Jugendlich <- with(sachsen_sel, ifelse(Age1 == "15" | Age2 == "15" | Age1 == "16" | Age2 == "16" | Age1 == "17" | Age2 == "17", "1", "0"))

#Löschen der Originalvariable Age1, 0 (welches fast immer eine Fehlangabe wegen Fahrerflucht ist) wird damit wie der restliche Schnitt gehandelt
#Löschen der dritten  Altersvariablen, da sie ähnliches ausdrückt aber wegen Fehldaten weniger signifikant sind
sachsen_sel <- select(sachsen_sel, -Age1, -Age2, -Age3)

### 3.9 Schadensschwere

#Kombinationsfunktionen haben nicht den gewünschten effekt gehabt. Einzelne Klassifizierung

# 3.9.1 Todesfälle

#Dummy Varialbe, nur 2 Fälle mit 2 Toten
sachsen_sel$Fatalities <- with(sachsen_sel, ifelse(Fatalities == 1 | Fatalities == 2, "1", "0"))

# 3.9.2 Schwerverletzte

#Über 3 Schwerverletzte sind Daten mit wenig Systematik und mit sehr geringer Anzahl
sachsen_sel$schwerverletzt_1 <- with(sachsen_sel, ifelse(SeriousInjuries == 1, "1", "0"))
sachsen_sel$Schwerverletzt_2bis3 <- with(sachsen_sel, ifelse(SeriousInjuries == 2 | SeriousInjuries == 3, "1", "0"))
sachsen_sel$Schwerverletzt_ab3 <- with(sachsen_sel, ifelse(SeriousInjuries > 3, "1", "0"))
sachsen_sel <- select(sachsen_sel, -SeriousInjuries)

# 3.9.3 Leichtverletzte

#Über 4 Leichtverletzte sind Daten mit wenig Systematik und mit sehr geringer Anzahl
sachsen_sel$leichtverletzt_1 <- with(sachsen_sel, ifelse(LightInjuries == 1, "1", "0"))
sachsen_sel$leichtverletzt_2bis4 <- with(sachsen_sel, ifelse(LightInjuries == 2 | LightInjuries == 3 | LightInjuries == 4, "1", "0"))
sachsen_sel$leichtverletzt_ab4 <- with(sachsen_sel, ifelse(LightInjuries > 4, "1", "0"))
sachsen_sel <- select(sachsen_sel, -LightInjuries)

# 3.9.4 Beteiligtenzahl 

#Über 5 Beteiligte sind Daten mit wenig Systematik und mit sehr geringer Anzahl
sachsen_sel$beteiligte_1 <- with(sachsen_sel, ifelse(Involved == 1, "1", "0"))
sachsen_sel$beteiligte_3bis5 <- with(sachsen_sel, ifelse(Involved == 3 | Involved == 4 | Involved == 5, "1", "0"))
sachsen_sel$beteiligte_ab5 <- with(sachsen_sel, ifelse(Involved > 5, "1", "0"))
sachsen_sel <- select(sachsen_sel, -Involved)

# 3.9.5 Geld-Schaden

sachsen_sel$Damage <- fct_explicit_na(cut(sachsen_sel$Damage, breaks = c(-1,1000,3000,5000,8000,16000,100000), labels = c("bis1000","bis3000","bis5000","bis8000","bis16000","rest")), "keine Angabe")

#Encoding zu 5 Dummy-Spalten für 6 Klassen
sachsen_sel$bis1000euro <- with(sachsen_sel, ifelse(Damage == "bis1000", "1", "0"))
sachsen_sel$bis3000euro <- with(sachsen_sel, ifelse(Damage == "bis3000", "1", "0"))
sachsen_sel$bis5000euro <- with(sachsen_sel, ifelse(Damage == "bis5000", "1", "0"))
sachsen_sel$bis8000euro <- with(sachsen_sel, ifelse(Damage == "bis8000", "1", "0"))
sachsen_sel$bis16000euro <- with(sachsen_sel, ifelse(Damage == "bis16000", "1", "0"))
sachsen_sel <- select(sachsen_sel, -Damage)

# 3.10 Fahrtrichtung

sachsen_sel$Fahrtrichtung_angegeben <- with(sachsen_sel, ifelse(Fahrtrichtung == "absteigend" | Fahrtrichtung == "aufsteigend", "1", "0"))
sachsen_sel <- select(sachsen_sel, -Fahrtrichtung)

### 3.11 Dummy-Variablen Encoding

# 3.11.1 RoadType

#Weglassen der Gemeindestraße wegen redundanter Information
sachsen_sel$autobahn <- with(sachsen_sel, ifelse(RoadType == "A", "1", "0"))
sachsen_sel$bundesstrasse <- with(sachsen_sel, ifelse(RoadType == "B", "1", "0"))
sachsen_sel$landesstrasse <- with(sachsen_sel, ifelse(RoadType == "L", "1", "0"))
sachsen_sel$kreisstrasse <- with(sachsen_sel, ifelse(RoadType == "K", "1", "0"))
sachsen_sel <- select(sachsen_sel, -RoadType)

# 3.11.2 CollisionType ~ Unfallart

#Encoding zu Dummy-Spalten
sachsen_sel$unfallart_1 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss mit anfahrendem/anhaltendem/ruhendem Fahrzeug", "1", "0"))
sachsen_sel$unfallart_2 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss mit vorausfahrendem/wartendem Fahrzeug", "1", "0"))
sachsen_sel$unfallart_3 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss mit seitlich in gleicher Richtung fahrendem Fahrzeug", "1", "0"))
sachsen_sel$unfallart_4 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss mit entgegenkommendem Fahrzeug", "1", "0"))
sachsen_sel$unfallart_5 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss mit einbiegendem/kreuzendem Fahrzeug", "1", "0"))
sachsen_sel$unfallart_6 <- with(sachsen_sel, ifelse(CollisionType == "Zusammenstoss zwischen Fahrzeug und FussgÃ¤nger", "1", "0"))
sachsen_sel$unfallart_7 <- with(sachsen_sel, ifelse(CollisionType == "Aufprall auf Fahrbahnhindernis", "1", "0"))
sachsen_sel$unfallart_8 <- with(sachsen_sel, ifelse(CollisionType == "Abkommen von Fahrbahn nach rechts", "1", "0"))
sachsen_sel$unfallart_9 <- with(sachsen_sel, ifelse(CollisionType == "Abkommen von Fahrbahn nach links", "1", "0"))
sachsen_sel <- select(sachsen_sel, -CollisionType)

### 3.12 Loeschen und Umbenennen

# 3.12.1 Spalte X

sachsen_sel <- select(sachsen_sel, -X)

# 3.12.2 Geringe Informationsmengen loeschen

sachsen_sel <- select(sachsen_sel, -BadView, -BadSigns)

# 3.12.3 Unfalltyp

#Achtung!: Der Name von Unfallart 4 wurde in der Excel "Vorbereitung_teil1" manuell durch "Ueberschreiten-Unfall (UeS)" ersetzt, da sonst kryptische Zeichen nicht gelesen werden können!
#Entsprechend der hier eingetragene geänderte Name

sachsen_sel$AccidentType <- with(sachsen_sel, ifelse(AccidentType == "Sonstiger Unfall (SO)", "7", ifelse(AccidentType == "Fahrunfall (F)", "1", ifelse(AccidentType == "Unfall durch ruhenden Verkehr (RV)", "5", ifelse(AccidentType == "Unfall im LÃ¤ngsverkehr (LV)", "6", ifelse(AccidentType == "Einbiegen/Kreuzen-Unfall (EK)", "3", ifelse(AccidentType == "Ueberschreiten-Unfall (UeS)", "4", ifelse(AccidentType == "Abbiegeunfall (AB)", "2", "0"))))))))

write.csv2(sachsen_sel, paste(daten,"/ergebnisse/2_Vorbereitung_teil2.csv", sep = ""))


