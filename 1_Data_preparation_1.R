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

sachsenvisumsafety <- read.csv(paste(daten,"/euska_sachsen_2019_visumsafety.csv", sep = ""), row.names = NULL, sep = ";")
sachsen_normal <- read.csv(paste(daten,"/euska_sachsen_2019.csv", sep = ""), row.names = NULL, sep = ";")

#Teil 1: Offensichtliches Löschen und Hinzufügen von Features.

#Löschen aller Größen, die nicht aussagekräftig oder nicht übertragbar sind (IDs, ortsspezifische Angaben, Zusatzinformationen)
sachsen_sel <- rename(sachsenvisumsafety)
sachsen_sel <- select(sachsen_sel, -Longitude, -ï..AccidentID, -Latitude, -Direction, -Office, -City, -ZIP, -RoadID, -RoadName, -RoadName2, -Location, -Sketch, - Description, - Notes, -Image, -Attachment)

#Löschen aller Größen, die nicht nicht interpretierbar sind, zu viele leere Felder enthalten oder klar redundant sind (z.B. accident Type encoded, Kategorie)
sachsen_sel <- select(sachsen_sel, -AccidentCost, -SpeedLimit, - Hazard, -Category, -CollisionCode, -Type1, -Type2, -Type3, -Type4, -Type5, -Type6, -IndividualReason, -LicenseAge1, -Offence1, -LicenseAge2, -Offence2, -LicenseAge3, -Offence3, -Vehicle1, -Vehicle2, -Vehicle3)

#Ergänzen von Wochentag, Fahrtrichtung und Hindernisaufprall aus anderem Export, richtig formatiertes Tagesdatum und Tageszeit
sachsen_sel2 <- select(sachsen_normal, WoTag, Fahrtrichtung, Datum, Zeit, AH)
sachsen_sel2 <- rename(sachsen_sel2, Aufprall_Hindernis = AH)
sachsen_sel <- select(sachsen_sel, -Date, -Time)
sachsen_sel <- cbind(sachsen_sel, sachsen_sel2)
#print(sachsen_sel)

#Auswahl der Hauptinformationen und schreiben der neuen csv (csv2 für deutsche Trennversion mit Semikolons, für Excel interpretierbar)
write.csv2(sachsen_sel, paste(daten,"/ergebnisse/1_Vorbereitung_teil1.csv", sep = ""))


