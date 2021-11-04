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
library(StatMatch)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

sachsen <- read.csv(paste(daten,"/Ergebnisse/2_Vorbereitung_teil2.csv", sep = ""), row.names = NULL, sep = ";")

#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
sachsen <- sachsen[, -1]

#Datensplit 1:4 mit 2,5facher Datenmenge wie in Vergleichsarbeit von Max

a <- 5000
b <- 4*a

set.seed(1)
allrows <- 1:nrow(sachsen)
stichprobe_empfaenger <- sample(allrows, size = a, replace = FALSE)
allrows_ohne_stichprobe <- allrows[-stichprobe_empfaenger]
stichprobe_spender <- sample(allrows_ohne_stichprobe, size = b, replace = FALSE)

recipient <- sachsen[stichprobe_empfaenger,] 
donor <- sachsen[stichprobe_spender,]

#Faktorisieren der Datensätze falls hier etwas übersehen wurde

for (i in 1:length(donor)){
  donor[,i] <- factor(donor[,i])
}


for (i in 1:length(recipient)){
  recipient[,i] <- factor(recipient[,i])
}

#Datensplits speichern

write.csv2(recipient, paste(daten,"/ergebnisse/3_recipient_5k.csv", sep = ""))
write.csv2(donor, paste(daten,"/ergebnisse/3_donor_20k.csv", sep = ""))

### Kontigenztabellen berechnen und Hellinger Distanz zwischen gleichen Attributen der zwei Datensätze
### --> Populationsvergleich

{if(nlevels(donor$Fatalities) == nlevels(recipient$Fatalities)){
  

  tt.R <- xtabs(~ Fatalities, data = recipient) #Empfänger
  tt.D <- xtabs(~ Fatalities, data = donor) #Spender 

  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_1 <- c("Fatalities", round(comp_a, 3))
  
} else {comp_1 <- c("Fatalities", "untersch. Levels")} }
  
{if(nlevels(donor$Urban) == nlevels(recipient$Urban)){
  
  
  tt.R <- xtabs(~ Urban, data = recipient) #Empfänger
  tt.D <- xtabs(~ Urban, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_2 <- c("Urban", round(comp_a, 3))
  
} else {comp_2 <- c("Urban", "untersch. Levels")} }

{if(nlevels(donor$Intersection) == nlevels(recipient$Intersection)){
  
  
  tt.R <- xtabs(~ Intersection, data = recipient) #Empfänger
  tt.D <- xtabs(~ Intersection, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_3 <- c("Intersection", round(comp_a, 3))
  
} else {comp_3 <- c("Intersection", "untersch. Levels")} }

{if(nlevels(donor$TrafficLights) == nlevels(recipient$TrafficLights)){
  
  
  tt.R <- xtabs(~ TrafficLights, data = recipient) #Empfänger
  tt.D <- xtabs(~ TrafficLights, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_4 <- c("TrafficLights", round(comp_a, 3))
  
} else {comp_4 <- c("TrafficLights", "untersch. Levels")} }

{if(nlevels(donor$Driveway) == nlevels(recipient$Driveway)){
  
  
  tt.R <- xtabs(~ Driveway, data = recipient) #Empfänger
  tt.D <- xtabs(~ Driveway, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_5 <- c("Driveway", round(comp_a, 3))
  
} else {comp_5 <- c("Driveway", "untersch. Levels")} }

{if(nlevels(donor$Curve) == nlevels(recipient$Curve)){
  
  
  tt.R <- xtabs(~ Curve, data = recipient) #Empfänger
  tt.D <- xtabs(~ Curve, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_6 <- c("Curve", round(comp_a, 3))
  
} else {comp_6 <- c("Curve", "untersch. Levels")} }

{if(nlevels(donor$Slope) == nlevels(recipient$Slope)){
  
  
  tt.R <- xtabs(~ Slope, data = recipient) #Empfänger
  tt.D <- xtabs(~ Slope, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_7 <- c("Slope", round(comp_a, 3))
  
} else {comp_7 <- c("Slope", "untersch. Levels")} }

{if(nlevels(donor$Crosswalk) == nlevels(recipient$Crosswalk)){
  
  
  tt.R <- xtabs(~ Crosswalk, data = recipient) #Empfänger
  tt.D <- xtabs(~ Crosswalk, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_8 <- c("Crosswalk", round(comp_a, 3))
  
} else {comp_8 <- c("Crosswalk", "untersch. Levels")} }

{if(nlevels(donor$Roundabout) == nlevels(recipient$Roundabout)){
  
  
  tt.R <- xtabs(~ Roundabout, data = recipient) #Empfänger
  tt.D <- xtabs(~ Roundabout, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_9 <- c("Roundabout", round(comp_a, 3))
  
} else {comp_9 <- c("Roundabout", "untersch. Levels")} }

{if(nlevels(donor$Runaway) == nlevels(recipient$Runaway)){
  
  
  tt.R <- xtabs(~ Runaway, data = recipient) #Empfänger
  tt.D <- xtabs(~ Runaway, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_10 <- c("Runaway", round(comp_a, 3))
  
} else {comp_10 <- c("Runaway", "untersch. Levels")} }

{if(nlevels(donor$Car) == nlevels(recipient$Car)){
  
  
  tt.R <- xtabs(~ Car, data = recipient) #Empfänger
  tt.D <- xtabs(~ Car, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_11 <- c("Car", round(comp_a, 3))
  
} else {comp_11 <- c("Car", "untersch. Levels")} }

{if(nlevels(donor$Pedestrian) == nlevels(recipient$Pedestrian)){
  
  
  tt.R <- xtabs(~ Pedestrian, data = recipient) #Empfänger
  tt.D <- xtabs(~ Pedestrian, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_12 <- c("Pedestrian", round(comp_a, 3))
  
} else {comp_12 <- c("Pedestrian", "untersch. Levels")} }

{if(nlevels(donor$Bicycle) == nlevels(recipient$Bicycle)){
  
  
  tt.R <- xtabs(~ Bicycle, data = recipient) #Empfänger
  tt.D <- xtabs(~ Bicycle, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_13 <- c("Bicycle", round(comp_a, 3))
  
} else {comp_13 <- c("Bicycle", "untersch. Levels")} }

{if(nlevels(donor$Motorcycle) == nlevels(recipient$Motorcycle)){
  
  
  tt.R <- xtabs(~ Motorcycle, data = recipient) #Empfänger
  tt.D <- xtabs(~ Motorcycle, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_14 <- c("Motorcycle", round(comp_a, 3))
  
} else {comp_14 <- c("Motorcycle", "untersch. Levels")} }

{if(nlevels(donor$Truck) == nlevels(recipient$Truck)){
  
  
  tt.R <- xtabs(~ Truck, data = recipient) #Empfänger
  tt.D <- xtabs(~ Truck, data = donor) #Spender
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_15 <- c("Truck", round(comp_a, 3))
  
} else {comp_15 <- c("Truck", "untersch. Levels")} }

{if(nlevels(donor$Bus) == nlevels(recipient$Bus)){
  
  
  tt.R <- xtabs(~ Bus, data = recipient) #Empfänger
  tt.D <- xtabs(~ Bus, data = donor) #Spender
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_16 <- c("Bus", round(comp_a, 3))
  
} else {comp_16 <- c("Bus", "untersch. Levels")} }

{if(nlevels(donor$Tram) == nlevels(recipient$Tram)){
  
  
  tt.R <- xtabs(~ Tram, data = recipient) #Empfänger
  tt.D <- xtabs(~ Tram, data = donor) #Spender
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_17 <- c("Tram", round(comp_a, 3))
  
} else {comp_17 <- c("Tram", "untersch. Levels")} }

{if(nlevels(donor$Train) == nlevels(recipient$Train)){
  
  
  tt.R <- xtabs(~ Train, data = recipient) #Empfänger
  tt.D <- xtabs(~ Train, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_18 <- c("Train", round(comp_a, 3))
  
} else {comp_18 <- c("Train", "untersch. Levels")} }

{if(nlevels(donor$AccidentType) == nlevels(recipient$AccidentType)){
  
  
  tt.R <- xtabs(~ AccidentType, data = recipient) #Empfänger
  tt.D <- xtabs(~ AccidentType, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_19 <- c("AccidentType", round(comp_a, 3))
  
} else {comp_19 <- c("AccidentType", "untersch. Levels")} }

{if(nlevels(donor$Slippery) == nlevels(recipient$Slippery)){
  
  
  tt.R <- xtabs(~ Slippery, data = recipient) #Empfänger
  tt.D <- xtabs(~ Slippery, data = donor) #Spender
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_20 <- c("Slippery", round(comp_a, 3))
  
} else {comp_20 <- c("Slippery", "untersch. Levels")} }

{if(nlevels(donor$Darkness) == nlevels(recipient$Darkness)){
  
  
  tt.R <- xtabs(~ Darkness, data = recipient) #Empfänger
  tt.D <- xtabs(~ Darkness, data = donor) #Spender
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_21 <- c("Darkness", round(comp_a, 3))
  
} else {comp_21 <- c("Darkness", "untersch. Levels")} }

{if(nlevels(donor$Animal) == nlevels(recipient$Animal)){
  
  
  tt.R <- xtabs(~ Animal, data = recipient) #Empfänger
  tt.D <- xtabs(~ Animal, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_22 <- c("Animal", round(comp_a, 3))
  
} else {comp_22 <- c("Animal", "untersch. Levels")} }

{if(nlevels(donor$ObstacleOnRoad) == nlevels(recipient$ObstacleOnRoad)){
  
  
  tt.R <- xtabs(~ ObstacleOnRoad, data = recipient) #Empfänger
  tt.D <- xtabs(~ ObstacleOnRoad, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_23 <- c("ObstacleOnRoad", round(comp_a, 3))
  
} else {comp_23 <- c("ObstacleOnRoad", "untersch. Levels")} }

{if(nlevels(donor$BadWeather) == nlevels(recipient$BadWeather)){
  
  
  tt.R <- xtabs(~ BadWeather, data = recipient) #Empfänger
  tt.D <- xtabs(~ BadWeather, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_25 <- c("BadWeather", round(comp_a, 3))
  
} else {comp_25 <- c("BadWeather", "untersch. Levels")} }

{if(nlevels(donor$BadRoad) == nlevels(recipient$BadRoad)){
  
  
  tt.R <- xtabs(~ BadRoad, data = recipient) #Empfänger
  tt.D <- xtabs(~ BadRoad, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_26 <- c("BadRoad", round(comp_a, 3))
  
} else {comp_26 <- c("BadRoad", "untersch. Levels")} }

{if(nlevels(donor$EnvironmentalReason) == nlevels(recipient$EnvironmentalReason)){
  
  
  tt.R <- xtabs(~ EnvironmentalReason, data = recipient) #Empfänger
  tt.D <- xtabs(~ EnvironmentalReason, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_28 <- c("EnvironmentalReason", round(comp_a, 3))
  
} else {comp_28 <- c("EnvironmentalReason", "untersch. Levels")} }

{if(nlevels(donor$Tree) == nlevels(recipient$Tree)){
  
  
  tt.R <- xtabs(~ Tree, data = recipient) #Empfänger
  tt.D <- xtabs(~ Tree, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_29 <- c("Tree", round(comp_a, 3))
  
} else {comp_29 <- c("Tree", "untersch. Levels")} }

{if(nlevels(donor$ObstacleOffRoad) == nlevels(recipient$ObstacleOffRoad)){
  
  
  tt.R <- xtabs(~ ObstacleOffRoad, data = recipient) #Empfänger
  tt.D <- xtabs(~ ObstacleOffRoad, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_30 <- c("ObstacleOffRoad", round(comp_a, 3))
  
} else {comp_30 <- c("ObstacleOffRoad", "untersch. Levels")} }

{if(nlevels(donor$Overtaking) == nlevels(recipient$Overtaking)){
  
  
  tt.R <- xtabs(~ Overtaking, data = recipient) #Empfänger
  tt.D <- xtabs(~ Overtaking, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_31 <- c("Overtaking", round(comp_a, 3))
  
} else {comp_31 <- c("Overtaking", "untersch. Levels")} }

{if(nlevels(donor$Alcohol) == nlevels(recipient$Alcohol)){
  
  
  tt.R <- xtabs(~ Alcohol, data = recipient) #Empfänger
  tt.D <- xtabs(~ Alcohol, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_32 <- c("Alcohol", round(comp_a, 3))
  
} else {comp_32 <- c("Alcohol", "untersch. Levels")} }

{if(nlevels(donor$Speeding) == nlevels(recipient$Speeding)){
  
  
  tt.R <- xtabs(~ Speeding, data = recipient) #Empfänger
  tt.D <- xtabs(~ Speeding, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_33 <- c("Speeding", round(comp_a, 3))
  
} else {comp_33 <- c("Speeding", "untersch. Levels")} }

{if(nlevels(donor$RedLight) == nlevels(recipient$RedLight)){
  
  
  tt.R <- xtabs(~ RedLight, data = recipient) #Empfänger
  tt.D <- xtabs(~ RedLight, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_34 <- c("RedLight", round(comp_a, 3))
  
} else {comp_34 <- c("RedLight", "untersch. Levels")} }

{if(nlevels(donor$Distance) == nlevels(recipient$Distance)){
  
  
  tt.R <- xtabs(~ Distance, data = recipient) #Empfänger
  tt.D <- xtabs(~ Distance, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_35 <- c("Distance", round(comp_a, 3))
  
} else {comp_35 <- c("Distance", "untersch. Levels")} }

{if(nlevels(donor$Priority) == nlevels(recipient$Priority)){
  
  
  tt.R <- xtabs(~ Priority, data = recipient) #Empfänger
  tt.D <- xtabs(~ Priority, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_36 <- c("Priority", round(comp_a, 3))
  
} else {comp_36 <- c("Priority", "untersch. Levels")} }

{if(nlevels(donor$Turning) == nlevels(recipient$Turning)){
  
  
  tt.R <- xtabs(~ Turning, data = recipient) #Empfänger
  tt.D <- xtabs(~ Turning, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_37 <- c("Turning", round(comp_a, 3))
  
} else {comp_37 <- c("Turning", "untersch. Levels")} }

{if(nlevels(donor$Aufprall_Hindernis) == nlevels(recipient$Aufprall_Hindernis)){
  
  
  tt.R <- xtabs(~ Aufprall_Hindernis, data = recipient) #Empfänger
  tt.D <- xtabs(~ Aufprall_Hindernis, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_38 <- c("Aufprall_Hindernis", round(comp_a, 3))
  
} else {comp_38 <- c("Turning", "untersch. Levels")} }

{if(nlevels(donor$Winterzeit) == nlevels(recipient$Winterzeit)){
  
  
  tt.R <- xtabs(~ Winterzeit, data = recipient) #Empfänger
  tt.D <- xtabs(~ Winterzeit, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_39 <- c("Winterzeit", round(comp_a, 3))
  
} else {comp_39 <- c("Winterzeit", "untersch. Levels")} }

{if(nlevels(donor$Sommerpause) == nlevels(recipient$Sommerpause)){
  
  
  tt.R <- xtabs(~ Sommerpause, data = recipient) #Empfänger
  tt.D <- xtabs(~ Sommerpause, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_40 <- c("Sommerpause", round(comp_a, 3))
  
} else {comp_40 <- c("Sommerpause", "untersch. Levels")} }

{if(nlevels(donor$abnulluhr) == nlevels(recipient$abnulluhr)){
  
  
  tt.R <- xtabs(~ abnulluhr, data = recipient) #Empfänger
  tt.D <- xtabs(~ abnulluhr, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_41 <- c("abnulluhr", round(comp_a, 3))
  
} else {comp_41 <- c("abnulluhr", "untersch. Levels")} }

{if(nlevels(donor$abvieruhr) == nlevels(recipient$abvieruhr)){
  
  
  tt.R <- xtabs(~ abvieruhr, data = recipient) #Empfänger
  tt.D <- xtabs(~ abvieruhr, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_42 <- c("abvieruhr", round(comp_a, 3))
  
} else {comp_42 <- c("abvieruhr", "untersch. Levels")} }

{if(nlevels(donor$abfuenfuhrdreissig) == nlevels(recipient$abfuenfuhrdreissig)){
  
  
  tt.R <- xtabs(~ abfuenfuhrdreissig, data = recipient) #Empfänger
  tt.D <- xtabs(~ abfuenfuhrdreissig, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_43 <- c("abfuenfuhrdreissig", round(comp_a, 3))
  
} else {comp_43 <- c("abfuenfuhrdreissig", "untersch. Levels")} }

{if(nlevels(donor$absechsuhrdreissig) == nlevels(recipient$absechsuhrdreissig)){
  
  
  tt.R <- xtabs(~ absechsuhrdreissig, data = recipient) #Empfänger
  tt.D <- xtabs(~ absechsuhrdreissig, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_44 <- c("absechsuhrdreissig", round(comp_a, 3))
  
} else {comp_44 <- c("absechsuhrdreissig", "untersch. Levels")} }

{if(nlevels(donor$abdreizehnuhrdreissig) == nlevels(recipient$abdreizehnuhrdreissig)){
  
  
  tt.R <- xtabs(~ abdreizehnuhrdreissig, data = recipient) #Empfänger
  tt.D <- xtabs(~ abdreizehnuhrdreissig, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_45 <- c("abdreizehnuhrdreissig", round(comp_a, 3))
  
} else {comp_45 <- c("abdreizehnuhrdreissig", "untersch. Levels")} }

{if(nlevels(donor$abachtzehnuhr) == nlevels(recipient$abachtzehnuhr)){
  
  
  tt.R <- xtabs(~ abachtzehnuhr, data = recipient) #Empfänger
  tt.D <- xtabs(~ abachtzehnuhr, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_46 <- c("abachtzehnuhr", round(comp_a, 3))
  
} else {comp_46 <- c("abachtzehnuhr", "untersch. Levels")} }

{if(nlevels(donor$Samstag) == nlevels(recipient$Samstag)){
  
  
  tt.R <- xtabs(~ Samstag, data = recipient) #Empfänger
  tt.D <- xtabs(~ Samstag, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_47 <- c("Samstag", round(comp_a, 3))
  
} else {comp_47 <- c("Samstag", "untersch. Levels")} }

{if(nlevels(donor$Sonntag) == nlevels(recipient$Sonntag)){
  
  
  tt.R <- xtabs(~ Sonntag, data = recipient) #Empfänger
  tt.D <- xtabs(~ Sonntag, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_48 <- c("Sonntag", round(comp_a, 3))
  
} else {comp_48 <- c("Sonntag", "untersch. Levels")} }

{if(nlevels(donor$Kind) == nlevels(recipient$Kind)){
  
  
  tt.R <- xtabs(~ Kind, data = recipient) #Empfänger
  tt.D <- xtabs(~ Kind, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_49 <- c("Kind", round(comp_a, 3))
  
} else {comp_49 <- c("Kind", "untersch. Levels")} }

{if(nlevels(donor$Jugendlich) == nlevels(recipient$Jugendlich)){
  
  
  tt.R <- xtabs(~ Jugendlich, data = recipient) #Empfänger
  tt.D <- xtabs(~ Jugendlich, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_50 <- c("Jugendlich", round(comp_a, 3))
  
} else {comp_50 <- c("Jugendlich", "untersch. Levels")} }

{if(nlevels(donor$schwerverletzt_1) == nlevels(recipient$schwerverletzt_1)){
  
  
  tt.R <- xtabs(~ schwerverletzt_1, data = recipient) #Empfänger
  tt.D <- xtabs(~ schwerverletzt_1, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_51 <- c("schwerverletzt_1", round(comp_a, 3))
  
} else {comp_51 <- c("schwerverletzt_1", "untersch. Levels")} }

{if(nlevels(donor$Schwerverletzt_2bis3) == nlevels(recipient$Schwerverletzt_2bis3)){
  
  
  tt.R <- xtabs(~ Schwerverletzt_2bis3, data = recipient) #Empfänger
  tt.D <- xtabs(~ Schwerverletzt_2bis3, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_52 <- c("Schwerverletzt_2bis3", round(comp_a, 3))
  
} else {comp_52 <- c("Schwerverletzt_2bis3", "untersch. Levels")} }

{if(nlevels(donor$Schwerverletzt_ab3) == nlevels(recipient$Schwerverletzt_ab3)){
  
  
  tt.R <- xtabs(~ Schwerverletzt_ab3, data = recipient) #Empfänger
  tt.D <- xtabs(~ Schwerverletzt_ab3, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_53 <- c("Schwerverletzt_ab3", round(comp_a, 3))
  
} else {comp_53 <- c("Schwerverletzt_ab3", "untersch. Levels")} }

{if(nlevels(donor$leichtverletzt_1) == nlevels(recipient$leichtverletzt_1)){
  
  
  tt.R <- xtabs(~ leichtverletzt_1, data = recipient) #Empfänger
  tt.D <- xtabs(~ leichtverletzt_1, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_54 <- c("leichtverletzt_1", round(comp_a, 3))
  
} else {comp_54 <- c("leichtverletzt_1", "untersch. Levels")} }

{if(nlevels(donor$leichtverletzt_2bis4) == nlevels(recipient$leichtverletzt_2bis4)){
  
  
  tt.R <- xtabs(~ leichtverletzt_2bis4, data = recipient) #Empfänger
  tt.D <- xtabs(~ leichtverletzt_2bis4, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_55 <- c("leichtverletzt_2bis4", round(comp_a, 3))
  
} else {comp_55 <- c("leichtverletzt_2bis4", "untersch. Levels")} }

{if(nlevels(donor$leichtverletzt_ab4) == nlevels(recipient$leichtverletzt_ab4)){
  
  
  tt.R <- xtabs(~ leichtverletzt_ab4, data = recipient) #Empfänger
  tt.D <- xtabs(~ leichtverletzt_ab4, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_56 <- c("leichtverletzt_ab4", round(comp_a, 3))
  
} else {comp_56 <- c("leichtverletzt_ab4", "untersch. Levels")} }

{if(nlevels(donor$beteiligte_1) == nlevels(recipient$beteiligte_1)){
  
  
  tt.R <- xtabs(~ beteiligte_1, data = recipient) #Empfänger
  tt.D <- xtabs(~ beteiligte_1, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_57 <- c("beteiligte_1", round(comp_a, 3))
  
} else {comp_57 <- c("beteiligte_1", "untersch. Levels")} }

{if(nlevels(donor$beteiligte_3bis5) == nlevels(recipient$beteiligte_3bis5)){
  
  
  tt.R <- xtabs(~ beteiligte_3bis5, data = recipient) #Empfänger
  tt.D <- xtabs(~ beteiligte_3bis5, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_58 <- c("beteiligte_3bis5", round(comp_a, 3))
  
} else {comp_58 <- c("beteiligte_3bis5", "untersch. Levels")} }

{if(nlevels(donor$beteiligte_ab5) == nlevels(recipient$beteiligte_ab5)){
  
  
  tt.R <- xtabs(~ beteiligte_ab5, data = recipient) #Empfänger
  tt.D <- xtabs(~ beteiligte_ab5, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_59 <- c("beteiligte_ab5", round(comp_a, 3))
  
} else {comp_59 <- c("beteiligte_ab5", "untersch. Levels")} }

{if(nlevels(donor$bis1000euro) == nlevels(recipient$bis1000euro)){
  
  
  tt.R <- xtabs(~ bis1000euro, data = recipient) #Empfänger
  tt.D <- xtabs(~ bis1000euro, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_60 <- c("bis1000euro", round(comp_a, 3))
  
} else {comp_60 <- c("bis1000euro", "untersch. Levels")} }

{if(nlevels(donor$bis3000euro) == nlevels(recipient$bis3000euro)){
  
  
  tt.R <- xtabs(~ bis3000euro, data = recipient) #Empfänger
  tt.D <- xtabs(~ bis3000euro, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_61 <- c("bis3000euro", round(comp_a, 3))
  
} else {comp_61 <- c("bis3000euro", "untersch. Levels")} }

{if(nlevels(donor$bis5000euro) == nlevels(recipient$bis5000euro)){
  
  
  tt.R <- xtabs(~ bis5000euro, data = recipient) #Empfänger
  tt.D <- xtabs(~ bis5000euro, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_62 <- c("bis5000euro", round(comp_a, 3))
  
} else {comp_62 <- c("bis5000euro", "untersch. Levels")} }


{if(nlevels(donor$bis8000euro) == nlevels(recipient$bis8000euro)){
  
  
  tt.R <- xtabs(~ bis8000euro, data = recipient) #Empfänger
  tt.D <- xtabs(~ bis8000euro, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_63 <- c("bis8000euro", round(comp_a, 3))
  
} else {comp_63 <- c("bis8000euro", "untersch. Levels")} }

{if(nlevels(donor$bis16000euro) == nlevels(recipient$bis16000euro)){
  
  
  tt.R <- xtabs(~ bis16000euro, data = recipient) #Empfänger
  tt.D <- xtabs(~ bis16000euro, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_64 <- c("bis16000euro", round(comp_a, 3))
  
} else {comp_64 <- c("bis16000euro", "untersch. Levels")} }

{if(nlevels(donor$Fahrtrichtung_angegeben) == nlevels(recipient$Fahrtrichtung_angegeben)){
  
  
  tt.R <- xtabs(~ Fahrtrichtung_angegeben, data = recipient) #Empfänger
  tt.D <- xtabs(~ Fahrtrichtung_angegeben, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_65 <- c("Fahrtrichtung_angegeben", round(comp_a, 3))
  
} else {comp_65 <- c("Fahrtrichtung_angegeben", "untersch. Levels")} }

{if(nlevels(donor$autobahn) == nlevels(recipient$autobahn)){
  
  
  tt.R <- xtabs(~ autobahn, data = recipient) #Empfänger
  tt.D <- xtabs(~ autobahn, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_66 <- c("autobahn", round(comp_a, 3))
  
} else {comp_66 <- c("autobahn", "untersch. Levels")} }

{if(nlevels(donor$bundesstrasse) == nlevels(recipient$bundesstrasse)){
  
  
  tt.R <- xtabs(~ bundesstrasse, data = recipient) #Empfänger
  tt.D <- xtabs(~ bundesstrasse, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_67 <- c("bundesstrasse", round(comp_a, 3))
  
} else {comp_67 <- c("bundesstrasse", "untersch. Levels")} }

{if(nlevels(donor$landesstrasse) == nlevels(recipient$landesstrasse)){
  
  
  tt.R <- xtabs(~ landesstrasse, data = recipient) #Empfänger
  tt.D <- xtabs(~ landesstrasse, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_68 <- c("landesstrasse", round(comp_a, 3))
  
} else {comp_68 <- c("landesstrasse", "untersch. Levels")} }

{if(nlevels(donor$kreisstrasse) == nlevels(recipient$kreisstrasse)){
  
  
  tt.R <- xtabs(~ kreisstrasse, data = recipient) #Empfänger
  tt.D <- xtabs(~ kreisstrasse, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_69 <- c("kreisstrasse", round(comp_a, 3))
  
} else {comp_69 <- c("kreisstrasse", "untersch. Levels")} }

{if(nlevels(donor$unfallart_1) == nlevels(recipient$unfallart_1)){
  
  
  tt.R <- xtabs(~ unfallart_1, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_1, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_70 <- c("unfallart_1", round(comp_a, 3))
  
} else {comp_70 <- c("unfallart_1", "untersch. Levels")} }

{if(nlevels(donor$unfallart_2) == nlevels(recipient$unfallart_2)){
  
  
  tt.R <- xtabs(~ unfallart_2, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_2, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_71 <- c("unfallart_2", round(comp_a, 3))
  
} else {comp_71 <- c("unfallart_2", "untersch. Levels")} }

{if(nlevels(donor$unfallart_3) == nlevels(recipient$unfallart_3)){
  
  
  tt.R <- xtabs(~ unfallart_3, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_3, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_72 <- c("unfallart_3", round(comp_a, 3))
  
} else {comp_72 <- c("unfallart_3", "untersch. Levels")} }

{if(nlevels(donor$unfallart_4) == nlevels(recipient$unfallart_4)){
  
  
  tt.R <- xtabs(~ unfallart_4, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_4, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_73 <- c("unfallart_4", round(comp_a, 3))
  
} else {comp_73 <- c("unfallart_4", "untersch. Levels")} }

{if(nlevels(donor$unfallart_5) == nlevels(recipient$unfallart_5)){
  
  
  tt.R <- xtabs(~ unfallart_5, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_5, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_74 <- c("unfallart_5", round(comp_a, 3))
  
} else {comp_74 <- c("unfallart_5", "untersch. Levels")} }

{if(nlevels(donor$unfallart_6) == nlevels(recipient$unfallart_6)){
  
  
  tt.R <- xtabs(~ unfallart_6, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_6, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_75 <- c("unfallart_6", round(comp_a, 3))
  
} else {comp_75 <- c("unfallart_6", "untersch. Levels")} }

{if(nlevels(donor$unfallart_7) == nlevels(recipient$unfallart_7)){
  
  
  tt.R <- xtabs(~ unfallart_7, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_7, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_76 <- c("unfallart_7", round(comp_a, 3))
  
} else {comp_76 <- c("unfallart_7", "untersch. Levels")} }

{if(nlevels(donor$unfallart_8) == nlevels(recipient$unfallart_8)){
  
  
  tt.R <- xtabs(~ unfallart_8, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_8, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_24 <- c("unfallart_8", round(comp_a, 3)) 
  #comp_24, weil das oben entfernt wurde
} else {comp_24 <- c("unfallart_8", "untersch. Levels")} }

{if(nlevels(donor$unfallart_9) == nlevels(recipient$unfallart_9)){
  
  
  tt.R <- xtabs(~ unfallart_9, data = recipient) #Empfänger
  tt.D <- xtabs(~ unfallart_9, data = donor) #Spender 
  
  comp_a <- StatMatch::comp.prop(p1=tt.R, p2=tt.D, n1 = nrow(recipient), n2 = nrow(donor), ref = FALSE) #Verteilungsvergleich
  comp_a <- comp_a$meas[[4]] #Ablegen der 4. Ausgabe der Comp.prop Funktion, welches die Hellinger-Distanz ist
  comp_27 <- c("unfallart_9", round(comp_a, 3))
  #comp_27, weil das oben entfernt wurde
} else {comp_27 <- c("unfallart_9", "untersch. Levels")} }

comp_uebersicht <- rbind(comp_1, comp_2, comp_3, comp_4, comp_5, comp_6, comp_7, comp_8, comp_9, comp_10, comp_11, comp_12, comp_13, comp_14, comp_15, comp_16, comp_17, comp_18, comp_19, comp_20, comp_21, comp_22, comp_23, comp_24, comp_25, comp_26, comp_27, comp_28, comp_29, comp_30, comp_31, comp_32, comp_33, comp_34, comp_35, comp_36, comp_37, comp_38, comp_39, comp_40, comp_41, comp_42, comp_43, comp_44, comp_45, comp_46, comp_47, comp_48, comp_49, comp_50, comp_51, comp_52, comp_53, comp_54, comp_55, comp_56, comp_57, comp_58, comp_59, comp_60, comp_61, comp_62, comp_63, comp_64, comp_65, comp_66, comp_67, comp_68, comp_69, comp_70, comp_71, comp_72, comp_73, comp_74, comp_75, comp_76)

print(comp_uebersicht)

#Es muessen keine Variablen weggelassen werden wegen schlechter Verteilung / falscher Population.

write.csv2(comp_uebersicht, paste(daten,"/ergebnisse/4_hellinger_distanzen_20k_5k.csv", sep = ""))
