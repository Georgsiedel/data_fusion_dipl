library(randomForest)
library(caret)
library(ggplot2)
library(data.table)
library(e1071)
library(dplyr)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

### 7. Feature Reduction

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")

#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
donor <- donor[, -1]
donor <- select(donor, -Aufprall_Hindernis) #Aufprall Hindernis ist spezifische Variable
recipient <- recipient[, -1]

#Sortieren der Features nach ermittelter Feature Importance, absteigend
donor <- select(donor
                ,Schwerverletzt_ab3
                ,BadRoad
                ,Schwerverletzt_2bis3
                ,beteiligte_ab5
                ,Train
                ,leichtverletzt_ab4
                ,Sommerpause
                ,Fatalities
                ,Sonntag
                ,Tree
                ,Samstag
                ,abachtzehnuhr
                ,BadWeather
                ,Winterzeit
                ,abvieruhr
                ,Jugendlich
                ,ObstacleOnRoad
                ,abdreizehnuhrdreissig
                ,abfuenfuhrdreissig
                ,leichtverletzt_2bis4
                ,absechsuhrdreissig
                ,abnulluhr
                ,Crosswalk
                ,Slippery
                ,Roundabout
                ,Motorcycle
                ,unfallart_7
                ,Tram
                ,beteiligte_3bis5
                ,Darkness
                ,Slope
                ,bis5000euro
                ,Kind
                ,bis3000euro
                ,bis8000euro
                ,schwerverletzt_1
                ,Alcohol
                ,autobahn
                ,EnvironmentalReason
                ,bis16000euro
                ,RedLight
                ,unfallart_9
                ,Truck
                ,kreisstrasse
                ,Pedestrian
                ,bis1000euro
                ,Car
                ,unfallart_6
                ,Runaway
                ,ObstacleOffRoad
                ,unfallart_8
                ,leichtverletzt_1
                ,Urban
                ,Bus
                ,landesstrasse
                ,Overtaking
                ,unfallart_2
                ,TrafficLights
                ,Curve
                ,Animal
                ,bundesstrasse
                ,Priority
                ,beteiligte_1
                ,unfallart_1
                ,Bicycle
                ,Driveway
                ,Intersection
                ,unfallart_4
                ,unfallart_3
                ,Turning
                ,Fahrtrichtung_angegeben
                ,unfallart_5
                ,Distance
                ,Speeding
                ,AccidentType
)

#7.1 Grober Feature reduction Durchlauf

#Tuning der nach VIM geordneten Features
#5fold x1, mit ermittelten Hyperparametern
control <- trainControl(method = "repeatedcv", number = 5, repeats = 1, search = "grid")
tunegrid <- expand.grid(.mtry = 9) #Aus Grid-Search ermitteltes mtry und ntree (weiter unten)
metric <- "Accuracy"

start_time <- Sys.time()
modellist <- list()
for(x in c(0:12)) {
  set.seed(1)
  fit <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control, ntree = 2000)
  key <- toString(74 - x * 5)
  modellist[[key]] <- fit  
  donor <- donor[, -(1:5)]  
}

#Mit 74 bis zu 14 wichtigsten Features in 5er Abständen reduziert

end_time <- Sys.time()

results <- resamples(modellist)
end_time - start_time
summary(results)
dotplot(results)

#7.2 Feiner Durchlauf 

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")

#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
donor <- donor[, -1]
recipient <- recipient[, -1]

donor <- select(donor
                ,Schwerverletzt_ab3
                ,BadRoad
                ,Schwerverletzt_2bis3
                ,beteiligte_ab5
                ,Train
                ,leichtverletzt_ab4
                ,Sommerpause
                ,Fatalities
                ,Sonntag
                ,Tree
                ,Samstag
                ,abachtzehnuhr
                ,BadWeather
                ,Winterzeit
                ,abvieruhr
                ,Jugendlich
                ,ObstacleOnRoad
                ,abdreizehnuhrdreissig
                ,abfuenfuhrdreissig
                ,leichtverletzt_2bis4
                ,absechsuhrdreissig
                ,abnulluhr
                ,Crosswalk
                ,Slippery
                ,Roundabout
                ,Motorcycle
                ,unfallart_7
                ,Tram
                ,beteiligte_3bis5
                ,Darkness
                ,Slope
                ,bis5000euro
                ,Kind
                ,bis3000euro
                ,bis8000euro
                ,schwerverletzt_1
                ,Alcohol
                ,autobahn
                ,EnvironmentalReason
                ,bis16000euro
                ,RedLight
                ,unfallart_9
                ,Truck
                ,kreisstrasse
                ,Pedestrian
                ,bis1000euro
                ,Car
                ,unfallart_6
                ,Runaway
                ,ObstacleOffRoad
                ,unfallart_8
                ,leichtverletzt_1
                ,Urban
                ,Bus
                ,landesstrasse
                ,Overtaking
                ,unfallart_2
                ,TrafficLights
                ,Curve
                ,Animal
                ,bundesstrasse
                ,Priority
                ,beteiligte_1
                ,unfallart_1
                ,Bicycle
                ,Driveway
                ,Intersection
                ,unfallart_4
                ,unfallart_3
                ,Turning
                ,Fahrtrichtung_angegeben
                ,unfallart_5
                ,Distance
                ,Speeding
                ,AccidentType
)

#Tuning der nach VIM geordneten Features
#10fold x2
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "grid")
tunegrid <- expand.grid(.mtry = 9) #Aus Grid-Search ermitteltes mtry und ntree (weiter unten)
metric <- "Accuracy"

donor <- donor[, -(1:7)]

start_time <- Sys.time()
modellist <- list()
for(x in c(0:12)) {
  set.seed(1)
  fit <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control, ntree = 2000)
  key <- toString(67 - x * 3)
  modellist[[key]] <- fit  
  donor <- donor[, -1]  
}

#67 bis 55 wichtigste Features mit 1er Abstand der Reduktion

end_time <- Sys.time()

results <- resamples(modellist)
end_time - start_time
summary(results)
dotplot(results)
