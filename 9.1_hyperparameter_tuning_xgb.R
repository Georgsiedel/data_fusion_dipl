library(caret)
library(ggplot2)
library(data.table)
library(dplyr)
library(StatMatch)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")
#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
donor <- donor[, -1]
recipient <- recipient[, -1]

donor <- select(donor
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

recipient <- select(recipient
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

#9.1 nrounds, eta, max_depth, gamma


start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid")
tunegrid <- expand.grid(
  nrounds = c(3:8),
  eta = c(0.3,0.4,0.5,0.6,0.7),
  max_depth = c(8:20),
  gamma = c(0,0.2,0.5,1,2,5,10,20),
  colsample_bytree=c(1), 
  min_child_weight=c(1),
  subsample = c(1)
)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "xgbTree", metric="Accuracy", tuneGrid = tunegrid, trControl = control)

print(model)
# Test/Vorhersage mit predict-Funktion

AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

#Auswertung Genauigkeit (Ebene 1)

accuracy <- confusionMatrix(prediction, AccidentType)$overall["Accuracy"]
print(accuracy)
print(confusionMatrix(prediction, AccidentType)$byClass)

#9.2 Gamma, Colsample, minchildweight, subsample

start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid")
tunegrid <- expand.grid(
  nrounds = c(7),
  eta = c(0.3),
  max_depth = c(19),
  gamma = c(1.5,2,2.5,3),
  colsample_bytree=c(0.25,0.5,0.75,1), 
  min_child_weight=c(0.2,0.5,1,2,5,10),
  subsample = c(0.1,0.25,0.5,0.75,1)
)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "xgbTree", metric="Accuracy", tuneGrid = tunegrid, trControl = control)

print(model)
# Test/Vorhersage mit predict-Funktion

AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

# Auswertung Genauigkeit (Ebene 1)

accuracy <- confusionMatrix(prediction, AccidentType)$overall["Accuracy"]
print(accuracy)
print(confusionMatrix(prediction, AccidentType)$byClass)

end_time <- Sys.time()
end_time - start_time

#9.3 Feintuning

start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "grid")
tunegrid <- expand.grid(
  nrounds = c(6,7,8),
  eta = c(0.25,0.3,0.35),
  max_depth = c(18,19,20),
  gamma = c(2),
  colsample_bytree=c(0.7,0.8,0.9), 
  min_child_weight=c(0.1,0.15,0.2,0.3,0.4),
  subsample = c(1)
)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "xgbTree", metric="Accuracy", tuneGrid = tunegrid, trControl = control)

print(model)
# Test/Vorhersage mit predict-Funktion

AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

# Auswertung Genauigkeit (Ebene 1)

accuracy <- confusionMatrix(prediction, AccidentType)$overall["Accuracy"]
print(accuracy)
print(confusionMatrix(prediction, AccidentType)$byClass)

end_time <- Sys.time()
end_time - start_time