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

# Runde 1

start_time <- Sys.time()

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial" )

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

# Runde 2

start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid")

tunegrid <- expand.grid(sigma = c(0.01,0.0125,0.015,0.0175,0.02), C = c(0.8,0.9,1,1.1,1.2))

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial",
               tuneGrid = tunegrid, trControl = control)

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

# Runde 3

start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid")

tunegrid <- expand.grid(sigma = c(0.005,0.0075,0.009,0.01), C = c(1.1,1.2,1.3,1.4,1.5,1.75,2,2.5))

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial",
               tuneGrid = tunegrid, trControl = control)

print(model)
# Test/Vorhersage mit predict-Funktion

AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

# uswertung Genauigkeit (Ebene 1)

accuracy <- confusionMatrix(prediction, AccidentType)$overall["Accuracy"]
print(accuracy)
print(confusionMatrix(prediction, AccidentType)$byClass)

end_time <- Sys.time()
end_time - start_time

#Runde 4

start_time <- Sys.time()

control <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "grid")

tunegrid <- expand.grid(sigma = c(0.009,0.01,0.011,0.012), C = c(2.5,2.75,3,3.5))
model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial",
               tuneGrid = tunegrid, trControl = control)

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