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


control <- trainControl(method = "none", search = "grid")

tunegrid <- expand.grid(size = c(8:12), decay = c(0.05,0.075,0.1,0.15,0.2,0.25))

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "nnet",
               tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

accuracy <- confusionMatrix(prediction, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy)
