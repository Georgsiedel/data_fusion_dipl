library(caret)
library(ggplot2)
library(data.table)
library(dplyr)
library(StatMatch)
library(writexl)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")

### 13. Ergebnisvergleich

#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
donor <- donor[, -1]
recipient <- recipient[, -1]

AccidentType_rec <- select(recipient, AccidentType)

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
)

start_time <- Sys.time()
start_time

X.mtc <- c("abdreizehnuhrdreissig",
           "abfuenfuhrdreissig",
           "leichtverletzt_2bis4",
           "absechsuhrdreissig",
           "abnulluhr",
           "Crosswalk",
           "Slippery",
           "Roundabout",
           "Motorcycle",
           "unfallart_7",
           "Tram",
           "beteiligte_3bis5",
           "Darkness",
           "Slope",
           "bis5000euro",
           "Kind",
           "bis3000euro",
           "bis8000euro",
           "schwerverletzt_1",
           "Alcohol",
           "autobahn",
           "EnvironmentalReason",
           "bis16000euro",
           "RedLight",
           "unfallart_9",
           "Truck",
           "kreisstrasse",
           "Pedestrian",
           "bis1000euro",
           "Car",
           "unfallart_6",
           "Runaway",
           "ObstacleOffRoad",
           "unfallart_8",
           "leichtverletzt_1",
           "Urban",
           "Bus",
           "landesstrasse",
           "Overtaking",
           "unfallart_2",
           "TrafficLights",
           "Curve",
           "Animal",
           "bundesstrasse",
           "Priority",
           "beteiligte_1",
           "unfallart_1",
           "Bicycle",
           "Driveway",
           "Intersection",
           "unfallart_4",
           "unfallart_3",
           "Turning",
           "Fahrtrichtung_angegeben",
           "unfallart_5",
           "Distance",
           "Speeding"
)

### 1.Hotdeck

out.dhd <- NND.hotdeck(data.rec = recipient, data.don = donor, match.vars = X.mtc, dist.fun = "gower", constrained = TRUE)
#Ergebnis ohne Spenderklassen, mit MV über RandomForests

#Erstellen des Fusionsdatensatzes mit richtigem und vorhergesagtem AccidentType sowie Aufprall_Hindernis
recipient_dhd <- create.fused(data.rec = recipient, data.don = donor, mtc.ids = out.dhd$mtc.ids, z.vars = "AccidentType")
#Wiedereinfügen der spezifischen Hilfsvariable Aufprall_Hindernis
#vorhergesagter AccidentType wird zu "AccidentType_pred", der alte wieder zu AccidentType
recipient_dhd <- rename(recipient_dhd, AccidentType_pred = AccidentType)
recipient_dhd <- mutate(recipient_dhd, AccidentType = AccidentType_rec$AccidentType)

anzGleich <- 0
anzUngleich <- 0

for (i in 1:nrow(recipient)) {
  
  if (recipient_dhd$AccidentType[i]		==	recipient_dhd$AccidentType_pred[i])
    anzGleich = anzGleich + 1
  else {
    anzUngleich = anzUngleich + 1
  }
}
accuracy_dhd <- anzGleich/nrow(recipient)
print(accuracy_dhd)

dhd_time <- Sys.time()
print(dhd_time-start_time)

recipient <- mutate(recipient, AccidentType = AccidentType_rec$AccidentType)

### 2.RF

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tunegrid <- expand.grid(mtry = 9)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", tuneGrid = tunegrid, trControl = control, ntree = 2000)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

accuracy <- confusionMatrix(prediction, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy)

recipient_rf <- mutate(recipient, AccidentType_pred = prediction)

rf_time <- Sys.time()
print(rf_time-dhd_time)

### 3. xgB

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tunegrid <- expand.grid(
  nrounds = 8,
  eta = 0.25,
  max_depth = 19,
  gamma = 2,
  colsample_bytree= 0.8, 
  min_child_weight= 0.1,
  subsample = 1)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "xgbTree", tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

accuracy <- confusionMatrix(prediction, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy)

recipient_xgb <- mutate(recipient, AccidentType_pred = prediction)

xgb_time <- Sys.time()
print(xgb_time-rf_time)

### 4.nnet

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")

tunegrid <- expand.grid(size = 10, decay = 0.2)

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "nnet",
               tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

accuracy <- confusionMatrix(prediction, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy)

recipient_nnet <- mutate(recipient, AccidentType_pred = prediction)

nnet_time <- Sys.time()
print(nnet_time-xgb_time)

### 5.svm

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")

tunegrid <- expand.grid(sigma = 0.009, C = 3.5)
model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial",
               tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction <- predict(model, recipient)

accuracy <- confusionMatrix(prediction, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy)

recipient_svm <- mutate(recipient, AccidentType_pred = prediction)

svm_time <- Sys.time()
print(svm_time-nnet_time)

### Vergleich der Gemeinsamkeiten der Modelle


ergebnisse <- cbind(recipient_dhd$AccidentType, recipient_dhd$AccidentType_pred, recipient_rf$AccidentType_pred, recipient_xgb$AccidentType_pred, recipient_nnet$AccidentType_pred, recipient_svm$AccidentType_pred)

colnames(ergebnisse) <- c("AccidentType", "AccidentType_dhd", "AccidentType_rf", "AccidentType_xgb", "AccidentType_nnet", "AccidentType_svm")
ergebnisse <- as.data.frame(ergebnisse)

#Paarweiser Vergleich zwischen zwei Modellen, wie oft beide richig liegen (pp), beide falsch (nn), und wie oft einer richtig und einer falsch (pn)
#Ziel: paarweise ConfusionMatrix, um Erkenntnisse fürs Ensembling zu gewinnen


#Erstellung Counter
dhd_rf_pp <- 0
dhd_xgb_pp <- 0
dhd_nnet_pp <- 0
dhd_svm_pp <- 0
rf_xgb_pp <- 0
rf_nnet_pp <- 0
rf_svm_pp <- 0
xgb_nnet_pp <- 0
xgb_svm_pp <- 0
nnet_svm_pp <- 0
dhd_rf_nn <- 0
dhd_xgb_nn <- 0
dhd_nnet_nn <- 0
dhd_svm_nn <- 0
rf_xgb_nn <- 0
rf_nnet_nn <- 0
rf_svm_nn <- 0
xgb_nnet_nn <- 0
xgb_svm_nn <- 0
nnet_svm_nn <- 0
dhd_rf_pn <- 0
dhd_xgb_pn <- 0
dhd_nnet_pn <- 0
dhd_svm_pn <- 0
rf_xgb_pn <- 0
rf_nnet_pn <- 0
rf_svm_pn <- 0
xgb_nnet_pn <- 0
xgb_svm_pn <- 0
nnet_svm_pn <- 0

c <- vector()

for (i in 1:nrow(ergebnisse)) {
  
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_rf[i] & ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i]){
    dhd_rf_pp = dhd_rf_pp + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_xgb[i] & ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i]){
    dhd_xgb_pp = dhd_xgb_pp + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i]){
    dhd_nnet_pp = dhd_nnet_pp + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i]){
    dhd_svm_pp = dhd_svm_pp + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_xgb[i] & ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i]){
    rf_xgb_pp = rf_xgb_pp + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i]){
    rf_nnet_pp = rf_nnet_pp + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i]){
    rf_svm_pp = rf_svm_pp + 1
  }
  if (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i]){
    xgb_nnet_pp = xgb_nnet_pp + 1
  }
  if (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i]){
    xgb_svm_pp = xgb_svm_pp + 1
  }
  if (ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType[i]){
    nnet_svm_pp = nnet_svm_pp + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_rf[i] & ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType[i]){
    dhd_rf_nn = dhd_rf_nn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_xgb[i] & ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType[i]){
    dhd_xgb_nn = dhd_xgb_nn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType[i]){
    dhd_nnet_nn = dhd_nnet_nn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType[i]){
    dhd_svm_nn = dhd_svm_nn + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_xgb[i] & ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType[i]){
    rf_xgb_nn = rf_xgb_nn + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType[i]){
    rf_nnet_nn = rf_nnet_nn + 1
  }
  if (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType[i]){
    rf_svm_nn = rf_svm_nn + 1
  }
  if (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType_nnet[i] & ergebnisse$AccidentType_xgb[i] != ergebnisse$AccidentType[i]){
    xgb_nnet_nn = xgb_nnet_nn + 1
  }
  if (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_xgb[i] != ergebnisse$AccidentType[i]){
    xgb_svm_nn = xgb_svm_nn + 1
  }
  if (ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType_svm[i] & ergebnisse$AccidentType_nnet[i] != ergebnisse$AccidentType[i]){
    nnet_svm_nn = nnet_svm_nn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType_rf[i] & (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i])){
    dhd_rf_pn = dhd_rf_pn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType_xgb[i] & (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i])){
    dhd_xgb_pn = dhd_xgb_pn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType_nnet[i] & (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType[i])){
    dhd_nnet_pn = dhd_nnet_pn + 1
  }
  if (ergebnisse$AccidentType_dhd[i] != ergebnisse$AccidentType_svm[i] & (ergebnisse$AccidentType_dhd[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_svm[i] == ergebnisse$AccidentType[i])){
    dhd_svm_pn = dhd_svm_pn + 1
  }
  if (ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType_xgb[i] & (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i])){
    rf_xgb_pn = rf_xgb_pn + 1
  }
  if (ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType_nnet[i] & (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType[i])){
    rf_nnet_pn = rf_nnet_pn + 1
  }
  if (ergebnisse$AccidentType_rf[i] != ergebnisse$AccidentType_svm[i] & (ergebnisse$AccidentType_rf[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_svm[i] == ergebnisse$AccidentType[i])){
    rf_svm_pn = rf_svm_pn + 1
  }
  if (ergebnisse$AccidentType_xgb[i] != ergebnisse$AccidentType_nnet[i] & (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType[i])){
    xgb_nnet_pn = xgb_nnet_pn + 1
  }
  if (ergebnisse$AccidentType_xgb[i] != ergebnisse$AccidentType_svm[i] & (ergebnisse$AccidentType_xgb[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_svm[i] == ergebnisse$AccidentType[i])){
    xgb_svm_pn = xgb_svm_pn + 1
  }
  if (ergebnisse$AccidentType_nnet[i] != ergebnisse$AccidentType_svm[i] & (ergebnisse$AccidentType_nnet[i] == ergebnisse$AccidentType[i] | ergebnisse$AccidentType_svm[i] == ergebnisse$AccidentType[i])){
    nnet_svm_pn = nnet_svm_pn + 1
  }
  #Ausgabe der Anzahl verschiedener VOrhersagen bei jedem Eintrag
  c <-c(c, length(levels(as.factor(ergebnisse[i,2:6]))))
}
#Auswertung der Anzahl verschiedener Vorhersagen, um Erkenntnisse für das Majority Vote zu gewinnen
anzahl <- hist(c, breaks = c(0.5,1.5,2.5,3.5,4.5,5.5), plot = FALSE)
print(anzahl$counts)


list_pp <- list(dhd_rf_pp,
                dhd_xgb_pp,
                dhd_nnet_pp,
                dhd_svm_pp,
                rf_xgb_pp,
                rf_nnet_pp,
                rf_svm_pp,
                xgb_nnet_pp,
                xgb_svm_pp,
                nnet_svm_pp)

list_pn <- list(dhd_rf_pn,
                dhd_xgb_pn,
                dhd_nnet_pn,
                dhd_svm_pn,
                rf_xgb_pn,
                rf_nnet_pn,
                rf_svm_pn,
                xgb_nnet_pn,
                xgb_svm_pn,
                nnet_svm_pn)

list_nn <- list(dhd_rf_nn,
                dhd_xgb_nn,
                dhd_nnet_nn,
                dhd_svm_nn,
                rf_xgb_nn,
                rf_nnet_nn,
                rf_svm_nn,
                xgb_nnet_nn,
                xgb_svm_nn,
                nnet_svm_nn)

list <- cbind(list_pp, list_pn, list_nn)
print(list)

end_time <- Sys.time()
end_time - start_time