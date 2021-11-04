library(caret)
library(ggplot2)
library(data.table)
library(dplyr)
library(StatMatch)
library(writexl)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")

### 15. Test und Auswertung Stacking-Ensemble

#Es wird mit den 4 ML-Modellen recipient UND donor vorhergesagt und mit den Ergebnissen der Classifier als zusätzliche Variable mittel DHD gematcht.

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
                    ,AccidentType
)

start_time <- Sys.time()
start_time

### 15.1.RF

control <- trainControl(method = "none", search = "grid")
tunegrid <- expand.grid(mtry = 9)

model <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", tuneGrid = tunegrid, trControl = control, ntree = 2000)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction_rec <- predict(model, recipient)
prediction_don <- predict(model, donor)

accuracy_rec <- confusionMatrix(prediction_rec, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy_rec)
accuracy_don <- confusionMatrix(prediction_don, as.factor(donor$AccidentType))$overall["Accuracy"]
print(accuracy_don)

recipient_rf <- mutate(recipient, AccidentType_pred_rec = prediction_rec)
donor_rf <- mutate(donor, AccidentType_pred_don = prediction_don)

rf_time <- Sys.time()
print(rf_time-start_time)

### 15.2. xgB

control <- trainControl(method = "none", search = "grid")
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
prediction_rec <- predict(model, recipient)
prediction_don <- predict(model, donor)

accuracy_rec <- confusionMatrix(prediction_rec, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy_rec)
accuracy_don <- confusionMatrix(prediction_don, as.factor(donor$AccidentType))$overall["Accuracy"]
print(accuracy_don)

recipient_xgb <- mutate(recipient, AccidentType_pred_rec = prediction_rec)
donor_xgb <- mutate(donor, AccidentType_pred_don = prediction_don)


xgb_time <- Sys.time()
print(xgb_time-rf_time)

### 15.3.nnet

control <- trainControl(method = "none", search = "grid")

tunegrid <- expand.grid(size = 10, decay = 0.2)

model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "nnet",
               tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction_rec <- predict(model, recipient)
prediction_don <- predict(model, donor)

accuracy_rec <- confusionMatrix(prediction_rec, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy_rec)
accuracy_don <- confusionMatrix(prediction_don, as.factor(donor$AccidentType))$overall["Accuracy"]
print(accuracy_don)

recipient_nnet <- mutate(recipient, AccidentType_pred_rec = prediction_rec)
donor_nnet <- mutate(donor, AccidentType_pred_don = prediction_don)

nnet_time <- Sys.time()
print(nnet_time-xgb_time)

### 15.4.svm

control <- trainControl(method = "none", search = "grid")

tunegrid <- expand.grid(sigma = 0.009, C = 3.5)
model <- train(as.factor(AccidentType) ~ .,
               data=donor,
               method = "svmRadial",
               tuneGrid = tunegrid, trControl = control)

recipient$AccidentType <- as.factor(recipient$AccidentType)
prediction_rec <- predict(model, recipient)
prediction_don <- predict(model, donor)

accuracy_rec <- confusionMatrix(prediction_rec, as.factor(recipient$AccidentType))$overall["Accuracy"]
print(accuracy_rec)
accuracy_don <- confusionMatrix(prediction_don, as.factor(donor$AccidentType))$overall["Accuracy"]
print(accuracy_don)

recipient_svm <- mutate(recipient, AccidentType_pred_rec = prediction_rec)
donor_svm <- mutate(donor, AccidentType_pred_don = prediction_don)

svm_time <- Sys.time()
print(svm_time-nnet_time)

#Zusammenfassen der Vorhersagen der ML-Modelle in den recipient und den donor, entfernen des AccidentType im recipient, damit dieser vorhergesagt werden kann

recipient <- select(recipient, -AccidentType)
recipient <- mutate(recipient, AccidentType_rf = recipient_rf$AccidentType_pred_rec, AccidentType_xgb = recipient_xgb$AccidentType_pred_rec, AccidentType_nnet = recipient_nnet$AccidentType_pred_rec, AccidentType_svm = recipient_svm$AccidentType_pred_rec)
donor <- mutate(donor, AccidentType_rf = donor_rf$AccidentType_pred_don, AccidentType_xgb = donor_xgb$AccidentType_pred_don, AccidentType_nnet = donor_nnet$AccidentType_pred_don, AccidentType_svm = donor_svm$AccidentType_pred_don)

gc()
#hier 33 originale Variablen
X.mtc <- c(  
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
  "Speeding",
  "AccidentType_rf",
  "AccidentType_xgb",
  "AccidentType_nnet",
  "AccidentType_svm"
)

### 15.5 Hotdeck

out.dhd <- NND.hotdeck(data.rec = recipient, data.don = donor, match.vars = X.mtc, dist.fun = "gower", constrained = TRUE)
#Ergebnis ohne Spenderklassen, mit MV über RandomForests

#Erstellen des Fusionsdatensatzes mit richtigem und vorhergesagtem AccidentType sowie Aufprall_Hindernis
recipient <- create.fused(data.rec = recipient, data.don = donor, mtc.ids = out.dhd$mtc.ids, z.vars = "AccidentType")
#Wiedereinfügen der spezifischen Hilfsvariable Aufprall_Hindernis
#vorhergesagter AccidentType wird zu "AccidentType_pred", der alte wieder zu AccidentType
recipient <- rename(recipient, AccidentType_pred = AccidentType)
recipient <- mutate(recipient, AccidentType = AccidentType_rec$AccidentType)

# 15.6.1 Accuracy Auswertung

anzGleich <- 0
anzUngleich <- 0

for (i in 1:nrow(recipient)) {
  
  if (recipient$AccidentType[i]		==	recipient$AccidentType_pred[i])
    anzGleich = anzGleich + 1
  else {
    anzUngleich = anzUngleich + 1
  }
}
accuracy <- anzGleich/nrow(recipient)
print(accuracy)

dhd_time <- Sys.time()
print(dhd_time-svm_time)


# 14.6.2 Erstellung Variablen für die Verteilungsauswertung

#leere Vektoren fÃ¼r for Schleife

#Level 1
vec.accuracy         <- vector()

#Level2
comp.lv2.XZ1.meas         <- vector()
comp.lv2.XZ2.meas           <- vector()
comp.lv2.XZ3.meas         <- vector()
comp.lv2.gesamt.XZ1.meas         <- vector()
comp.lv2.gesamt.XZ2.meas           <- vector()
comp.lv2.gesamt.XZ3.meas         <- vector()
comp.lv2.z.meas         <- vector()

comp.lv2.XZ1.chi         <- vector()
comp.lv2.XZ2.chi           <- vector()
comp.lv2.XZ3.chi         <- vector()
comp.lv2.gesamt.XZ1.chi         <- vector()
comp.lv2.gesamt.XZ2.chi           <- vector()
comp.lv2.gesamt.XZ3.chi         <- vector()
comp.lv2.z.chi         <- vector()

comp.lv2.XZ1.orig.meas <- vector()
comp.lv2.XZ2.orig.meas <- vector()
comp.lv2.XZ3.orig.meas <- vector()
comp.lv2.XZ1.orig.chi <- vector()
comp.lv2.XZ2.orig.chi <- vector()
comp.lv2.XZ3.orig.chi <- vector()

#Level 4

comp.lv4.xz1.meas <- vector()
comp.lv4.xz1.chi <- vector()
comp.lv4.xz2.meas <- vector()
comp.lv4.xz2.chi <- vector()
comp.lv4.xz3.meas <- vector()
comp.lv4.xz3.chi <- vector()

comp.lv4.z.meas <- vector()
comp.lv4.z.chi <- vector()

comp.lv4.zx1.meas <- vector()
comp.lv4.zx1.chi <- vector()
comp.lv4.zx2.meas <- vector()
comp.lv4.zx2.chi <- vector()
comp.lv4.zx3.meas <- vector()
comp.lv4.zx3.chi <- vector()
comp.lv4.zx4.meas <- vector()
comp.lv4.zx4.chi <- vector()
comp.lv4.zx5.meas <- vector()
comp.lv4.zx5.chi <- vector()
comp.lv4.zx6.meas <- vector()
comp.lv4.zx6.chi <- vector()
comp.lv4.zx7.meas <- vector()
comp.lv4.zx7.chi <- vector()
comp.lv4.zx8.meas <- vector()
comp.lv4.zx8.chi <- vector()
comp.lv4.zx9.meas <- vector()
comp.lv4.zx9.chi <- vector()
comp.lv4.zx10.meas <- vector()
comp.lv4.zx10.chi <- vector()
comp.lv4.zx11.meas <- vector()
comp.lv4.zx11.chi <- vector()
comp.lv4.zx12.meas <- vector()
comp.lv4.zx12.chi <- vector()
comp.lv4.zx13.meas <- vector()
comp.lv4.zx13.chi <- vector()
comp.lv4.zx14.meas <- vector()
comp.lv4.zx14.chi <- vector()
comp.lv4.zx15.meas <- vector()
comp.lv4.zx15.chi <- vector()
comp.lv4.zx16.meas <- vector()
comp.lv4.zx16.chi <- vector()
comp.lv4.zx17.meas <- vector()
comp.lv4.zx17.chi <- vector()
comp.lv4.zx18.meas <- vector()
comp.lv4.zx18.chi <- vector()
comp.lv4.zx19.meas <- vector()
comp.lv4.zx19.chi <- vector()
comp.lv4.zx20.meas <- vector()
comp.lv4.zx20.chi <- vector()
comp.lv4.zx21.meas <- vector()
comp.lv4.zx21.chi <- vector()
comp.lv4.zx22.meas <- vector()
comp.lv4.zx22.chi <- vector()
comp.lv4.zx23.meas <- vector()
comp.lv4.zx23.chi <- vector()
comp.lv4.zx24.meas <- vector()
comp.lv4.zx24.chi <- vector()
comp.lv4.zx25.meas <- vector()
comp.lv4.zx25.chi <- vector()


# 15.6.3 Erstellung der decoded-Variablen zur Auswertung der bedingten Verteilungen

#bestehende Variablen, die nicht oder nicht sinnvoll dekodierbar sind und in binärer Form ausgewertet werden:
# Urban, Fahrtrichtung_angegeben, Slippery, Crosswalk, Darkness, Kind, schwerverletzt_1, Runaway, TrafficLights, ObstacleOffRoad, Bicycle, Bus, Car, Truck, Tram, Pedestrian, Motorcycle, Car
# insgesamt 18

#Variablen, die ohne Überschneidung dekodiert werden können:
donor$Uhrzeit <- with(donor, ifelse(abdreizehnuhrdreissig == "1", "abdreizehnuhrdreissig", ifelse(abfuenfuhrdreissig == "1", "abfuenfuhrdreissig", ifelse(absechsuhrdreissig == "1", "absechsuhrdreissig", ifelse(abnulluhr == "1", "abnulluhr", "andere")))))
recipient$Uhrzeit <- with(recipient, ifelse(abdreizehnuhrdreissig == "1", "abdreizehnuhrdreissig", ifelse(abfuenfuhrdreissig == "1", "abfuenfuhrdreissig", ifelse(absechsuhrdreissig == "1", "absechsuhrdreissig", ifelse(abnulluhr == "1", "abnulluhr", "andere")))))

donor$leichtverletzt <- with(donor, ifelse(leichtverletzt_1 == "1", "leichtverletzt_1", ifelse(leichtverletzt_2bis4 == "1", "leichtverletzt_2bis4", "andere")))
recipient$leichtverletzt <- with(recipient, ifelse(leichtverletzt_1 == "1", "leichtverletzt_1", ifelse(leichtverletzt_2bis4 == "1", "leichtverletzt_2bis4", "andere")))

donor$unfallart <- with(donor, ifelse(unfallart_7 == "1", "unfallart_7", ifelse(unfallart_9 == "1", "unfallart_9", ifelse(unfallart_6 == "1", "unfallart_6", ifelse(unfallart_8 == "1", "unfallart_8", ifelse(unfallart_2 == "1", "unfallart_2", ifelse(unfallart_1 == "1", "unfallart_1", ifelse(unfallart_4 == "1", "unfallart_4", ifelse(unfallart_3 == "1", "unfallart_3", ifelse(unfallart_5 == "1", "unfallart_5", "andere"))))))))))
recipient$unfallart <- with(recipient, ifelse(unfallart_7 == "1", "unfallart_7", ifelse(unfallart_9 == "1", "unfallart_9", ifelse(unfallart_6 == "1", "unfallart_6", ifelse(unfallart_8 == "1", "unfallart_8", ifelse(unfallart_2 == "1", "unfallart_2", ifelse(unfallart_1 == "1", "unfallart_1", ifelse(unfallart_4 == "1", "unfallart_4", ifelse(unfallart_3 == "1", "unfallart_3", ifelse(unfallart_5 == "1", "unfallart_5", "andere"))))))))))

donor$beteiligte <- with(donor, ifelse(beteiligte_1 == "1", "beteiligte_1", ifelse(beteiligte_3bis5 == "1", "beteiligte_3bis5", "andere")))
recipient$beteiligte <- with(recipient, ifelse(beteiligte_1 == "1", "beteiligte_1", ifelse(beteiligte_3bis5 == "1", "beteiligte_3bis5", "andere")))

donor$sachschaden <- with(donor, ifelse(bis5000euro == "1", "bis5000euro", ifelse(bis3000euro == "1", "bis3000euro", ifelse(bis8000euro == "1", "bis8000euro", ifelse(bis16000euro == "1", "bis16000euro", ifelse(bis1000euro == "1", "bis1000euro", "andere"))))))
recipient$sachschaden <- with(recipient, ifelse(bis5000euro == "1", "bis5000euro", ifelse(bis3000euro == "1", "bis3000euro", ifelse(bis8000euro == "1", "bis8000euro", ifelse(bis16000euro == "1", "bis16000euro", ifelse(bis1000euro == "1", "bis1000euro", "andere"))))))

donor$strassenart <- with(donor, ifelse(autobahn == "1", "autobahn", ifelse(kreisstrasse == "1", "kreisstrasse", ifelse(landesstrasse == "1", "landesstrasse", ifelse(bundesstrasse == "1", "bundesstrasse", "andere")))))
recipient$strassenart <- with(recipient, ifelse(autobahn == "1", "autobahn", ifelse(kreisstrasse == "1", "kreisstrasse", ifelse(landesstrasse == "1", "landesstrasse", ifelse(bundesstrasse == "1", "bundesstrasse", "andere")))))

#Variablen, die nur mit Überschneidung dekodiert werden können:
#Charakt. Unfallstelle und Unfallursache
#Kategorien des Attributes werden bei doppelten oder max. dreifachen Einträgen zufällig ausgewählt , um die Verteilung nicht zu beeinträchtigen. Anzahl der Instanzen mit mehr als einer Kategorie <10%

donor$char_unfallstelle <- with(donor, ifelse(Driveway == "0" & Curve == "0" & Slope == "0" & Roundabout == "0" & Intersection == "0", "andere", "0"))  
recipient$char_unfallstelle <- with(recipient, ifelse(Driveway == "0" & Curve == "0" & Slope == "0" & Roundabout == "0" & Intersection == "0", "andere", "0"))

for (i in 1:nrow(donor)) {
  if(as.numeric(donor$Driveway[i]) + as.numeric(donor$Curve[i]) + as.numeric(donor$Slope[i]) + as.numeric(donor$Roundabout[i])+as.numeric(donor$Intersection[i]) == 1){
    donor$char_unfallstelle[i] <- with(donor, ifelse(Driveway[i] == "1", "Driveway", ifelse(Curve[i] == "1", "Curve", ifelse(Slope[i] == "1", "Slope", ifelse(Roundabout[i] == "1", "Roundabout", "Intersection")))))
  }
  if(as.numeric(donor$Driveway[i]) + as.numeric(donor$Curve[i]) + as.numeric(donor$Slope[i]) + as.numeric(donor$Roundabout[i])+as.numeric(donor$Intersection[i]) >= 2){
    c <- vector(mode = "character")
    if(donor$Driveway[i] == "1"){
      c <- c(c,"Driveway")
    }
    if(donor$Curve[i] == "1"){
      c <- c(c,"Curve")
    }
    if(donor$Slope[i] == "1"){
      c <- c(c,"Slope")
    }
    if(donor$Roundabout[i] == "1"){
      c <- c(c,"Roundabout")
    }
    if(donor$Intersection[i] == "1"){
      c <- c(c,"Intersection")
    }
    donor$char_unfallstelle[i] <- sample(c, size=1)
  }
}

for (i in 1:nrow(recipient)) {
  if(as.numeric(recipient$Driveway[i]) + as.numeric(recipient$Curve[i]) + as.numeric(recipient$Slope[i]) + as.numeric(recipient$Roundabout[i])+as.numeric(recipient$Intersection[i]) == 1){
    recipient$char_unfallstelle[i] <- with(recipient, ifelse(Driveway[i] == "1", "Driveway", ifelse(Curve[i] == "1", "Curve", ifelse(Slope[i] == "1", "Slope", ifelse(Roundabout[i] == "1", "Roundabout", "Intersection")))))
  }
  if(as.numeric(recipient$Driveway[i]) + as.numeric(recipient$Curve[i]) + as.numeric(recipient$Slope[i]) + as.numeric(recipient$Roundabout[i])+as.numeric(recipient$Intersection[i]) >= 2){
    c <- vector(mode = "character")
    if(recipient$Driveway[i] == "1"){
      c <- c(c,"Driveway")
    }
    if(recipient$Curve[i] == "1"){
      c <- c(c,"Curve")
    }
    if(recipient$Slope[i] == "1"){
      c <- c(c,"Slope")
    }
    if(recipient$Roundabout[i] == "1"){
      c <- c(c,"Roundabout")
    }
    if(recipient$Intersection[i] == "1"){
      c <- c(c,"Intersection")
    }
    recipient$char_unfallstelle[i] <- sample(c, size=1)
  }
}

donor$unfallursache <- with(donor, ifelse(Speeding == "0" & Distance == "0" & Turning == "0" & Priority == "0" & Animal == "0" & Overtaking == "0" & RedLight == "0" & Alcohol == "0" & EnvironmentalReason == "0", "andere", "0"))
recipient$unfallursache <- with(recipient, ifelse(Speeding == "0" & Distance == "0" & Turning == "0" & Priority == "0" & Animal == "0" & Overtaking == "0" & RedLight == "0" & Alcohol == "0" & EnvironmentalReason == "0", "andere", "0"))

for (i in 1:nrow(donor)) {
  if(as.numeric(donor$Speeding[i]) + as.numeric(donor$Distance[i]) + as.numeric(donor$Turning[i]) + as.numeric(donor$Priority[i])+as.numeric(donor$Animal[i])+as.numeric(donor$Overtaking[i])+as.numeric(donor$RedLight[i])+as.numeric(donor$Alcohol[i])+as.numeric(donor$EnvironmentalReason[i]) == 1){
    donor$unfallursache[i] <- with(donor, ifelse(Speeding[i] == "1", "Speeding", ifelse(Distance[i] == "1", "Distance", ifelse(Turning[i] == "1", "Turning", ifelse(Priority[i] == "1", "Priority", ifelse(Animal[i] == "1", "Animal", ifelse(Overtaking[i] == "1", "Overtaking", ifelse(RedLight[i] == "1", "RedLight", ifelse(Alcohol[i] == "1", "Alcohol", "EnvironmentalReason")))))))))
  }
  if(as.numeric(donor$Speeding[i]) + as.numeric(donor$Distance[i]) + as.numeric(donor$Turning[i]) + as.numeric(donor$Priority[i])+as.numeric(donor$Animal[i])+as.numeric(donor$Overtaking[i])+as.numeric(donor$RedLight[i])+as.numeric(donor$Alcohol[i])+as.numeric(donor$EnvironmentalReason[i]) >= 2){
    c <- vector(mode = "character")
    if(donor$Speeding[i] == "1"){
      c(c,"Speeding")
    }
    if(donor$Distance[i] == "1"){
      c <- c(c,"Distance") 
    }
    if(donor$Turning[i] == "1"){
      c <- c(c,"Turning") 
    }
    if(donor$Priority[i] == "1"){
      c <- c(c,"Priority") 
    }
    if(donor$Animal[i] == "1"){
      c <- c(c,"Animal") 
    }
    if(donor$Overtaking[i] == "1"){
      c <- c(c,"Overtaking") 
    }
    if(donor$RedLight[i] == "1"){
      c <- c(c,"RedLight") 
    }
    if(donor$Alcohol[i] == "1"){
      c <- c(c,"Alcohol") 
    }
    if(donor$EnvironmentalReason[i] == "1"){
      c <- c(c,"EnvironmentalReason") 
    }
    donor$unfallursache[i] <- sample(c, size=1)
  }
}

for (i in 1:nrow(recipient)) {
  if(as.numeric(recipient$Speeding[i]) + as.numeric(recipient$Distance[i]) + as.numeric(recipient$Turning[i]) + as.numeric(recipient$Priority[i])+as.numeric(recipient$Animal[i])+as.numeric(recipient$Overtaking[i])+as.numeric(recipient$RedLight[i])+as.numeric(recipient$Alcohol[i])+as.numeric(recipient$EnvironmentalReason[i]) == 1){
    recipient$unfallursache[i] <- with(recipient, ifelse(Speeding[i] == "1", "Speeding", ifelse(Distance[i] == "1", "Distance", ifelse(Turning[i] == "1", "Turning", ifelse(Priority[i] == "1", "Priority", ifelse(Animal[i] == "1", "Animal", ifelse(Overtaking[i] == "1", "Overtaking", ifelse(RedLight[i] == "1", "RedLight", ifelse(Alcohol[i] == "1", "Alcohol", "EnvironmentalReason")))))))))
  }
  if(as.numeric(recipient$Speeding[i]) + as.numeric(recipient$Distance[i]) + as.numeric(recipient$Turning[i]) + as.numeric(recipient$Priority[i])+as.numeric(recipient$Animal[i])+as.numeric(recipient$Overtaking[i])+as.numeric(recipient$RedLight[i])+as.numeric(recipient$Alcohol[i])+as.numeric(recipient$EnvironmentalReason[i]) >= 2){
    c <- vector(mode = "character")
    if(recipient$Speeding[i] == "1"){
      c(c,"Speeding")
    }
    if(recipient$Distance[i] == "1"){
      c <- c(c,"Distance") 
    }
    if(recipient$Turning[i] == "1"){
      c <- c(c,"Turning") 
    }
    if(recipient$Priority[i] == "1"){
      c <- c(c,"Priority") 
    }
    if(recipient$Animal[i] == "1"){
      c <- c(c,"Animal") 
    }
    if(recipient$Overtaking[i] == "1"){
      c <- c(c,"Overtaking") 
    }
    if(recipient$RedLight[i] == "1"){
      c <- c(c,"RedLight") 
    }
    if(recipient$Alcohol[i] == "1"){
      c <- c(c,"Alcohol") 
    }
    if(recipient$EnvironmentalReason[i] == "1"){
      c <- c(c,"EnvironmentalReason") 
    }
    recipient$unfallursache[i] <- sample(c, size=1)
  }
}

# 15.6.4 Erstellung Kontigenztabellen für Ebene 4

spender.tab.z <- xtabs(~AccidentType, data = donor)
z.spender_xz1 <- xtabs(~AccidentType
                       +unfallursache
                       +char_unfallstelle
                       +strassenart
                       +Fahrtrichtung_angegeben
                       +unfallart
                       ,data = donor)
z.spender_xz2 <- xtabs(~AccidentType
                       +Urban
                       +leichtverletzt
                       +Bicycle
                       +Bus
                       +beteiligte
                       +sachschaden
                       +TrafficLights
                       +Runaway
                       +ObstacleOffRoad
                       ,data = donor)
z.spender_xz3 <- xtabs(~AccidentType
                       +Uhrzeit
                       +Slippery
                       +Crosswalk
                       +Truck
                       +Pedestrian
                       +Car
                       +Tram
                       +Motorcycle
                       +Darkness
                       +Kind
                       +schwerverletzt_1
                       ,data = donor)#Referenz Verteilung
spender.tab.zx1 <- xtabs(~AccidentType+unfallursache, data = donor)
spender.tab.zx2 <- xtabs(~AccidentType+char_unfallstelle, data = donor)
spender.tab.zx3 <- xtabs(~AccidentType+strassenart, data = donor)
spender.tab.zx4 <- xtabs(~AccidentType+Fahrtrichtung_angegeben, data = donor)
spender.tab.zx5 <- xtabs(~AccidentType+unfallart, data = donor)
spender.tab.zx6 <- xtabs(~AccidentType+Urban, data = donor)
spender.tab.zx7 <- xtabs(~AccidentType+leichtverletzt, data = donor)
spender.tab.zx8 <- xtabs(~AccidentType+Bicycle, data = donor)
spender.tab.zx9 <- xtabs(~AccidentType+Bus, data = donor)
spender.tab.zx10 <- xtabs(~AccidentType+beteiligte, data = donor)
spender.tab.zx11 <- xtabs(~AccidentType+sachschaden, data = donor)
spender.tab.zx12 <- xtabs(~AccidentType+TrafficLights, data = donor)
spender.tab.zx13 <- xtabs(~AccidentType+Runaway, data = donor)
spender.tab.zx14 <- xtabs(~AccidentType+ObstacleOffRoad, data = donor)
spender.tab.zx15 <- xtabs(~AccidentType+Uhrzeit, data = donor)
spender.tab.zx16 <- xtabs(~AccidentType+Slippery, data = donor)
spender.tab.zx17 <- xtabs(~AccidentType+Crosswalk, data = donor)
spender.tab.zx18 <- xtabs(~AccidentType+Truck, data = donor)
spender.tab.zx19 <- xtabs(~AccidentType+Pedestrian, data = donor)
spender.tab.zx20 <- xtabs(~AccidentType+Car, data = donor)
spender.tab.zx21 <- xtabs(~AccidentType+Tram, data = donor)
spender.tab.zx22 <- xtabs(~AccidentType+Motorcycle, data = donor)
spender.tab.zx23 <- xtabs(~AccidentType+Darkness, data = donor)
spender.tab.zx24 <- xtabs(~AccidentType+Kind, data = donor)
spender.tab.zx25 <- xtabs(~AccidentType+schwerverletzt_1, data = donor)


#gesamter originaler Datensatz
orig_recipient <- select(recipient, -AccidentType_pred)
d.gesamt.data = rbind(orig_recipient, donor)

# 15.7.1 Auswertung der gemeinsamen Verteilung (Ebene 2)

#Vergleich der Gesamtverteilung von fXZ, Vorhergesagte Variable im recipient vs. tatsächliche Variable im recipient

# Kontingenztabellen fÃ¼r beide Verteilungen 
synth.tab.xz1 <-  xtabs(~AccidentType_pred
                        +unfallursache
                        +char_unfallstelle
                        +strassenart
                        +Fahrtrichtung_angegeben
                        +unfallart
                        ,data = recipient)
synth.tab.xz2 <-  xtabs(~AccidentType_pred
                        +Urban
                        +leichtverletzt
                        +Bicycle
                        +Bus
                        +beteiligte
                        +sachschaden
                        +TrafficLights
                        +Runaway
                        +ObstacleOffRoad
                        ,data = recipient)
synth.tab.xz3 <-  xtabs(~AccidentType_pred
                        +Uhrzeit
                        +Slippery
                        +Crosswalk
                        +Truck
                        +Pedestrian
                        +Car
                        +Tram
                        +Motorcycle
                        +Darkness
                        +Kind
                        +schwerverletzt_1
                        ,data = recipient)
orig.tab.xz1 <-  xtabs(~AccidentType
                       +unfallursache
                       +char_unfallstelle
                       +strassenart
                       +Fahrtrichtung_angegeben
                       +unfallart
                       ,data = recipient)
orig.tab.xz2 <-  xtabs(~AccidentType
                       +Urban
                       +leichtverletzt
                       +Bicycle
                       +Bus
                       +beteiligte
                       +sachschaden
                       +TrafficLights
                       +Runaway
                       +ObstacleOffRoad
                       ,data = recipient)
orig.tab.xz3 <-  xtabs(~AccidentType
                       +Uhrzeit
                       +Slippery
                       +Crosswalk
                       +Truck
                       +Pedestrian
                       +Car
                       +Tram
                       +Motorcycle
                       +Darkness
                       +Kind
                       +schwerverletzt_1
                       ,data = recipient)

#Vergleich der Verteilungen mit der Funktion von D'Orazio
lv2.xz1 <- comp.prop(p1 = synth.tab.xz1, p2 = orig.tab.xz1, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.xz2 <- comp.prop(p1 = synth.tab.xz2, p2 = orig.tab.xz2, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.xz3 <- comp.prop(p1 = synth.tab.xz3, p2 = orig.tab.xz3, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# Vergleich der Gesamtverteilung von fXZ, Vorhergesagte Variable im recipient vs. tatsächliche Variable im Gesamtdatensatz

# fÃ¼r die Kontingenztabelle der Verteilung mit der fusionierten Variable im fuisonierten Datensatz wird die synth.tab.xyz verwendet

#Kontingenztabelle fÃ¼r Verteilung mit originaler Variable uart im zusammengefÃ¼gten Datensatz 
gesamt.tab.xz1 <- xtabs(~AccidentType
                        +unfallursache
                        +char_unfallstelle
                        +strassenart
                        +Fahrtrichtung_angegeben
                        +unfallart
                        , data = d.gesamt.data)
gesamt.tab.xz2 <- xtabs(~AccidentType
                        +Urban
                        +leichtverletzt
                        +Bicycle
                        +Bus
                        +beteiligte
                        +sachschaden
                        +TrafficLights
                        +Runaway
                        +ObstacleOffRoad
                        , data = d.gesamt.data)
gesamt.tab.xz3 <- xtabs(~AccidentType
                        +Uhrzeit
                        +Slippery
                        +Crosswalk
                        +Truck
                        +Pedestrian
                        +Car
                        +Tram
                        +Motorcycle
                        +Darkness
                        +Kind
                        +schwerverletzt_1
                        , data = d.gesamt.data)

#Vergleich der Verteilungen mit der Funktion von D'Orazio
lv2.gesamt.xz1 <- comp.prop(p1 = synth.tab.xz1, p2 = gesamt.tab.xz1, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.gesamt.xz2 <- comp.prop(p1 = synth.tab.xz2, p2 = gesamt.tab.xz2, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.gesamt.xz3 <- comp.prop(p1 = synth.tab.xz3, p2 = gesamt.tab.xz3, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# Vergleich der Verteilung von fz

#Kontingenztabelle fÃ¼r beide Verteilungen
synth.tab.z <-  xtabs(~AccidentType_pred, data = recipient)
orig.tab.z <-  xtabs(~AccidentType, data = recipient)

#Vergleich der Verteilungen mit der Funktion von D'Orazio
lv2.z <- comp.prop(p1 = synth.tab.z, p2 = orig.tab.z, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# ergleich der tatsächlichen Gesamtverteilungen von donor und recipient als Maßstab der Ungebnauigkeit des Datensplits bzw. Randomness der HD

lv2.xz1.orig <- comp.prop(p1 = z.spender_xz1, p2 = orig.tab.xz1, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.xz2.orig <- comp.prop(p1 = z.spender_xz2, p2 = orig.tab.xz2, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv2.xz3.orig <- comp.prop(p1 = z.spender_xz3, p2 = orig.tab.xz3, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# 15.7.2 Auswertung der Randverteilung (Ebene 4)

#Vergleich der Gesamtverteilung von fXZ vom fusionierten Datensatz mit dem Spenderdatensatz --> real testbar

#Tabellen fÃ¼r Spender siehe oben 
#Tabellen für recipient siehe Level2
#Kontingentabelle fusionierte Datei

#Vergleich der Randverteilung
lv4.xz1 <- comp.prop(p1 = synth.tab.xz1, p2 = z.spender_xz1, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.xz2 <- comp.prop(p1 = synth.tab.xz2, p2 = z.spender_xz2, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.xz3 <- comp.prop(p1 = synth.tab.xz3, p2 = z.spender_xz3, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

#Vergleich der Randverteilung von Z (AccidentType) --> imputierte Variable

#Tabellen fÃ¼r Spender siehe oben 
#Tabellen für recipient siehe Level2
#Kontingentabelle fusionierte Datei

#Vergleich der Randverteilung
lv4.z <- comp.prop(p1 = synth.tab.z, p2 = spender.tab.z, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# Vergleich der Verteilung von fXZ --> jeweils fÃ¼r jede XZ Kombination 

#Kontingentabellen fÃ¼r Verteilungen mit imputierter Variable 

synth.tab.zx1 <- xtabs(~AccidentType_pred+unfallursache, data = recipient)
synth.tab.zx2 <- xtabs(~AccidentType_pred+char_unfallstelle, data = recipient)
synth.tab.zx3 <- xtabs(~AccidentType_pred+strassenart, data = recipient)
synth.tab.zx4 <- xtabs(~AccidentType_pred+Fahrtrichtung_angegeben, data = recipient)
synth.tab.zx5 <- xtabs(~AccidentType_pred+unfallart, data = recipient)
synth.tab.zx6 <- xtabs(~AccidentType_pred+Urban, data = recipient)
synth.tab.zx7 <- xtabs(~AccidentType_pred+leichtverletzt, data = recipient)
synth.tab.zx8 <- xtabs(~AccidentType_pred+Bicycle, data = recipient)
synth.tab.zx9 <- xtabs(~AccidentType_pred+Bus, data = recipient)
synth.tab.zx10 <- xtabs(~AccidentType_pred+beteiligte, data = recipient)
synth.tab.zx11 <- xtabs(~AccidentType_pred+sachschaden, data = recipient)
synth.tab.zx12 <- xtabs(~AccidentType_pred+TrafficLights, data = recipient)
synth.tab.zx13 <- xtabs(~AccidentType_pred+Runaway, data = recipient)
synth.tab.zx14 <- xtabs(~AccidentType_pred+ObstacleOffRoad, data = recipient)
synth.tab.zx15 <- xtabs(~AccidentType_pred+Uhrzeit, data = recipient)
synth.tab.zx16 <- xtabs(~AccidentType_pred+Slippery, data = recipient)
synth.tab.zx17 <- xtabs(~AccidentType_pred+Crosswalk, data = recipient)
synth.tab.zx18 <- xtabs(~AccidentType_pred+Truck, data = recipient)
synth.tab.zx19 <- xtabs(~AccidentType_pred+Pedestrian, data = recipient)
synth.tab.zx20 <- xtabs(~AccidentType_pred+Car, data = recipient)
synth.tab.zx21 <- xtabs(~AccidentType_pred+Tram, data = recipient)
synth.tab.zx22 <- xtabs(~AccidentType_pred+Motorcycle, data = recipient)
synth.tab.zx23 <- xtabs(~AccidentType_pred+Darkness, data = recipient)
synth.tab.zx24 <- xtabs(~AccidentType_pred+Kind, data = recipient)
synth.tab.zx25 <- xtabs(~AccidentType_pred+schwerverletzt_1, data = recipient)


#Vergleich der Verteilung mit der Funktion von D'Orazio
# Aufstellung der Kontingenztabelle im Spenderdatensatz unter Kapitel 2
lv4.zx1 <- comp.prop(p1 = synth.tab.zx1, p2 = spender.tab.zx1, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx2 <- comp.prop(p1 = synth.tab.zx2, p2 = spender.tab.zx2, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx3 <- comp.prop(p1 = synth.tab.zx3, p2 = spender.tab.zx3, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx4 <- comp.prop(p1 = synth.tab.zx4, p2 = spender.tab.zx4, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx5 <- comp.prop(p1 = synth.tab.zx5, p2 = spender.tab.zx5, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx6 <- comp.prop(p1 = synth.tab.zx6, p2 = spender.tab.zx6, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx7 <- comp.prop(p1 = synth.tab.zx7, p2 = spender.tab.zx7, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx8 <- comp.prop(p1 = synth.tab.zx8, p2 = spender.tab.zx8, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx9 <- comp.prop(p1 = synth.tab.zx9, p2 = spender.tab.zx9, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx10 <- comp.prop(p1 = synth.tab.zx10, p2 = spender.tab.zx10, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx11 <- comp.prop(p1 = synth.tab.zx11, p2 = spender.tab.zx11, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx12 <- comp.prop(p1 = synth.tab.zx12, p2 = spender.tab.zx12, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx13 <- comp.prop(p1 = synth.tab.zx13, p2 = spender.tab.zx13, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx14 <- comp.prop(p1 = synth.tab.zx14, p2 = spender.tab.zx14, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx15 <- comp.prop(p1 = synth.tab.zx15, p2 = spender.tab.zx15, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx16 <- comp.prop(p1 = synth.tab.zx16, p2 = spender.tab.zx16, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx17 <- comp.prop(p1 = synth.tab.zx17, p2 = spender.tab.zx17, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx18 <- comp.prop(p1 = synth.tab.zx18, p2 = spender.tab.zx18, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx19 <- comp.prop(p1 = synth.tab.zx19, p2 = spender.tab.zx19, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx20 <- comp.prop(p1 = synth.tab.zx20, p2 = spender.tab.zx20, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx21 <- comp.prop(p1 = synth.tab.zx21, p2 = spender.tab.zx21, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx22 <- comp.prop(p1 = synth.tab.zx22, p2 = spender.tab.zx22, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx23 <- comp.prop(p1 = synth.tab.zx23, p2 = spender.tab.zx23, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx24 <- comp.prop(p1 = synth.tab.zx24, p2 = spender.tab.zx24, n1 = nrow(recipient), n2 = NULL, ref = TRUE)
lv4.zx25 <- comp.prop(p1 = synth.tab.zx25, p2 = spender.tab.zx25, n1 = nrow(recipient), n2 = NULL, ref = TRUE)

# 15.8 Speichern der multiplen Durchläufe als Ergebnis in Vektoren und Ausgabe

# Erhalt der einzelnen Werte 

vec.accuracy <- c(vec.accuracy,accuracy)

# Erhalt der gemeinsamen Verteilung

comp.lv2.XZ1.meas <- rbind(comp.lv2.XZ1.meas,lv2.xz1$meas)
comp.lv2.XZ1.chi <- rbind(comp.lv2.XZ1.chi,lv2.xz1$chi.sq )

comp.lv2.XZ2.meas <- rbind(comp.lv2.XZ2.meas,lv2.xz2$meas)
comp.lv2.XZ2.chi <- rbind(comp.lv2.XZ2.chi,lv2.xz2$chi.sq )

comp.lv2.XZ3.meas <- rbind(comp.lv2.XZ3.meas,lv2.xz3$meas)
comp.lv2.XZ3.chi <- rbind(comp.lv2.XZ3.chi,lv2.xz3$chi.sq )



comp.lv2.gesamt.XZ1.meas <- rbind(comp.lv2.gesamt.XZ1.meas,lv2.gesamt.xz1$meas)
comp.lv2.gesamt.XZ1.chi <- rbind(comp.lv2.gesamt.XZ1.chi,lv2.gesamt.xz1$chi.sq)

comp.lv2.gesamt.XZ2.meas <- rbind(comp.lv2.gesamt.XZ2.meas,lv2.gesamt.xz2$meas)
comp.lv2.gesamt.XZ2.chi <- rbind(comp.lv2.gesamt.XZ2.chi,lv2.gesamt.xz2$chi.sq)

comp.lv2.gesamt.XZ3.meas <- rbind(comp.lv2.gesamt.XZ3.meas,lv2.gesamt.xz3$meas)
comp.lv2.gesamt.XZ3.chi <- rbind(comp.lv2.gesamt.XZ3.chi,lv2.gesamt.xz3$chi.sq)



comp.lv2.z.meas <- rbind(comp.lv2.z.meas,lv2.z$meas)
comp.lv2.z.chi <- rbind(comp.lv2.z.chi,lv2.z$chi.sq )



comp.lv2.XZ1.orig.meas <- rbind(comp.lv2.XZ1.orig.meas,lv2.xz1.orig$meas)
comp.lv2.XZ1.orig.chi <- rbind(comp.lv2.XZ1.orig.chi,lv2.xz1.orig$chi.sq )

comp.lv2.XZ2.orig.meas <- rbind(comp.lv2.XZ2.orig.meas,lv2.xz2.orig$meas)
comp.lv2.XZ2.orig.chi <- rbind(comp.lv2.XZ2.orig.chi,lv2.xz2.orig$chi.sq )

comp.lv2.XZ3.orig.meas <- rbind(comp.lv2.XZ3.orig.meas,lv2.xz3.orig$meas)
comp.lv2.XZ3.orig.chi <- rbind(comp.lv2.XZ3.orig.chi,lv2.xz3.orig$chi.sq )


# Erhalt der Randverteilung

comp.lv4.xz1.meas <- rbind(comp.lv4.xz1.meas, lv4.xz1$meas)
comp.lv4.xz1.chi <- rbind(comp.lv4.xz1.chi, lv4.xz1$chi.sq)

comp.lv4.xz2.meas <- rbind(comp.lv4.xz2.meas, lv4.xz2$meas)
comp.lv4.xz2.chi <- rbind(comp.lv4.xz2.chi, lv4.xz2$chi.sq)

comp.lv4.xz3.meas <- rbind(comp.lv4.xz3.meas, lv4.xz3$meas)
comp.lv4.xz3.chi <- rbind(comp.lv4.xz3.chi, lv4.xz3$chi.sq)



comp.lv4.z.meas <- rbind(comp.lv4.z.meas, lv4.z$meas)
comp.lv4.z.chi <- rbind(comp.lv4.z.chi, lv4.z$chi.sq)



comp.lv4.zx1.meas <- rbind(comp.lv4.zx1.meas, lv4.zx1$meas)
comp.lv4.zx1.chi <- rbind(comp.lv4.zx1.chi, lv4.zx1$chi.sq)

comp.lv4.zx2.meas <- rbind(comp.lv4.zx2.meas, lv4.zx2$meas)
comp.lv4.zx2.chi <- rbind(comp.lv4.zx2.chi, lv4.zx2$chi.sq)

comp.lv4.zx3.meas <- rbind(comp.lv4.zx3.meas, lv4.zx3$meas)
comp.lv4.zx3.chi <- rbind(comp.lv4.zx3.chi, lv4.zx3$chi.sq)

comp.lv4.zx4.meas <- rbind(comp.lv4.zx4.meas, lv4.zx4$meas)
comp.lv4.zx4.chi <- rbind(comp.lv4.zx4.chi, lv4.zx4$chi.sq)

comp.lv4.zx5.meas <- rbind(comp.lv4.zx5.meas, lv4.zx5$meas)
comp.lv4.zx5.chi <- rbind(comp.lv4.zx5.chi, lv4.zx5$chi.sq)

comp.lv4.zx6.meas <- rbind(comp.lv4.zx6.meas, lv4.zx6$meas)
comp.lv4.zx6.chi <- rbind(comp.lv4.zx6.chi, lv4.zx6$chi.sq)

comp.lv4.zx7.meas <- rbind(comp.lv4.zx7.meas, lv4.zx7$meas)
comp.lv4.zx7.chi <- rbind(comp.lv4.zx7.chi, lv4.zx7$chi.sq)

comp.lv4.zx8.meas <- rbind(comp.lv4.zx8.meas, lv4.zx8$meas)
comp.lv4.zx8.chi <- rbind(comp.lv4.zx8.chi, lv4.zx8$chi.sq)

comp.lv4.zx9.meas <- rbind(comp.lv4.zx9.meas, lv4.zx9$meas)
comp.lv4.zx9.chi <- rbind(comp.lv4.zx9.chi, lv4.zx9$chi.sq)

comp.lv4.zx10.meas <- rbind(comp.lv4.zx10.meas, lv4.zx10$meas)
comp.lv4.zx10.chi <- rbind(comp.lv4.zx10.chi, lv4.zx10$chi.sq)

comp.lv4.zx11.meas <- rbind(comp.lv4.zx11.meas, lv4.zx11$meas)
comp.lv4.zx11.chi <- rbind(comp.lv4.zx11.chi, lv4.zx11$chi.sq)

comp.lv4.zx12.meas <- rbind(comp.lv4.zx12.meas, lv4.zx12$meas)
comp.lv4.zx12.chi <- rbind(comp.lv4.zx12.chi, lv4.zx12$chi.sq)

comp.lv4.zx13.meas <- rbind(comp.lv4.zx13.meas, lv4.zx13$meas)
comp.lv4.zx13.chi <- rbind(comp.lv4.zx13.chi, lv4.zx13$chi.sq)

comp.lv4.zx14.meas <- rbind(comp.lv4.zx14.meas, lv4.zx14$meas)
comp.lv4.zx14.chi <- rbind(comp.lv4.zx14.chi, lv4.zx14$chi.sq)

comp.lv4.zx15.meas <- rbind(comp.lv4.zx15.meas, lv4.zx15$meas)
comp.lv4.zx15.chi <- rbind(comp.lv4.zx15.chi, lv4.zx15$chi.sq)

comp.lv4.zx16.meas <- rbind(comp.lv4.zx16.meas, lv4.zx16$meas)
comp.lv4.zx16.chi <- rbind(comp.lv4.zx16.chi, lv4.zx16$chi.sq)

comp.lv4.zx17.meas <- rbind(comp.lv4.zx17.meas, lv4.zx17$meas)
comp.lv4.zx17.chi <- rbind(comp.lv4.zx17.chi, lv4.zx17$chi.sq)

comp.lv4.zx18.meas <- rbind(comp.lv4.zx18.meas, lv4.zx18$meas)
comp.lv4.zx18.chi <- rbind(comp.lv4.zx18.chi, lv4.zx18$chi.sq)

comp.lv4.zx19.meas <- rbind(comp.lv4.zx19.meas, lv4.zx19$meas)
comp.lv4.zx19.chi <- rbind(comp.lv4.zx19.chi, lv4.zx19$chi.sq)

comp.lv4.zx20.meas <- rbind(comp.lv4.zx20.meas, lv4.zx20$meas)
comp.lv4.zx20.chi <- rbind(comp.lv4.zx20.chi, lv4.zx20$chi.sq)

comp.lv4.zx21.meas <- rbind(comp.lv4.zx21.meas, lv4.zx21$meas)
comp.lv4.zx21.chi <- rbind(comp.lv4.zx21.chi, lv4.zx21$chi.sq)

comp.lv4.zx22.meas <- rbind(comp.lv4.zx22.meas, lv4.zx22$meas)
comp.lv4.zx22.chi <- rbind(comp.lv4.zx22.chi, lv4.zx22$chi.sq)

comp.lv4.zx23.meas <- rbind(comp.lv4.zx23.meas, lv4.zx23$meas)
comp.lv4.zx23.chi <- rbind(comp.lv4.zx23.chi, lv4.zx23$chi.sq)

comp.lv4.zx24.meas <- rbind(comp.lv4.zx24.meas, lv4.zx24$meas)
comp.lv4.zx24.chi <- rbind(comp.lv4.zx24.chi, lv4.zx24$chi.sq)

comp.lv4.zx25.meas <- rbind(comp.lv4.zx25.meas, lv4.zx25$meas)
comp.lv4.zx25.chi <- rbind(comp.lv4.zx25.chi, lv4.zx25$chi.sq)


#for-Schleife endet
print("ENDE Simulationsdurchlauf")

# 15.9 Zusammenfassung in Dataframes

#die Fusion wird k mal durchgefÃ¼hrt und dafÃ¼r werden jedes mal verschieden Kennwerte berechnet (s.o)
#Zusammenfassung derErgebnisse in dataframes  

# Ebene 1: gibt die Anzahl an,  Wie oft die Werte der fusionierten Variable mit der vorhandenen Variable im Spenderdatensatz Ã¼berein stimmen?
df.Ebene1 <- data.frame(vec.accuracy)

#Ebene 2:
df.Ebene2 <- data.frame(comp.lv2.XZ1.meas[,c(1,3,4)],comp.lv2.XZ2.meas[,c(1,3,4)],comp.lv2.XZ3.meas[,c(1,3,4)],comp.lv2.XZ1.orig.meas[,c(1,3,4)],comp.lv2.XZ2.orig.meas[,c(1,3,4)],comp.lv2.XZ3.orig.meas[,c(1,3,4)],comp.lv2.gesamt.XZ1.meas[,c(1,3,4)],comp.lv2.gesamt.XZ2.meas[,c(1,3,4)],comp.lv2.gesamt.XZ3.meas[,c(1,3,4)], comp.lv2.z.meas[,c(1,3,4)])

#Ebene 4: 
df.Ebene4 <- data.frame(comp.lv4.z.meas[,c(1,3,4)], comp.lv4.xz1.meas[,c(1,3,4)],comp.lv4.xz2.meas[,c(1,3,4)],comp.lv4.xz3.meas[,c(1,3,4)],comp.lv4.zx1.meas[,c(1,3,4)],comp.lv4.zx2.meas[,c(1,3,4)], comp.lv4.zx3.meas[,c(1,3,4)], comp.lv4.zx4.meas[,c(1,3,4)], comp.lv4.zx5.meas[,c(1,3,4)],comp.lv4.zx6.meas[,c(1,3,4)],comp.lv4.zx7.meas[,c(1,3,4)], comp.lv4.zx8.meas[,c(1,3,4)], comp.lv4.zx9.meas[,c(1,3,4)], comp.lv4.zx10.meas[,c(1,3,4)]
                        ,comp.lv4.zx11.meas[,c(1,3,4)],comp.lv4.zx12.meas[,c(1,3,4)], comp.lv4.zx13.meas[,c(1,3,4)], comp.lv4.zx14.meas[,c(1,3,4)], comp.lv4.zx15.meas[,c(1,3,4)],comp.lv4.zx16.meas[,c(1,3,4)],comp.lv4.zx17.meas[,c(1,3,4)], comp.lv4.zx18.meas[,c(1,3,4)], comp.lv4.zx19.meas[,c(1,3,4)], comp.lv4.zx20.meas[,c(1,3,4)]
                        ,comp.lv4.zx21.meas[,c(1,3,4)],comp.lv4.zx22.meas[,c(1,3,4)], comp.lv4.zx23.meas[,c(1,3,4)], comp.lv4.zx24.meas[,c(1,3,4)], comp.lv4.zx25.meas[,c(1,3,4)])

print(df.Ebene1)
print(df.Ebene2)
print(df.Ebene4)

end_time <- Sys.time()
end_time - start_time