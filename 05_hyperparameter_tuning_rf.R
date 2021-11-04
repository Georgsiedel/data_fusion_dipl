library(randomForest)
library(caret)
library(ggplot2)
library(data.table)
library(e1071)
library(dplyr)

daten <- "E:/Documents/Unfalldaten/euska_sachsen"

donor <- read.csv(paste(daten,"/Ergebnisse/3_donor_20k.csv", sep = ""), row.names = NULL, sep = ";")
recipient <- read.csv(paste(daten,"/Ergebnisse/3_recipient_5k.csv", sep = ""), row.names = NULL, sep = ";")

#Loeschen der ersten Spalte, die automatisch hinzugefügt wird
donor <- donor[, -1]
recipient <- recipient[, -1]

#5.1 Tuning von mtry
#5.1.1 Grobes Tuning 3-20

#5 Fache Kreuzuvalidierung, die 1 mal wiederholt wird / Suche mittels Grid
control <- trainControl(method = "repeatedcv", number = 5, repeats = 1, search = "grid")

set.seed(1)

tunegrid <- expand.grid(.mtry = c(3:20))
metric <- "Accuracy"
rf_gridsearch <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control)

print(rf_gridsearch)
plot(rf_gridsearch)

#5.1.2 Feintuning mtry 5-11

#10 Fache Kreuzuvalidierung, die 3 mal wiederholt wird / Suche mittels Grid
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")

set.seed(1)

tunegrid <- expand.grid(.mtry = c(5:11))
metric <- "Accuracy"
rf_gridsearch <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control)

print(rf_gridsearch)
plot(rf_gridsearch)

#5.2 Tuning von ntree (manuell) 

#10 Fache Kreuzuvalidierung, die 3 mal wiederholt wird / Suche mittels Grid
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tunegrid <- expand.grid(.mtry = 9) #Aus Grid-Search ermitteltes mtry
metric <- "Accuracy"                          
modellist <- list()
for(ntree in c(500, 1000, 1500, 2000)) {
  set.seed(1)
  fit <- train(as.factor(AccidentType) ~ ., data = donor, method = "rf",  metric = metric, tuneGrid = tunegrid, trControl = control, ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit              
}

results <- resamples(modellist)
summary(results)
