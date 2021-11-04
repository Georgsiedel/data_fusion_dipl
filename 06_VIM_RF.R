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
donor <- select(donor, -Aufprall_Hindernis) #Aufprall Hindernis ist spezifische Variable
recipient <- recipient[, -1]

#3fache Wiederholung und 10fache Crossvalidation
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#Training mit Ergebnissen des Hyperparametertunings
rf_train<-randomForest(as.factor(AccidentType)~.,data=donor,  trControl = control, replace=TRUE,ntree=2000, mtry=7,
                       importance=TRUE,proximity=TRUE)
#Auswertung nach MDA, MDG, und ROC-Importance und schreiben in CSV-Dateien zur Auswertung
varImpPlot(rf_train,type=1,sort=TRUE, n.var=74 ,main="Wichtigkeit der EV nach min. Fehler")
varImp(rf_train)
imp <- importance(rf_train)
write.csv(imp, 'imp.csv')
print(imp)
print(rf_train)
roc_imp <- filterVarImp(x = donor, y = donor$AccidentType)
print(roc_imp)
write.csv(roc_imp, 'roc_imp.csv')
