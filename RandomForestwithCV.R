#####################################
##
## Now we are ready to build a random forest.
##
#####################################

#install and load relevant packages
library(ggplot2)
install.packages("cowplot")
library(cowplot)
install.packages("randomForest")
library(randomForest)
install.packages("pROC")
library(pROC)
install.packages("readr")
library(readr)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

#load data
load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names


###Erklärung:
#eine Raute bedeutet, das hier als Kommentierung stehen lassen (dann sind wir einigermaßen gleich auch wenn alle verschieden Coden)
###3 Rauten bedeuten, dass hier etw. angepasst werden muss. Das danach gerne löschen
###auch gerne eigene Sachen kommentieren, was auch immer zu den Modellen auffällt!

#####
#Geschlecht 
###(Name der DV die wir untersuchen austauschen)

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable
data_Geschlecht <- data[,c(28, 30:258)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$Geschlecht <- as.factor(data_Geschlecht$Geschlecht)


#Training und Test Dataset
set.seed(400)

# Partitioning of the data: Create index matrix of selected values

# Create index matrix 
index <- createDataPartition(data_Geschlecht$Geschlecht, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGeschlecht <- data_Geschlecht[index,]
test_dfGeschlecht <- data_Geschlecht[-index,]

# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

#crtlspecs <- trainControl(method="cv", number=10, 
                          #savePredictions="all",
                          #classProbs = TRUE)

trControl <- trainControl(method = "cv", 
                          number=10,
                          search="grid")


#set random seed again 

set.seed(400)

# train model with: 300 trees (default)

###mtry: wenn numerisch, dann default = sqrt(229); wenn continuous, dann default = 229/3
### generates 300 tress by default, können wir so lassen
###anpassen: IV, data = neues Dataset; mtry anpassen zu entweder sqrt(229) oder 229/3

modelGeschlechtRF <- train(Geschlecht ~ ., 
                           data=train_dfGeschlecht, 
                           method="rf", metric= "Accuracy", 
                           trControl = trControl)

# print model

print(modelGeschlechtRF)

# test of the ideal mtry

set.seed(400)
tuneGrid <- expand.grid(.mtry = c(1: 20))
rf_mtry <- train(Geschlecht ~ ., 
                 data=train_dfGeschlecht, 
                 method="rf", metric= "Accuracy", 
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,)

print(rf_mtry)

#save the best mtry 

bestmtry <- rf_mtry$bestTune$mtry

# use it in Random Forest Model

set.seed(400)
tuneGrid <- expand.grid(.mtry = bestmtry)
modelGeschlechtRF <- train(Geschlecht ~ ., 
                           data=train_dfGeschlecht, 
                           method="rf", metric= "Accuracy", 
                           tuneGrid = tuneGrid,
                           trControl = trControl,
                           importance = TRUE)

# search for the best ntrees

store_maxtrees <- list()
for (ntree in c(300, 350, 400, 450, 500, 550, 600, 800, 1000)) {
  set.seed(400)
  rf_maxtrees <- train(Geschlecht~.,
                       data = train_dfGeschlecht,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

### hier das finale model mit n trees und bestmtry einfügen 



# Print model
### hier den Model namen ändern
print(modelGeschlechtRF)

#output in terms of regression coefficients
summary(modelGeschlechtRF)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

importance(modelGeschlechtRF)
varImp(modelGeschlechtRF)
varImpPlot(modelGeschlechtRF)

#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, ledacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes, renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden

Geschlecht <- data_Geschlecht$Geschlecht
model_Geschlecht_ROC <- roc(Geschlecht, modelGeschlechtRF$votes[,1], plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...
roc.info_Geschlecht <- roc(Geschlecht, modelGeschlechtRF$votes[,1], legacy.axes=TRUE)
str(roc.info_Geschlecht)

# Code wenn wir zwei Modelle vergleichen wollen
##
#######################################
# roc(Geschlecht, model$data_Geschlecht, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# plot.roc(Geschlecht, model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

# Legend creates a legend on the ROC plot giving your models names and different colors 

# legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlecht)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlecht$Geschlecht)