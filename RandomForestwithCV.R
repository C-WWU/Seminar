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
install.packages("dplyr")
library(dplyr)

#load data
load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#Gibt es NAs in der DV?
sum(is.na(data_Geschlecht$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Geschlecht <- data_Geschlecht %>% filter(weiblich_maennlich != "NA")


###Erklärung:
#eine Raute bedeutet, das hier als Kommentierung stehen lassen (dann sind wir einigermaßen gleich auch wenn alle verschieden Coden)
###3 Rauten bedeuten, dass hier etw. angepasst werden muss. Das danach gerne löschen
###auch gerne eigene Sachen kommentieren, was auch immer zu den Modellen auffällt!

#####
#Geschlecht 
###(Name der DV die wir untersuchen austauschen)

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable
data_Geschlecht <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$weiblich_maennlich <- as.factor(data_Geschlecht$weiblich_maennlich)


### hier das dataset, DV und Ausprägungen anpassen

data_Geschlecht$weiblich_maennlich = as.character(data_Geschlecht$weiblich_maennlich)
data_Geschlecht$weiblich_maennlich[data_Geschlecht$weiblich_maennlich == "männlich"] = "maennlich"
data_Geschlecht$weiblich_maennlich = as.factor(data_Geschlecht$weiblich_maennlich)

#-----------------------------------------------------------------------------------------------------------------

### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN


#Training und Test Dataset
set.seed(400)

# Partitioning of the data: Create index matrix of selected values

# Create index matrix 
index <- createDataPartition(data_Geschlecht$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGeschlecht <- data_Geschlecht[index,]
test_dfGeschlecht <- data_Geschlecht[-index,]

### hier das dataset, DV und Ausprägungen anpassen

#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 1] <- "weiblich"
#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 2] <- "männlich"
#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 3] <- "divers"

#train_dfGeschlecht$Geschlecht <- as.factor(train_dfGeschlecht$Geschlecht)
#test_dfGeschlecht$Geschlecht <- as.factor(test_dfGeschlecht$Geschlecht)



#----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation


myControl = trainControl(
  method = "repeatedcv",
  repeats=3,
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #Wenn das benutzt wird, auch ClassProbs = True setzen!; Nimmt in kombin 
  classProbs = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


#set random seed again 

set.seed(400)

# train model with: 300 trees (default)


modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht, 
                           method="ranger", metric= "ROC", 
                           na.action = na.omit,
                           trControl = myControl)


# print model

print(modelGeschlechtRF)

# test of the ideal mtry

myGrid = expand.grid(mtry = c(1:20),
                     splitrule = "extratrees", # What does this mean? Theres also "gini" --> the gini tells you which variables were the most important for building the trees 
                     min.node.size = c(5,10,15))


modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit,
                           trControl = myControl)

# Print model to console

modelGeschlechtRF
summary(modelGeschlechtRF)
plot(modelGeschlechtRF)

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry


# use it in Random Forest Model

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="extratrees", min.node.size = 5)
modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           trControl = myControl)

# search for the best ntrees

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="extratrees", min.node.size = 5)
modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           ntree = 500,
                           trControl = myControl)

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="extratrees", min.node.size = 5)
modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           ntree = 1000,
                           trControl = myControl)

### hier das finale model mit n trees, bestmtry und node size einfügen 

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="extratrees", min.node.size = 5)
modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlecht, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           ntree = 500,
                           trControl = myControl,
                           importance = TRUE)

# Print model
### hier den Model namen ändern
print(modelGeschlechtRF)

#output in terms of regression coefficients
summary(modelGeschlechtRF)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

importance(modelGeschlechtRF)
varImpPlot(modelGeschlechtRF)

#checking direction of the 10 most important variables
###anpassen: name vom dataset
imp <- importance(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(model, pred.data = as.data.frame(train_dfGeschlecht), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)


#------------------------------------------test for multicollinearity----------------------------

install.packages("car")
library(car)

vif(model)

#----------------------------------------FIND BEST THRESHOLD WITH ROC----------------------------------------


#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, ledacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes, renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden

#Geschlecht <- data_Geschlecht$Geschlecht
model_Geschlecht_ROC <- roc(weiblich_maennlich~modelGeschlechtRF$votes[,1], plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...
roc.info_Geschlecht <- roc(weiblich_maennlich, modelGeschlechtRF$votes[,1], legacy.axes=TRUE)
str(roc.info_Geschlecht)


# ------------------------------------------------MODEL COMPARISON------------------------------------------------


### Können wir auch lassen wenn wir Cross-Validation benutzen!!

# Code wenn wir zwei Modelle vergleichen wollen

# roc(Geschlecht, model$data_Geschlecht, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# plot.roc(Geschlecht, model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

# Legend creates a legend on the ROC plot giving your models names and different colors 

# legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)


# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model, newdata=test_dfGeschlecht)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlecht$Geschlecht)


#--------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)