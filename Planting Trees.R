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
install.packages("pdp")
library(pdp)
install.packages("dplyr")
library(dplyr)


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
data_Geschlecht <- data[,c(25, 27:255)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$Geschlecht <- as.factor(data_Geschlecht$Geschlecht)

#Gibt es NAs in der DV?
sum(is.na(data_Geschlecht$Geschlecht)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Geschlecht <- data_Geschlecht %>% filter(Geschlecht != "NA")

#-----------------------------------------------------------------------------------------------------------------

### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN


#Training und Test Dataset
set.seed(400)

# Partitioning of the data: Create index matrix of selected values

# Create index matrix 
index <- createDataPartition(data_Geschlecht$Geschlecht, p=.8, list= FALSE, times= 1)

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



###mtry: wenn numerisch, dann default = sqrt(229); wenn continuous, dann default = 229/3
### generates 300 tress by default, können wir so lassen
###anpassen: IV, data = neues Dataset; mtry anpassen zu entweder sqrt(229) oder 229/3
model <- randomForest(Geschlecht ~ ., data=train_dfGeschlecht, proximity=TRUE, mtry = sqrt(229))

#Modell prüfen

print(model)

#grafische Darstellung des OOB samples:

###anpassen: Z. 58 times = 1+Ausprägungen; Z. 59 rep anpassen mit ausprägungen, z. 60ff Error = anpassen!!
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"],  
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#Plot und Modell speichern
###NAmen anpassen zu richtiger Variable!
ggsave("oob_error_rate_300_trees_Geschlecht.pdf")

###Model abspeichern: Variablenname anpassen
model_Geschlecht_300 <- model


#zweites Model mit 500 Trees --> wird error weniger oder stagniert er? 
##Modelgleichung: DV anpassen, data = .. anpassen
model <- randomForest(Geschlecht ~ ., data=train_dfGeschlecht, ntree = 500, proximity=TRUE, mtry = sqrt(229))
model


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"], 
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

ggsave("oob_error_rate_500_trees_Geschlecht.pdf")

model_Geschlecht_500 <- model

## Kommentieren: wie hat sich der Error verändert? Größer, kleiner, oder gleich? Sprich, war es notwendig mit 1000 Trees zu arbeiten?

#zweites Model mit 1000 Trees --> wird error weniger oder stagniert er? 
##Modelgleichung: DV anpassen, data = .. anpassen
model <- randomForest(Geschlecht ~ ., data=train_dfGeschlecht, ntree = 1000, proximity=TRUE, mtry = sqrt(229))
model


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"], 
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

ggsave("oob_error_rate_1000_trees_Geschlecht.pdf")

model_Geschlecht_1000 <- model

## Kommentieren: wie hat sich der Error verändert? Größer, kleiner, oder gleich? Sprich, war es notwendig mit 1000 Trees zu arbeiten?


###ABWÄGEN: auch ausprobieren für andere Zahl von Trees notwendig, zb 500? (dann einfach Kopieren und ntree = 500 setzen) 


# Prüfen: was ist das ideale mtry? 
###Zeile 110: Modellgleichung anpassen wie davor; ntree wählen welches besser war (300 oder 1000 oder anderes)
oob.values <- vector(length=20)
for(i in 1:20) {
  temp.model <- randomForest(Geschlecht ~ ., data=train_dfGeschlecht, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# find  minimum error
min(oob.values)
# find  optimal value for mtry
which(oob.values == min(oob.values))

# create a model for proximities using the best value for mtry and check for most important variables
###anpassen: DV, Data, ntree
model <- randomForest(Geschlecht ~ ., 
                      data=train_dfGeschlecht,
                      ntree=1000, 
                      proximity=TRUE,
                      mtry=which(oob.values == min(oob.values)), 
                      Importance=TRUE)

# print model
model

#evaluate variable importance

importance(model)
varImpPlot(model)

# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

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

### Hier wieder DV und dataset austauschen, legacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes, renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden


model_Geschlecht_ROC <- roc(Geschlecht, model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...

roc.info_Geschlecht <- roc(Geschlecht, model$votes[,1], legacy.axes=TRUE)

str(roc.info_Geschlecht)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info_Geschlecht$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info_Geschlecht$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.infoGeschlecht$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%


# ------------------------------------------------MODEL COMPARISON------------------------------------------------


### Können wir auch lassen wenn wir Cross-Validation benutzen!!

# Code wenn wir zwei Modelle vergleichen wollen
##

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