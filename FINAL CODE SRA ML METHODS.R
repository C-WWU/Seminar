
#----------------------FINAL CODE IN SOCIALLY IRRESPONSIBLE ALGORITHMS------------------------------

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
install.packages("stepPlr")
library(stepPlr)
install.packages("mlbench")
library(mlbench)
install.packages("readxl")
library(readxl)
install.packages("DMwR")
library(DMwR)
install.packages("ROSE")
library(ROSE)
install.packages("ranger")
library(ranger)
library(tidyverse)
install.packages("MASS")
library(MASS)
install.packages("pdp")
library(pdp)


#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable

# c(313 --> das ist hier die column wo die Dv drin ist, in dem Fall weiblich_maennlich)
# c(27:255 --> das sind unsere IV's, sprich die Accounts)
data_GeschlechtMW <- data[,c(313, 27:255)]


### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_GeschlechtMW <- data_GeschlechtMW %>% subset(data_GeschlechtMW$weiblich_maennlich != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_GeschlechtMW$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfGeschlechtMW <- data_GeschlechtMW[index,]
test_dfGeschlechtMW <- data_GeschlechtMW[-index,]


#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------


#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

# hier muss eigentlich nichts geändert werden, es sei denn wir haben ein unbalanced sample, dann müssten wir überlegen welche resampling Methode wir wählen (hier ausgeklammert mit "smote")

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #für linear raus!! Wenn das benutzt wird, auch ClassProbs = True setzen! und ClassProbs für linear auch raus
  classProbs = TRUE, #für linear raus
  allowParallel=TRUE,
  #sampling = "smote", #wenn unbalanced, dann auch ausprobieren für: sampling = "up" / "down" / "smote"
  search = "grid"
)


# set random seed again

set.seed(1997)

# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### DV wird zuerst in den Klammern genannt, das auch immer anpassen. Der Rest kann eigentlich so bleiben. 
### Aktuell ist hier die Logistische Regression als Method eingetragen. 
### Wenn man eine lineare Regression bei bspw. dem Alter machen möchte, dann einmal die Method zu "lm" ändern und family zu "linear"?
### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN

# set random seed again

set.seed(1997)

model1 <- train(weiblich_maennlich ~.,
                data=train_dfGeschlechtMW,
                method = "glmStepAIC", family= binomial, ## es gibt auch eine method für stepwise in train aber nur für linear regression "lmstepAIC" 
                metric = "ROC", #--> for imbalanced data the metric "Kappa" can be used and improves the quality of the final model; for linear regression use "RSME"
                na.action = na.omit,
                trControl=myControl)

set.seed(1998)

model2 = train(weiblich_maennlich ~ Alman_Memes + Pamela_Reif + Tagesschau + AfD + Selena_Gomez, 
               data=train_dfGeschlechtMW,
               method = "glm", family= binomial, 
               metric = "ROC",
               na.action = na.omit,
               trControl=myControl) 

set.seed(1999)

model3 <- train(weiblich_maennlich ~ . ,
                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, 
                metric = "ROC",
                na.action = na.omit,
                trControl=myControl) 

print(model1)
print(model2)
print(model3)

#kappa rules of thumb for interpretation: 
# .81-1.00 Almost perfect
# .61-.80 Substantial
# .41-60 Moderate
# .21-.40 Fair
# .00-.20 Slight
# < .00 Poor 

# Output in terms of regression coefficients

summary(model1)
summary(model2)
summary(model3)

### in der model/summary stehen sowohl der AIC wert, als auch der ROC (AUC) wert, nachdem wir die Modelle miteinander vergleichen können. 

### AUC sollte möglichst hoch sein = nahe 1. 
### AIC sollte so niedrig wie möglich sein. 
### Sensitivity (True Positives) sollte möglichst hoch sein = nahe 1
### Specificity (True Negatives) sollte möglichst hoch sein = nahe 1

#variable Importance (predictor variables)

### diese Funktion gibt noch einmal die 10 wichtigsten variablen des models aus.

varImp(model1)
varImp(model2)
varImp(model3)


# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model1, newdata=test_dfGeschlechtMW)

# Create confusion matrix

confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#-------------------------------------------------RANDOM FOREST-----------------------------------------------------------

#----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


###mtry: wenn numerisch, dann default = sqrt(229); wenn continuous, dann default = 229/3
### generates 300 tress by default, können wir so lassen
###anpassen: IV, data = neues Dataset; mtry anpassen zu entweder sqrt(229) oder 229/3
model <- randomForest(weiblich_maennlich ~ ., data=train_dfGeschlechtMW, na.action = na.omit, proximity=TRUE, mtry = sqrt(229))

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
ggsave("oob_error_rate_500_trees_Geschlecht.pdf")

###Model abspeichern: Variablenname anpassen
model_Geschlecht_500 <- model

## Kommentieren: wie hat sich der Error verändert? Größer, kleiner, oder gleich? Sprich, war es notwendig mit 1000 Trees zu arbeiten?

#zweites Model mit 1000 Trees --> wird error weniger oder stagniert er? 
##Modelgleichung: DV anpassen, data = .. anpassen
model <- randomForest(weiblich_maennlich ~ ., data=train_dfGeschlechtMW, ntree = 1000, na.action = na.omit, proximity=TRUE, mtry = sqrt(229))
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
  temp.model <- randomForest(weiblich_maennlich ~ ., data=train_dfGeschlechtMW,na.action = na.omit, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# find  minimum error
min(oob.values)
# find  optimal value for mtry
which(oob.values == min(oob.values))

# create a model for proximities using the best value for mtry and check for most important variables
###anpassen: DV, Data, ntree
model <- randomForest(weiblich_maennlich ~ ., 
                      data=train_dfGeschlechtMW,
                      ntree=1000, 
                      proximity=TRUE,
                      na.action = na.omit,
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
  partialPlot(model, pred.data = as.data.frame(train_dfGeschlechtMW), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

imp <- varImp(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar <- impvar[11:20]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(model, pred.data = as.data.frame(train_dfGeschlechtMW), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)



#----------------------------------------FIND BEST THRESHOLD WITH ROC----------------------------------------

#-------------------------------------------NUR FÜR BINARY DATA!!!-----------------------------------

### das ist eine Option, wenn für uns für diesen Code entscheiden und gegen die Cross-Validation. denn bei der Cross-Validation wird der ROC/AUC Wert bereits immer mit ausgegeben und muss nicht nochmal einzeln berechnet werden. 

#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, legacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes, renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden

weiblich_maennlich <- train_dfGeschlechtMW$weiblich_maennlich
model_Geschlecht_ROC <- roc(weiblich_maennlich, model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...

roc.info_Geschlecht <- roc(weiblich_maennlich, model$votes[,1], legacy.axes=TRUE)

str(roc.info_Geschlecht)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info_Geschlecht$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info_Geschlecht$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.infoGeschlecht$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "männlich/weiblich".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "männlich/weiblich". 
## Thus, TPP = 0% and FPP = 0%

### Now that we're done with our ROC fun, let's reset the par() variables.

par(pty = "m")

# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model, newdata=test_dfGeschlechtMW)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#-------------------------------------------RANDOM FOREST WITH CROSS-VALIDATION-------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #Wenn das benutzt wird, auch ClassProbs = True setzen!
  classProbs = TRUE,
  allowParallel=TRUE,
  #sampling = "smote", #wenn sampling, dann hier anpassen und für alle drei Varianten ausprobieren!! (up, down, smote)
  search = "random",
)


#set random seed again 

set.seed(400)


modelGeschlechtRF <- train(weiblich_maennlich ~ ., # hier die DV einfügen. "~ ." heißt es werden alle Varibablen im dataframe als IV's genutzt um die DV zu predicten.
                           data=train_dfGeschlechtMW, # hier den data-frame definieren womit trainiert werden soll --> training_df!
                           method="ranger", # ranger is eine schnellere RF methode, man  kann auch "rf" für random forest eingeben
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, # sagt aus, dass fehlende Werte rausgelassen werden beim training
                           num.tree = 500, #
                           trControl = myControl) # training methode: bei uns Cross-Validation


# print model

print(modelGeschlechtRF)
summary(modelGeschlechtRF)


set.seed(401)

# Adjust num.trees to 1000 to evaluate which model performs better

modelGeschlechtRF2 <- train(weiblich_maennlich ~ ., # hier die DV einfügen. "~ ." heißt es werden alle Varibablen im dataframe als IV's genutzt um die DV zu predicten.
                           data=train_dfGeschlechtMW, # hier den data-frame definieren womit trainiert werden soll --> training_df!
                           method="ranger", # ranger is eine schnellere RF methode, man  kann auch "rf" für random forest eingeben
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, # sagt aus, dass fehlende Werte rausgelassen werden beim training
                           num.tree = 1000, #
                           trControl = myControl) # training methode: bei uns Cross-Validation

# print model

print(modelGeschlechtRF2)
summary(modelGeschlechtRF2)

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(1:20),
                     splitrule = "extratrees", # What does this mean? Theres also "gini" --> the gini tells you which variables were the most important for building the trees 
                     min.node.size = c(5,10,15))


modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlechtMW,
                           tuneGrid = myGrid,
                           method="ranger", # ranger is eine schnellere RF methode
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit,
                           num.tree = 500,
                           trControl = myControl)

# Print model to console

modelGeschlechtRF
summary(modelGeschlechtRF)
plot(modelGeschlechtRF)

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry


### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(400)
myGrid <- expand.grid(mtry = 15, splitrule ="extratrees", min.node.size = 10)
modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlechtMW, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 500,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model
### hier den Model namen ändern
print(modelGeschlechtRF)

#output in terms of regression coefficients
summary(modelGeschlechtRF)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelGeschlechtRF)
plot(varImp(modelGeschlechtRF), 20, main = "weiblich_maennlich")


#--------------ACHTUNG: DIE VARIABLE IMPORTANCE + RICHTUNG FUNKTIONIERT FÜR DIESEN CODE NOCH NICHT-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset
imp <- varImp(modelGeschlechtRF)
imp <- imp[[1]]
impvar <- rownames(imp)[order(imp, decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(modelGeschlechtRF, pred.data = as.data.frame(train_dfGeschlechtMW), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

