
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
library(stepPlr)
library(mlbench)
library(readxl)
library(DMwR)
library(ROSE)
library(ranger)
library(tidyverse)
library(MASS)
library(pdp)


#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable

# c(313 --> das ist hier die column wo die Dv drin ist, in dem Fall weiblich_maennlich)
# c(27:255 --> das sind unsere IV's, sprich die Accounts)
data_Partei <- data[,c(7, 27:255)]


### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Partei <- data_Partei %>% filter(Wahl_Partei != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Partei$Wahl_Partei, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfPartei <- data_Partei[index,]
test_dfPartei <- data_Partei[-index,]


#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------


#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

# hier muss eigentlich nichts geändert werden, es sei denn wir haben ein unbalanced sample, dann müssten wir überlegen welche resampling Methode wir wählen (hier ausgeklammert mit "smote")

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #Wenn das benutzt wird, auch ClassProbs = True setzen!; Nimmt in kombin 
  classProbs = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


# set random seed again

set.seed(1997)

# apply stepwise logistic regression to find most importand IV's 

### die braucht bei uns aktuell enorm lang, da die step-wise 229 Variablen durchrechnen muss. 
### Eventuell kann man diesen Step überspringen und relevante Variablen aus der RF Methode und eigene 5-10 wichtige hinzufügen. 

model <- glm(as.factor(Wahl_Partei)~ ., data = train_dfPartei, family = binomial) %>% 
  stepAIC(trace = FALSE)


# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### DV wird zuerst in den Klammern genannt, das auch immer anpassen. Der Rest kann eigentlich so bleiben. 
### Aktuell ist hier die Logistische Regression als Method eingetragen. 
### Wenn man eine lineare Regression bei bspw. dem Alter machen möchte, dann einmal die Method zu "lm" ändern und family zu "linear"?
### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN

# set random seed again

set.seed(1997)

model1 <- train(Wahl_Partei ~ RB_Leipzig + FC_Bayern_Muenchen + adidas_Deutschland + Westwing + dm, 
                data=train_dfPartei,
                method = "glm", family = linear, ## es gibt auch eine method für stepwise in train aber nur für linear regression "lmstepAIC" 
                metric = "ROC",
                na.action = na.omit,
                trControl=myControl)

set.seed(1998)

model2 = train(weiblich_maennlich ~ ., 
               data=train_dfGeschlechtMW,
               method = "glm", family= binomial, 
               na.action = na.omit,
               trControl=myControl) 

set.seed(1999)

model3 <- train(weiblich_maennlich ~ . 
                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, 
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
model <- randomForest(as.factor(Wahl_Partei) ~ ., data=train_dfPartei, proximity=TRUE)

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
model <- randomForest(as.factor(Wahl_Partei) ~ ., data=train_dfPartei, ntree = 1000, proximity=TRUE, mtry = sqrt(229))
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
  temp.model <- randomForest(as.factor(Wahl_Partei) ~ ., data=train_dfPartei, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# find  minimum error
min(oob.values)
# find  optimal value for mtry
which(oob.values == min(oob.values))

# create a model for proximities using the best value for mtry and check for most important variables
###anpassen: DV, Data, ntree
model <- randomForest(as.factor(Wahl_Partei) ~ ., 
                      data=train_dfPartei,
                      ntree=1000, 
                      proximity=TRUE,
                      mtry=which(oob.values == min(oob.values)), 
                      Importance=TRUE)

# print model
model

#evaluate variable importance

varImp(model)
varImpPlot(model)

# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

#checking direction of the 10 most important variables
###anpassen: name vom dataset
imp <- varImp(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(model, pred.data = as.data.frame(train_dfGeschlechtMW), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]), which.class = "Ich würde nicht wählen gehen")
}
par(op)


#----------------------------------------FIND BEST THRESHOLD WITH ROC----------------------------------------

### das ist eine Option, wenn für uns für diesen Code entscheiden und gegen die Cross-Validation. denn bei der Cross-Validation wird der ROC/AUC Wert bereits immer mit ausgegeben und muss nicht nochmal einzeln berechnet werden. 

#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, legacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes, renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden


model_Partei_ROC <- roc(Wahl_Partei, model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...

roc.info_Partei <- roc(Geschlecht, model$votes[,1], legacy.axes=TRUE)

str(roc.info_Partei)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info_Partei$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info_Partei$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.infoPartei$thresholds)

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

predictions <- predict(model, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfPartei$Wahl_Partei)


#-------------------------------------------RANDOM FOREST WITH CROSSVALIDATION-------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #Wenn das benutzt wird, auch ClassProbs = True setzen!
  classProbs = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


#set random seed again 

set.seed(400)

# train model with: 300 trees (default)


modelParteiRF <- train(Alter ~ ., # hier die DV einfügen. "~ ." heißt es werden alle Varibablen im dataframe als IV's genutzt um die DV zu predicten.
                           data=train_dfPartei, # hier den data-frame definieren womit trainiert werden soll --> training_df!
                           method="ranger", # ranger is eine schnellere RF methode, man  kann auch "rf" für random forest eingeben
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, # sagt aus, dass fehlende Werte rausgelassen werden beim training
                           trControl = myControl) # training methode: bei uns Cross-Validation


# print model

print(modelParteiRF)

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(1:20),
                     splitrule = "extratrees", # What does this mean? Theres also "gini" --> the gini tells you which variables were the most important for building the trees 
                     min.node.size = c(5,10,15))


modelParteiRF <- train(Wahl_Partei ~ ., 
                           data=train_dfPartei,
                           tuneGrid = myGrid,
                           method="ranger", # ranger is eine schnellere RF methode
                           metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, 
                           trControl = myControl)

# Print model to console

modelParteiRF
summary(modelParteiRF)
plot(modelParteiRF)

#save the best mtry 

bestmtry <- modelParteiRF$bestTune$mtry


### hier das finale model mit bestmtry und node size einfügen 

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="extratrees", min.node.size = 10)
modelParteiRF <- train(Wahl_Partei ~ ., 
                           data=train_dfPartei, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model
### hier den Model namen ändern
print(modelParteiRF)

#output in terms of regression coefficients
summary(modelParteiRF)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelParteiRF)
plot(varImp(modelParteiRF), 20, main = "weiblich_maennlich")

#checking direction of the 10 most important variables
###anpassen: name vom dataset
imp <- varImp(modelParteiRF)
imp <- imp[[1]]
impvar <- rownames(imp)[order(imp, decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(modelParteiRF, pred.data = as.data.frame(train_dfPartei), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(modelParteiRF, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfPartei$Wahl_Partei)


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

