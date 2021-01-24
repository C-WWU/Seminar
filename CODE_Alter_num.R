
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
data_Alter<- data[,c(24, 27:255)]

### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.factor(data_Alter$Alter)

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Geschlecht <- data_GeschlechtMW %>% filter(weiblich_maennlich != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------


#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

# hier muss eigentlich nichts geändert werden, es sei denn wir haben ein unbalanced sample, dann müssten wir überlegen welche resampling Methode wir wählen (hier ausgeklammert mit "smote")

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


# set random seed again

set.seed(1997)

# apply stepwise logistic regression to find most importand IV's 

### die braucht bei uns aktuell enorm lang, da die step-wise 229 Variablen durchrechnen muss. 
### Eventuell kann man diesen Step überspringen und relevante Variablen aus der RF Methode und eigene 5-10 wichtige hinzufügen. 

model <- lm(Alter~., data = train_dfAlter) %>% 
  stepAIC(trace = FALSE)


# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### DV wird zuerst in den Klammern genannt, das auch immer anpassen. Der Rest kann eigentlich so bleiben. 
### Aktuell ist hier die Logistische Regression als Method eingetragen. 
### Wenn man eine lineare Regression bei bspw. dem Alter machen möchte, dann einmal die Method zu "lm" ändern und family zu "linear"?
### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN

# set random seed again

set.seed(1997)

model1 <- train(Alter ~ .,
                data =train_dfAlter,
                method = "lmStepAIC", ## es gibt auch eine method für stepwise in train aber nur für linear regression "lmstepAIC" 
                metric = "RMSE",
                na.action = na.omit,
                trControl=myControl)

set.seed(1998)

model2 = train(Alter ~ ., 
               data=train_dfAlter,
               method = "lm", 
               metric = "Rsquared",
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

predictions <- predict(model1, newdata=test_dfAlter)

# Create confusion matrix

confusionMatrix(data=predictions, test_Alter$Alter)


#-------------------------------------------------RANDOM FOREST-----------------------------------------------------------

#----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


###mtry: wenn numerisch, dann default = sqrt(229); wenn continuous, dann default = 229/3
### generates 300 tress by default, können wir so lassen
###anpassen: IV, data = neues Dataset; mtry anpassen zu entweder sqrt(229) oder 229/3

set.seed(789)

model <- randomForest(Alter ~ ., data=train_dfAlter, proximity=TRUE, mtry = sqrt(229), Importance=TRUE)

#Modell prüfen

print(model)
summary(model)

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
model <- randomForest(Alter ~ ., data=train_dfAlter, ntree = 1000, proximity=TRUE, mtry = sqrt(229), importance =TRUE)
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

set.seed(1234)
model <- tuneRF(train_dfAlter, train_dfAlter$Alter, mtryStart = 3, ntreeTry = 500, stepFactor=2, improve=0.05, trace = TRUE, plot=TRUE) #doBest = TRUE/FALSE



###Zeile 110: Modellgleichung anpassen wie davor; ntree wählen welches besser war (300 oder 1000 oder anderes)
oob.values <- vector(length=20)
for(i in 1:20) {
  temp.model <- randomForest(Alter ~ ., data=train_dfAlter, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# find  minimum error
min(oob.values)
# find  optimal value for mtry
which(oob.values == min(oob.values))

# create a model for proximities using the best value for mtry and check for most important variables
###anpassen: DV, Data, ntree
model <- randomForest(Alter ~ ., 
                      data=train_dfAlter,
                      ntree=1000, 
                      proximity=TRUE,
                      mtry=192, 
                      Importance=TRUE)

# print model
model
summary(model)

#evaluate variable importance

varImp(model)
varImpPlot(model)

# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

#checking direction of the 10-20 most important variables
###anpassen: name vom dataset

imp <- varImp(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(model, pred.data = as.data.frame(train_dfAlter), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

imp <- varImp(model)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar <- impvar[11:20]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partialPlot(model, pred.data = as.data.frame(train_dfAlter), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(op)

# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model, newdata=test_dfAlter)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAlter$Alter)


#-------------------------------------------RANDOM FOREST WITH CROSS-VALIDATION-------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


#set random seed again 

set.seed(400)

# train model with: 300 trees (default)


modelAlter <- train(Alter ~ ., # hier die DV einfügen. "~ ." heißt es werden alle Varibablen im dataframe als IV's genutzt um die DV zu predicten.
                           data=train_dfAlter, # hier den data-frame definieren womit trainiert werden soll --> training_df!
                           method="ranger", # ranger is eine schnellere RF methode, man  kann auch "rf" für random forest eingeben
                           metric= "Rsquared", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, # sagt aus, dass fehlende Werte rausgelassen werden beim training
                           trControl = myControl, 
                          importance = 'impurity') # training methode: bei uns Cross-Validation


# print model

print(modelAlter)

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(1:20),
                     splitrule = "variance", # What does this mean? Theres also "gini" --> the gini tells you which variables were the most important for building the trees 
                     min.node.size = c(5,10,15))


modelAlter <- train(Alter ~ ., 
                           data=train_dfAlter,
                           tuneGrid = myGrid,
                           method="ranger", # ranger is eine schnellere RF methode
                           metric= "Rsquared", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           na.action = na.omit, 
                           trControl = myControl)

# Print model to console

modelAlter
summary(modelAlter)
plot(modelAlter)

#save the best mtry 

bestmtry <- modelAlter$bestTune$mtry


### hier das finale model mit bestmtry und node size einfügen 

set.seed(400)
myGrid <- expand.grid(mtry = bestmtry, splitrule ="variance", min.node.size = 5)
modelGeschlechtRF <- train(Alter ~ ., 
                           data=train_dfAlter, 
                           method="ranger", metric= "Rsquared", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model
### hier den Model namen ändern
print(modelAlter)

#output in terms of regression coefficients
summary(modelAlter)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelAlter)
plot(varImp(modelAlter), 20, main = "Alter")

#checking direction of the 10 most important variables
###anpassen: name vom dataset
imp <- varImp(modelAlter)
imp <- imp[[1]]
impvar <- rownames(imp)[order(imp, decreasing=TRUE)]
impvar <- impvar[1:10]
op <- par(mfrow=c(2, 5))
for (i in seq_along(impvar)) {
  partial(modelGeschlechtRF, pred.data = as.data.frame(train_dfAlter), x.var = impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]), paropts = list(.packages = "ranger"), plot = TRUE)
}
par(op)

# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(modelAlter, newdata=test_dfAlter)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAlter$Alter)


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)
