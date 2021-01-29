
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
library(plyr)
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
install.packages("elasticnet")
library(elasticnet)
install.packages("glmnet")
library(glmnet)
install.packages("Matrix")
library(Matrix)

options(max.print = 100000)

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



# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### DV wird zuerst in den Klammern genannt, das auch immer anpassen. Der Rest kann eigentlich so bleiben. 
### Aktuell ist hier die Logistische Regression als Method eingetragen. 
### Wenn man eine lineare Regression bei bspw. dem Alter machen möchte, dann einmal die Method zu "lm" ändern und family zu "linear"?
### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN



#--------------first regression: all parameters-----------------

set.seed(1997)

model1 <- train(weiblich_maennlich ~.,
                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, ## für mehr als zwei Ausprägungen (z.B. Alkohol) --> method = "multinom"
                metric = "ROC", #--> for imbalanced data the metric "Kappa" can be used and improves the quality of the final model; for linear regression use "RSME"
                na.action = na.omit,
                trControl=myControl)

print(model1)
summary(model1)

#variable Importance (predictor variables)

### diese Funktion gibt noch einmal die 10 wichtigsten variablen des models aus.

varImp(model1)

#look for most important variables
ImportanceAll1 <- varImp(model1)$importance
ImportanceAll1 <- arrange(ImportanceAll1, desc(Overall))
ImportanceAll1

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions1 <- predict(model1, newdata=test_dfGeschlechtMW)


# Create confusion matrix

confusionMatrix(data=predictions1, test_dfGeschlechtMW$weiblich_maennlich)



#------second regression: ridge/lasso for shrinking model---------

set.seed(1998)

myGrid <- expand.grid(alpha = 0:1,
                      lambda = seq(0.0001, 1, length = 100))

model2 <- train(weiblich_maennlich ~ .,
               data=train_dfGeschlechtMW,
               method = "glmnet", 
               metric = "ROC", 
               na.action = na.omit,
               tuneGrid = myGrid,
               trControl=myControl) 

print(model2)
summary(model2)
coef(model2$finalModel, model2$finalModel$lambdaOpt)


varImp(model2)

ImportanceAll2 <- varImp(model2)$importance
ImportanceAll2 <- arrange(ImportanceAll2, desc(Overall))
ImportanceAll2


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions2 <- predict(model2, newdata=test_dfGeschlechtMW)


# Create confusion matrix

confusionMatrix(data=predictions2, test_dfGeschlechtMW$weiblich_maennlich)


#------------third regression: specify ideal model--------------

set.seed(1999)

model3 <- train(weiblich_maennlich ~ EA_Sports_FIFA + Mady_Morrison + Gamingzelle + Montana_Black + Jens_Knossalla + kicker + Bundeswehr + Christian_Lindner + Die_Partei + Inscope21 + Ischtar_Isik + RB_Leipzig + Reyst + Leon_Skincare + Christoph_Icke_Dommisch + Linda_DIY + NYX_Professional_Makeup + Tiere_suchen_ein_Zuhause + dm + Playboy_Germany,                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, 
                metric = "ROC",
                na.action = na.omit,
                trControl=myControl) 

print(model3)
summary(model3)

varImp(model3)

ImportanceAll3 <- varImp(model3)$importance
ImportanceAll3 <- arrange(ImportanceAll3, desc(Overall))
ImportanceAll3


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions3 <- predict(model3, newdata=test_dfGeschlechtMW)


# Create confusion matrix

confusionMatrix(data=predictions3, test_dfGeschlechtMW$weiblich_maennlich)


#----------save best regression model----------------------

bestregression_GeschlechtMW <- model3

#####


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

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
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!

set.seed(1997)

myGrid = expand.grid(mtry = 10,
                     splitrule = "extratrees", 
                     min.node.size = 15)


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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)



####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(400)
myGrid <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(modelGeschlechtRF$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- modelGeschlechtRF

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

