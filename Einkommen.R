
#----------------------FINAL CODE IN SOCIALLY IRRESPONSIBLE ALGORITHMS------------------------------

#install and load relevant packages

install.packages("cowplot")
install.packages("randomForest")
install.packages("pROC")
install.packages("readr")
install.packages("caret")
install.packages("e1071")
install.packages("stepPlr")
install.packages("mlbench")
install.packages("readxl")
install.packages("DMwR")
install.packages("ROSE")
install.packages("ranger")
install.packages("MASS")
install.packages("pdp")
install.packages("elasticnet")
install.packages("glmnet")
install.packages("Matrix")
install.packages("Hmisc")


library(ggplot2)
library(cowplot)
library(randomForest)
library(pROC)
library(readr)
library(caret)
library(e1071)
library(plyr)
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
library(elasticnet)
library(glmnet)
library(Matrix)
library(Hmisc)


options(max.print = 100000)


###Miriam Stand: 
#Code steht, muss angepasst werden für alle 3 Versionen
#evtl. nochmal binär probieren, wenn Ergebnisse nichts werden?

#######################
#Einkommensgruppe: Categorical (3 Gruppen: hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Einkommen <- data[,c(315, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Einkommen$Einkommensgruppe)) #122 NAs
data_Einkommen <- data_Einkommen %>% subset(data_Einkommen$Einkommensgruppe != "NA")


#ist die Variable unbalanced?
table(data_Einkommen$Einkommensgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Einkommen$Einkommensgruppe)/sum(table(data_Einkommen$Einkommensgruppe))) #no information rate 61%

#IV als Faktor:
data_Einkommen$Einkommensgruppe <- as.factor(data_Einkommen$Einkommensgruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Einkommen$Einkommensgruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfEinkommen <- data_Einkommen[index,]
test_dfEinkommen <- data_Einkommen[-index,]

#setting reference level: 
train_dfEinkommen$Einkommensgruppe <- relevel(train_dfEinkommen$Einkommensgruppe, ref = "mittel")




#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------

#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)

myControl2 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "up", 
  search = "grid"
)

myControl3 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "down", 
  search = "grid"
)




# Specify multinomial regression model with most important IV's


#--------------first regression with Control1: all parameters-----------------

set.seed(1997)

model1.1 <- train(Einkommensgruppe ~.,
                  data=train_dfEinkommen,
                  method = "lda", 
                  metric = "Kappa",  #metric Kappa hilft bei imbalanced data
                  na.action = na.omit,
                  trControl=myControl1)

print(model1.1)
summary(model1.1)

#variable Importance (predictor variables)

varImp(model1.1)

#look for most important variables
ImportanceAll1.1 <- varImp(model1.1)$importance
ImportanceAll1.1 <- arrange(ImportanceAll1.1, desc(Overall))
ImportanceAll1.1

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.1 <- predict(model1.1, newdata=test_dfEinkommen)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.1), as.factor(test_dfEinkommen$Einkommensgruppe))


#--------------first regression with Control2: all parameters-----------------

set.seed(1997)

model1.2 <- train(Einkommensgruppe ~.,
                  data=train_dfEinkommen,
                  method = "multinom", 
                  metric = "Kappa", 
                  na.action = na.omit,
                  trControl=myControl2)

print(model1.2)
summary(model1.2)

#variable Importance (predictor variables)

varImp(model1.2)

#look for most important variables
ImportanceAll1.2 <- varImp(model1.2)$importance
ImportanceAll1.2 <- arrange(ImportanceAll1.2, desc(Overall))
ImportanceAll1.2

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.2 <- predict(model1.2, newdata=test_dfEinkommen)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.2), as.factor(test_dfEinkommen$Einkommensgruppe))


#--------------first regression with Control3: all parameters-----------------

set.seed(1997)

model1.3 <- train(Einkommensgruppe ~.,
                  data=train_dfEinkommen,
                  method = "multinom", 
                  metric = "Kappa", 
                  na.action = na.omit,
                  trControl=myControl3)

print(model1.3)
summary(model1.3)

#variable Importance (predictor variables)

varImp(model1.3)

#look for most important variables
ImportanceAll1.3 <- varImp(model1.3)$importance
ImportanceAll1.3 <- arrange(ImportanceAll1.3, desc(Overall))
ImportanceAll1.3

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.3 <- predict(model1.3, newdata=test_dfEinkommen)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.3), as.factor(test_dfEinkommen$Einkommensgruppe))

##upsampling (Control2) works best, will be used further --> best ROC and Sens, second best prediction accuracy


#------second regression: ridge/lasso for shrinking model---------

set.seed(1998)

myGrid <- expand.grid(alpha = 0:1,
                      lambda = seq(0.0001, 1, length = 100))

model2 <- train(Einkommensgruppe ~ .,
                data=train_dfEinkommen,
                method = "glmnet", 
                metric = "Kappa", 
                na.action = na.omit,
                tuneGrid = myGrid,
                trControl=myControl2) 

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

predictions2 <- predict(model2, newdata=test_dfEinkommen)


# Create confusion matrix

confusionMatrix(as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))


#------------third regression: specify ideal model--------------

#######WEITER ANPASSEN!!!

set.seed(1999)

model3 <- train(Einkommensgruppe ~ Die_groesste_Community_fuer_Muetter + Mohammed_Harkous + Felix_Lobrecht + RTL_Aktuell + Lisa_Mueller + Kelly_Misses_Vlog + DFB + Ischtar_Isik + Alice_Weidel + McFit + Aldi_Nord + Annenmaykantereit + Xbox_DACH + Yvonne_Pfeffer + Manuel_Neuer + Felix_von_der_Laden + Ford_Deutschland + Lillydoo + LionTTV + Wacken_Open_Air + CDU + AfD + FDP + Sarah_Harrison + Plantbased_Food_and_Travel + Create_By_Obi + katholisch_de + Evangelisch_de + Weber_Grill,
                data = train_dfEinkommen,
                method = "multinom", 
                metric = "Kappa",
                na.action = na.omit,
                trControl=myControl1) 

print(model3)
summary(model3)

#Signifikanzen sind nicht enthalten, daher nachbauen:
z <- summary(model3)$coefficients/summary(model3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


varImp(model3)

ImportanceAll3 <- varImp(model3)$importance
ImportanceAll3 <- arrange(ImportanceAll3, desc(Overall))
ImportanceAll3


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions3 <- as.factor(predict(model3, newdata=test_dfEinkommen))


# Create confusion matrix

confusionMatrix(data=predictions3, as.factor(test_dfEinkommen$Einkommensgruppe))


#----------save best regression model----------------------

bestregression_Green1 <- model3


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)

myControl2 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "up", 
  search = "grid"
)

myControl3 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "down", 
  search = "grid"
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size
#use metric Kappa because of unbalanced dataset

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RFEinkommen_11 <- train(Einkommensgruppe ~ ., 
                     data=train_dfEinkommen,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "Kappa",
                     num.tree = 500,
                     trControl = myControl1, 
                     na.action = na.omit,
                     importance = 'impurity')
set.seed(1997)
RFEinkommen_12 <- train(Einkommensgruppe ~ ., 
                     data=train_dfEinkommen,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "Kappa",
                     num.tree = 500,
                     trControl = myControl2,
                     na.action = na.omit, 
                     importance = 'impurity')
set.seed(1997)
RFEinkommen_13 <- train(Einkommensgruppe ~ ., 
                     data=train_dfEinkommen,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "Kappa",
                     num.tree = 500,
                     trControl = myControl3,
                     na.action = na.omit, 
                     importance = 'impurity')

# Print models to console

RFEinkommen_11
summary(RFEinkommen_11)
plot(RFEinkommen_11)
  #mtry = 17, extratrees, min.node.size = 10

RFEinkommen_12
summary(RFEinkommen_12)
plot(RFEinkommen_12)

RFEinkommen_13
summary(RFEinkommen_13)
plot(RFEinkommen_13)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFEinkommen_11, newdata=test_dfEinkommen)
predictions2 <- predict(RFEinkommen_12, newdata=test_dfEinkommen)
predictions3 <- predict(RFEinkommen_13, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
      predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1 auc
RFEinkommen_11 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model2 auc
RFEinkommen_12 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model3 auc
RFEinkommen_13 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()



#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
myGrid1 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
myGrid2 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
myGrid3 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)

set.seed(1997)
RFEinkommen_21 <- train(Einkommensgruppe ~ ., 
                          data=train_dfEinkommen, 
                          method="ranger", metric= "Kappa",
                          tuneGrid = myGrid1,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl1, 
                          importance = 'impurity')
set.seed(1997)
RFEinkommen_22 <- train(Einkommensgruppe ~ ., 
                          data=train_dfEinkommen, 
                          method="ranger", metric= "Kappa",
                          tuneGrid = myGrid2,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl2, 
                          importance = 'impurity')
set.seed(1997)
RFEinkommen_23 <- train(Einkommensgruppe ~ ., 
                          data=train_dfEinkommen, 
                          method="ranger", metric= "ROC",
                          tuneGrid = myGrid3,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl3, 
                          importance = 'impurity')

# Print models
RFEinkommen_21
summary(RFEinkommen_21)
plot(RFEinkommen_21)
  #mtry = 10, extratrees, min.node.size = 15


RFEinkommen_22
summary(RFEinkommen_22)
plot(RFEinkommen_22)

RFEinkommen_23
summary(RFEinkommen_23)
plot(RFEinkommen_23)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFEinkommen_21, newdata=test_dfEinkommen)
predictions2 <- predict(RFEinkommen_fin2, newdata=test_dfEinkommen)
predictions3 <- predict(RFEinkommen_23, newdata=test_dfEinkommen)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfEinkommen$Einkommensgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1
RFEinkommen_21 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model2
RFEinkommen_22 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model3
RFEinkommen_23 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()



#model1: 500 trees performs better
#model2: xx trees performs better
#model3: xx trees performs better

####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
myGrid1 <- expand.grid(mtry = 17, splitrule ="extratrees", min.node.size = 10)
myGrid2 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
myGrid3 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)

set.seed(1997)
RFEinkommen_fin1 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid1,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl1, 
                       importance = 'impurity')
set.seed(1997)
RFEinkommen_fin2 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid2,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl2, 
                       importance = 'impurity')
set.seed(1997)
RFEinkommen_fin3 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid3,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl3, 
                       importance = 'impurity')

# Print models
RFEinkommen_fin1
summary(RFEinkommen_fin1)
plot(RFEinkommen_fin1)

RFEinkommen_fin2
summary(RFEinkommen_fin2)
plot(RFEinkommen_fin2)

RFEinkommen_fin3
summary(RFEinkommen_fin3)
plot(RFEinkommen_fin3)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFEinkommen_fin1)
plot(varImp(RFEinkommen_fin1), 20, main = "Green_Values")

varImp(RFEinkommen_fin2)
plot(varImp(RFEinkommen_fin2), 20, main = "Green_Values")

varImp(RFEinkommen_fin3)
plot(varImp(RFEinkommen_fin3), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFEinkommen_fin1, newdata=test_dfEinkommen)
predictions <- predict(RFEinkommen_fin2, newdata=test_dfEinkommen)
predictions <- predict(RFEinkommen_fin3, newdata=test_dfEinkommen)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfEinkommen$Einkommensgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1
RFEinkommen_31 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model2
RFEinkommen_32 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#model3
RFEinkommen_33 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()


#best predictor: model x (sampling = xx)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFEinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFEinkommen_fin

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

besttree_Einkommen <- RFGreen2_fin123
saveRDS(besttree_Einkommen, "./tree_Einkommen.rds")

#load the model

tree_Einkommen <- readRDS("./tree_Einkommen.rds")
print(tree_Einkommen)