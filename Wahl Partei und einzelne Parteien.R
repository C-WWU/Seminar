
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

options(max.print = 100000)


####Stand Miriam

#alle: kann man machen aber muss es auch nicht --> angefangen, muss angepasst werden
#AfD: imbalanced
#Grün: okay
#CDU/CSU: okay
#Linke imbalanced
#FDP imbalanced
#Nichtwähler imbalanced
#SPD imbalanced
#die Partei: zu klein!



#######################
#Parteien: categorical, alle (9 Gruppen, Sonstige ausschließen --> 8 Gruppen)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Partei <- data[,c(7, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Partei$Wahl_Partei)) #181 NAs
#Sonstige auch als NA, um sie aus Analyse auszuschließen:
#"keine Angaben" als NA umcoden, um aus Analyse auszuschließen
data_Partei <- data_Partei %>% replace_with_na_all(condition = ~.x == "Sonstige:")
#Datenset ohne NAs
data_Partei <- data_Partei %>% subset(data_Partei$Wahl_Partei != "NA")


#ist die Variable unbalanced?
table(data_Partei$Wahl_Partei) #CDU & Grüne überwiegen, aber Verhältnis ist noch ok
max(table(data_Partei$Wahl_Partei)/sum(table(data_Partei$Wahl_Partei))) #no information rate 27%

#IV als Faktor:
data_Partei$Wahl_Partei <- as.factor(data_Partei$Wahl_Partei)

#Variablennamen anpassen für Analyse
data_Partei <- data_Partei %>% mutate(Wahl_Partei = case_when(Wahl_Partei == "CDU/CSU" ~ 'CDU_CSU',
                                                              Wahl_Partei == "SPD" ~ 'SPD',
                                                              Wahl_Partei == "Bündnis 90/Die Grünen" ~ 'Die_Gruenen',
                                                              Wahl_Partei == "FDP" ~ 'FDP',
                                                              Wahl_Partei == "AfD" ~ 'AfD',
                                                              Wahl_Partei == "Die Linke" ~ 'Die_Linke',
                                                              Wahl_Partei == "Die Partei" ~ 'Die_Partei',
                                                              Wahl_Partei == "Ich würde nicht wählen gehen" ~ 'Nichtwaehler'))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Partei$Wahl_Partei, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfPartei <- data_Partei[index,]
test_dfPartei <- data_Partei[-index,]



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


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation, kein resampling nötig

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size
#use metric Kappa because of categorical

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RFPartei_1 <- train(Wahl_Partei ~ ., 
                        data=train_dfPartei,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "Kappa",
                        num.tree = 500,
                        trControl = myControl1, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print model to console

RFPartei_1
summary(RFPartei_1)
plot(RFPartei_1)
#mtry = 19, extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFPartei_1, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfPartei$Wahl_Partei))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model1 auc: 0.6527
RFPartei_1 %>%
  test_roc(data = test_dfPartei) %>%
  auc()



#save the best mtry 

bestmtry <- RFPartei_1$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
myGrid1 <- expand.grid(mtry = 19, splitrule ="extratrees", min.node.size = 15)

set.seed(1997)
RFPartei_2 <- train(Wahl_Partei ~ ., 
                        data=train_dfPartei, 
                        method="ranger", metric= "Kappa",
                        tuneGrid = myGrid1,
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl1, 
                        importance = 'impurity')

# Print models
RFPartei_2
summary(RFPartei_2)
#mtry = xx, extratrees, min.node.size = xx

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFPartei_2, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfPartei$Wahl_Partei))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model1
RFPartei_2 %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#model: 1000 trees performs better


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFPartei_fin <- train(Wahl_Partei ~ ., 
                          data=train_dfPartei, 
                          method="ranger", metric= "Kappa",
                          tuneGrid = myGrid1,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl1, 
                          importance = 'impurity')

# Print models
RFPartei_fin
summary(RFPartei_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFPartei_fin)
plot(varImp(RFPartei_fin), 20, main = "Wahl_Partei")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFPartei_fin, newdata=test_dfPartei)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfPartei$Wahl_Partei))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model AUC: 0.6399
RFPartei_fin %>%
  test_roc(data = test_dfPartei) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFPartei_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFPartei_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "AfD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "AfD") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "CDU_CSU") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "CDU_CSU") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Gruenen") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Gruenen") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Linke") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Linke") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Partei") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Partei") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "FDP") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "FDP") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Nichtwaehler") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Nichtwaehler") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "SPD") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "SPD") %>%plotPartial






#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Partei <- RFPartei_fin
saveRDS(besttree_Partei, "./tree_Partei.rds")

#load the model

besttree_Partei <- readRDS("./tree_Partei.rds")
print(besttree_Partei)





#######################
#AfD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_AfD <- data[,c(341, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_AfD$AfD_Waehler)) #181 NAs
data_AfD <- data_AfD %>% subset(data_AfD$AfD_Waehler != "NA")


#ist die Variable unbalanced?
table(data_AfD$AfD_Waehler) #JA--> in Tests mit beachten!
max(table(data_AfD$AfD_Waehler)/sum(table(data_AfD$AfD_Waehler))) #no information rate 93%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AfD$AfD_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfAfD <- data_AfD[index,]
test_dfAfD <- data_AfD[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RfAfD_11 <- train(AfD_Waehler ~ ., 
                     data=train_dfAfD,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 500,
                     na.action = na.omit,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models to console

RfAfD_11
summary(RfAfD_11)
plot(RfAfD_11)
#mtry = xx, extratrees, min.node.size = xx



# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen2_11, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RfAfD_11 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RfAfD_11)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RfAfD_21 <- train(AfD_Waehler ~ ., 
                     data=train_dfAfD,
                     tuneGrid = myGrid1,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 1000,
                     na.action = na.omit,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models to console

RfAfD_21
summary(RfAfD_21)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RfAfD_21, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAfD$AfD_Waehler))

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RfAfD_21 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RfAfD_21)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFAfD_fin1 <- train(AfD_Waehler ~ ., 
                       data=train_dfGreen2, 
                       method="ranger", metric= "ROC",
                       tuneGrid = myGrid1,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models
RFAfD_fin1
summary(RFAfD_fin1)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFAfD_fin1)
plot(varImp(RFAfD_fin1), 20, main = "Afd_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAfD_fin1, newdata=test_dfAfD)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFAfD_fin1 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RfAfD_11,
                   Model2 = RfAfD_21,
                   Model3 = RFAfD_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#best forest: model x (sampling = xx)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFGreen2_fin1$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFGreen2_fin1

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_AfD <- RFGreen2_fin1
saveRDS(besttree_AfD, "./tree_AfD.rds")

#load the model

Tree_AfD <- readRDS("./tree_AfD.rds")
print(Tree_AfD)







#######################
#Die Grünen: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_Gruen <- data[,c(339, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Gruen$Gruene_Waehler)) #181 NAs
data_Gruen <- data_Gruen %>% subset(data_Gruen$Gruene_Waehler != "NA")


#ist die Variable unbalanced?
table(data_Gruen$Gruene_Waehler) #Verteilung 1:3 --> slightly imbalanced
max(table(data_Gruen$Gruene_Waehler)/sum(table(data_Gruen$Gruene_Waehler))) #no information rate 73%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Gruen$Gruene_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGruen <- data_Gruen[index,]
test_dfGruen <- data_Gruen[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_Gruene1 <- train(Gruene_Waehler ~ ., 
                  data=train_dfGruen,
                  tuneGrid = myGrid,
                  method="ranger", 
                  metric= "ROC",
                  num.tree = 500,
                  na.action = na.omit,
                  trControl = myControl1, 
                  importance = 'impurity')

# Print models to console

RF_Gruene1
summary(RF_Gruene1)
plot(RF_Gruene1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Gruene1, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGruen$Gruene_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Gruene1 %>%
  test_roc(data = test_dfGruen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_Gruene1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = x, splitrule ="extratrees", min.node.size = x)

set.seed(1997)
RF_Gruene2 <- train(Gruene_Waehler ~ ., 
                  data=train_dfGruen,
                  tuneGrid = myGrid1,
                  method="ranger", 
                  metric= "ROC",
                  num.tree = 1000,
                  na.action = na.omit,
                  trControl = myControl1, 
                  importance = 'impurity')

# Print models to console

RF_Gruene2
summary(RF_Gruene2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Gruene2, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Gruene2 %>%
  test_roc(data = test_dfGruen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_Gruene2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: 1000 trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFGruene_fin <- train(Gruene_Waehler ~ ., 
                    data=train_dfGruen, 
                    method="ranger", metric= "ROC",
                    tuneGrid = myGrid1,
                    num.tree = 1000,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models
RFGruene_fin
summary(RFGruene_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFGruene_fin)
plot(varImp(RFGruene_fin), 20, main = "Gruene_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFGruene_fin, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFGruene_fin %>%
  test_roc(data = test_dfGruen) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Gruene1,
                   Model2 = RF_Gruene2,
                   Model3 = RFGruene_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#best forest: model x (sampling = xx)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFGruene_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFGruene_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Grün <- RFGruene_fin
saveRDS(besttree_Grün, "./tree_Grün.rds")

#load the model

Tree_Grün <- readRDS("./tree_Grün.rds")
print(Tree_Grün)





#######################
#CDU/CSU: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_CDU <- data[,c(337, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_CDU$CDU_CSU_Waehler)) #181 NAs
data_CDU <- data_CDU %>% subset(data_CDU$CDU_CSU_Waehler != "NA")
data_CDU$CDU_CSU_Waehler <- as.factor(data_CDU$CDU_CSU_Waehler)


#ist die Variable unbalanced?
table(data_CDU$CDU_CSU_Waehler) #Verteilung 1:3 --> slightly imbalanced
max(table(data_CDU$CDU_CSU_Waehler)/sum(table(data_CDU$CDU_CSU_Waehler))) #no information rate 75%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_CDU$CDU_CSU_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfCDU <- data_CDU[index,]
test_dfCDU <- data_CDU[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_CDU1 <- train(CDU_CSU_Waehler ~ ., 
                    data=train_dfCDU,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RF_CDU1
summary(RF_CDU1)
plot(RF_CDU1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_CDU1, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfCDU$CDU_CSU_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_CDU1 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_CDU1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RF_CDU2 <- train(CDU_CSU_Waehler ~ ., 
                    data=train_dfCDU,
                    tuneGrid = myGrid1,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RF_CDU2
summary(RF_CDU2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_CDU2, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_CDU2 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_CDU2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: 1000 trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_CDU_fin <- train(CDU_CSU_Waehler ~ ., 
                      data=train_dfCDU, 
                      method="ranger", metric= "ROC",
                      tuneGrid = myGrid1,
                      num.tree = 1000,
                      trControl = myControl1, 
                      na.action = na.omit,
                      importance = 'impurity')

# Print models
RF_CDU_fin
summary(RF_CDU_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_CDU_fin)
plot(varImp(RF_CDU_fin), 20, main = "CDU_CSU_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_CDU_fin, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFGruene_fin %>%
  test_roc(data = test_dfCDU) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_CDU1,
                   Model2 = RF_CDU2,
                   Model3 = RF_CDU_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#best forest: model x (sampling = xx)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RF_CDU_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RF_CDU_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_CDU <- RF_CDU_fin
saveRDS(besttree_CDU, "./tree_Grün.rds")

#load the model

tree_CDU <- readRDS("./tree_Grün.rds")
print(tree_CDU)


#######################
#Die Linke: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_Linke <- data[,c(342, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Linke$Linke_Waehler)) #181 NAs
data_Linke <- data_Linke %>% subset(data_Linke$Linke_Waehler != "NA")
data_Linke$Linke_Waehler <- as.factor(data_Linke$Linke_Waehler)


#ist die Variable unbalanced?
table(data_Linke$Linke_Waehler) #very imbalanced
max(table(data_Linke$Linke_Waehler)/sum(table(data_Linke$Linke_Waehler))) #no information rate 92%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Linke$Linke_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfLinke <- data_Linke[index,]
test_dfLinke <- data_Linke[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_Linke1 <- train(Linke_Waehler ~ ., 
                 data=train_dfLinke,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_Linke1
summary(RF_Linke1)
plot(RF_Linke1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Linke1, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfLinke$Linke_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Linke1 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_Linke1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RF_Linke2 <- train(Linke_Waehler ~ ., 
                 data=train_dfLinke,
                 tuneGrid = myGrid1,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_Linke2
summary(RF_Linke2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Linke2, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Linke2 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_Linke2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_Linke_fin <- train(Linke_Waehler ~ ., 
                    data=train_dfLinke, 
                    method="ranger", metric= "ROC",
                    tuneGrid = myGrid1,
                    num.tree = 1000,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models
RF_Linke_fin
summary(RF_Linke_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_Linke_fin)
plot(varImp(RF_Linke_fin), 20, main = "Linke_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_Linke_fin, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Linke_fin %>%
  test_roc(data = test_dfLinke) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Linke1,
                   Model2 = RF_Linke2,
                   Model3 = RF_Linke_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RF_Linke_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RF_Linke_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Linke <- RF_Linke_fin
saveRDS(besttree_Linke, "./tree_Linke.rds")

#load the model

tree_Linke <- readRDS("./tree_Linke.rds")
print(tree_Linke)





#######################
#SPD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_SPD <- data[,c(338, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_SPD$SPD_Waehler)) #181 NAs
data_SPD <- data_SPD %>% subset(data_SPD$SPD_Waehler != "NA")
data_SPD$SPD_Waehler <- as.factor(data_SPD$SPD_Waehler)


#ist die Variable unbalanced?
table(data_SPD$SPD_Waehler) #very imbalanced
max(table(data_SPD$SPD_Waehler)/sum(table(data_SPD$SPD_Waehler))) #no information rate 89%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_SPD$SPD_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSPD <- data_SPD[index,]
test_dfSPD <- data_SPD[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_SPD1 <- train(SPD_Waehler ~ ., 
                   data=train_dfSPD,
                   tuneGrid = myGrid,
                   method="ranger", 
                   metric= "ROC",
                   num.tree = 500,
                   na.action = na.omit,
                   trControl = myControl1, 
                   importance = 'impurity')

# Print models to console

RF_SPD1
summary(RF_SPD1)
plot(RF_SPD1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_SPD1, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSPD$Linke_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_SPD1 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_SPD1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RF_SPD2 <- train(SPD_Waehler ~ ., 
                   data=train_dfSPD,
                   tuneGrid = myGrid1,
                   method="ranger", 
                   metric= "ROC",
                   num.tree = 1000,
                   na.action = na.omit,
                   trControl = myControl1, 
                   importance = 'impurity')

# Print models to console

RF_SPD2
summary(RF_SPD2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_SPD2, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_SPD2 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_SPD2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_SPD_fin <- train(SPD_Waehler ~ ., 
                      data=train_dfSPD, 
                      method="ranger", metric= "ROC",
                      tuneGrid = myGrid1,
                      num.tree = 1000,
                      trControl = myControl1, 
                      na.action = na.omit,
                      importance = 'impurity')

# Print models
RF_SPD_fin
summary(RF_SPD_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_SPD_fin)
plot(varImp(RF_SPD_fin), 20, main = "SPD_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_SPD_fin, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_SPD_fin %>%
  test_roc(data = test_dfSPD) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_SPD1,
                   Model2 = RF_SPD2,
                   Model3 = RF_SPD_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RF_SPD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RF_SPD_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_SPD <- RF_SPD_fin
saveRDS(besttree_SPD, "./tree_SPD.rds")

#load the model

besttree_SPD <- readRDS("./tree_SPD.rds")
print(besttree_SPD)




#######################
#FDP: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_FDP <- data[,c(340, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_FDP$FDP_Waehler)) #181 NAs
data_FDP <- data_FDP %>% subset(data_FDP$FDP_Waehler != "NA")
data_FDP$FDP_Waehler <- as.factor(data_FDP$FDP_Waehler)


#ist die Variable unbalanced?
table(data_FDP$FDP_Waehler) #very imbalanced
max(table(data_FDP$FDP_Waehler)/sum(table(data_FDP$FDP_Waehler))) #no information rate 89%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_FDP$FDP_Waehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfFDP <- data_FDP[index,]
test_dfFDP <- data_FDP[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_FDP1 <- train(FDP_Waehler ~ ., 
                 data=train_dfFDP,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_FDP1
summary(RF_FDP1)
plot(RF_FDP1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_FDP1, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfFDP$Linke_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_FDP1 %>%
  test_roc(data = test_dfFDP) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_FDP1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RF_FDP2 <- train(FDP_Waehler ~ ., 
                 data=train_dfFDP,
                 tuneGrid = myGrid1,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_FDP2
summary(RF_FDP2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_FDP2, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_FDP2 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_FDP2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_FDP_fin <- train(FDP_Waehler ~ ., 
                    data=train_dfFDP, 
                    method="ranger", metric= "ROC",
                    tuneGrid = myGrid1,
                    num.tree = 1000,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models
RF_FDP_fin
summary(RF_FDP_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_FDP_fin)
plot(varImp(RF_FDP_fin), 20, main = "FDP_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_FDP_fin, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_FDP_fin %>%
  test_roc(data = test_dfFDP) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_FDP1,
                   Model2 = RF_FDP2,
                   Model3 = RF_FDP_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RF_FDP_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RF_FDP_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_FDP <- RF_FDP_fin
saveRDS(besttree_FDP, "./tree_FDP.rds")

#load the model

besttree_FDP <- readRDS("./tree_FDP.rds")
print(besttree_FDP)



#######################
#Nichtwähler: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_Nichtwaehler <- data[,c(343, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Nichtwaehler$Nichtwahler)) #181 NAs
data_Nichtwaehler <- data_Nichtwaehler %>% subset(data_Nichtwaehler$Nichtwahler != "NA")
data_Nichtwaehler$Nichtwahler <- as.factor(data_Nichtwaehler$Nichtwahler)


#ist die Variable unbalanced?
table(data_Nichtwaehler$Nichtwahler) #very imbalanced
max(table(data_Nichtwaehler$Nichtwahler)/sum(table(data_Nichtwaehler$Nichtwahler))) #no information rate 89%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Nichtwaehler$Nichtwahler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfNichtwahler <- data_Nichtwaehler[index,]
test_dfNichtwahler <- data_Nichtwaehler[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RF_Nichtwahler1 <- train(Nichtwahler ~ ., 
                 data=train_dfNichtwahler,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_Nichtwahler1
summary(RF_Nichtwahler1)
plot(RF_Nichtwahler1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Nichtwahler1, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfNichtwahler$Nichtwahler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Nichtwahler1 %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_Nichtwahler1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RF_Nichtwahler2 <- train(Nichtwahler ~ ., 
                 data=train_dfNichtwahler,
                 tuneGrid = myGrid1,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_Nichtwahler2
summary(RF_Nichtwahler2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Nichtwahler2, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfNichtwahler$Nichtwahler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Nichtwahler2 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RF_Nichtwahler2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_Nichtwahler_fin <- train(Nichtwahler ~ ., 
                    data=train_dfNichtwahler, 
                    method="ranger", metric= "ROC",
                    tuneGrid = myGrid1,
                    num.tree = 1000,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models
RF_Nichtwahler_fin
summary(RF_Nichtwahler_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_Nichtwahler_fin)
plot(varImp(RF_Nichtwahler_fin), 20, main = "Nichtwahler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_Nichtwahler_fin, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfNichtwahler$Nichtwahler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RF_Nichtwahler_fin %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Nichtwahler1,
                   Model2 = RF_Nichtwahler2,
                   Model3 = RF_Nichtwahler_fin)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RF_Nichtwahler_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RF_Nichtwahler_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Nichtwahler <- RF_Nichtwahler_fin
saveRDS(besttree_Nichtwahler, "./tree_Nichtwahler.rds")

#load the model

besttree_Nichtwahler <- readRDS("./tree_Nichtwahler.rds")
print(besttree_Nichtwahler)

