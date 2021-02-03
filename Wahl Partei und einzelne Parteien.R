
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


#set tuning grid

set.seed(1997)
myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees
#use metric Kappa because of categorical


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



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFPartei_2 <- train(Wahl_Partei ~ ., 
                        data=train_dfPartei, 
                        method="ranger", metric= "Kappa",
                        tuneGrid = myGrid,
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

#model auc
RFPartei_2 %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#model: xx trees performs better


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RFPartei_fin <- RFPartei_xx

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

#model AUC: 
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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
RfAfD_1 <- train(AfD_Waehler ~ ., 
                     data=train_dfAfD,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 500,
                     na.action = na.omit,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models to console

RfAfD_1
summary(RfAfD_1)
plot(RfAfD_1)
#mtry = 11, extratrees, min.node.size = 5



# predict outcome using model from train_df applied to the test_df
predictions <- predict(RfAfD_1, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RfAfD_1 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


# ROC plot
model_list <- list(Model1 = RfAfD_1)

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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 

set.seed(1997)
RfAfD_2 <- train(AfD_Waehler ~ ., 
                     data=train_dfAfD,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 1000,
                     na.action = na.omit,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models to console

RfAfD_2
summary(RfAfD_2)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RfAfD_2, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RfAfD_2 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RfAfD_2)

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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xxx trees (performs better in predicting Afd Voters)


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RFAfD_fin <- RFAfd_x

# Print models
RFAfD_fin
summary(RFAfD_fin)



#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFAfD_fin)
plot(varImp(RFAfD_fin), 20, main = "Afd_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAfD_fin, newdata=test_dfAfD)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFAfD_fin %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RfAfD_1,
                   Model2 = RfAfD_2)

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

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

imp <- importance(RFAfD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFAfD_fin

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

besttree_AfD <- RFAfD_fin
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
max(table(data_Gruen$Gruene_Waehler)/sum(table(data_Gruen$Gruene_Waehler))) #no information rate 74%%


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

#set tuning grid 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 12, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Gruene1, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGruen$Gruene_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc: 0,7591
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

# Plot ROC curve

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
RF_Gruene2 <- train(Gruene_Waehler ~ ., 
                  data=train_dfGruen,
                  tuneGrid = myGrid,
                  method="ranger", 
                  metric= "ROC",
                  num.tree = 1000,
                  na.action = na.omit,
                  trControl = myControl1, 
                  importance = 'impurity')

# Print models to console

RF_Gruene2
summary(RF_Gruene2)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Gruene2, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_Gruene2 %>%
  test_roc(data = test_dfGruen) %>%
  auc()


#ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xxx trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFGruene_fin <- RF_Gruene_x

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

#model auc
RFGruene_fin %>%
  test_roc(data = test_dfGruen) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Gruene1,
                   Model2 = RF_Gruene2)

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

#set tuning grid 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 16, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_CDU1, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfCDU$CDU_CSU_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0.6216
RF_CDU1 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RF_CDU2 <- train(CDU_CSU_Waehler ~ ., 
                    data=train_dfCDU,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RF_CDU2
summary(RF_CDU2)
#mtry = xx, extratrees, min.node.size = xx

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_CDU2, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_CDU2 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: 500 trees


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_CDU_fin <- RD_CDU_x

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
                   Model2 = RF_CDU2)

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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 13, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Linke1, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfLinke$Linke_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,7462
RF_Linke1 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#ROC plos
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

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?


set.seed(1997)
RF_Linke2 <- train(Linke_Waehler ~ ., 
                 data=train_dfLinke,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_Linke2
summary(RF_Linke2)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Linke2, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RF_Linke2 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#ROC plot
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

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees performs better to predict Linke-Wähler, even though overall acuracy is worse


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_Linke_fin <- RFLinke_x


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

#model auc
RF_Linke_fin %>%
  test_roc(data = test_dfLinke) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Linke1,
                   Model2 = RF_Linke2)

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
table(data_SPD$SPD_Waehler) # imbalanced
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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSPD$SPD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0.5921
RF_SPD1 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 

set.seed(1997)
RF_SPD2 <- train(SPD_Waehler ~ ., 
                   data=train_dfSPD,
                   tuneGrid = myGrid,
                   method="ranger", 
                   metric= "ROC",
                   num.tree = 1000,
                   na.action = na.omit,
                   trControl = myControl1, 
                   importance = 'impurity')

# Print models to console

RF_SPD2
summary(RF_SPD2)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_SPD2, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_SPD2 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


# ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RF_SPD_fin <- RF_SPD_x

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

#model1 auc: 
RF_SPD_fin %>%
  test_roc(data = test_dfSPD) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_SPD1,
                   Model2 = RF_SPD2)

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

imp <- importance(RF_SPD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

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
max(table(data_FDP$FDP_Waehler)/sum(table(data_FDP$FDP_Waehler))) #no information rate 92%%


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

#set tuning grid 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 13, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_FDP1, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfFDP$FDP_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0.6219
RF_FDP1 %>%
  test_roc(data = test_dfFDP) %>%
  auc()


# ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 
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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_FDP2, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_FDP2 %>%
  test_roc(data = test_dfFDP) %>%
  auc()


#ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#better num.trees: xx trees --> worse in accuracy but better to predict FDP


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_FDP_fin <- RF_FDP_x


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

#model auc: x
RF_FDP_fin %>%
  test_roc(data = test_dfFDP) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_FDP1,
                   Model2 = RF_FDP2)

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

imp <- importance(RF_FDP_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

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
data_Nichtwahler <- data[,c(x, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Nichtwahler$Nichtwaehler)) #181 NAs
data_Nichtwahler <- data_Nichtwahler %>% subset(data_Nichtwahler$Nichtwaehler != "NA")
data_Nichtwahler$Nichtwaehler <- as.factor(data_Nichtwahler$Nichtwaehler)


#ist die Variable unbalanced?
table(data_Nichtwahler$Nichtwaehler) #very imbalanced
max(table(data_Nichtwahler$Nichtwaehler)/sum(table(data_Nichtwahler$Nichtwaehler))) #no information rate xx%%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_FDP$Nichtwaehler, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfNichtwahler <- data_FDP[index,]
test_dfNichtwahler <- data_FDP[-index,]


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

#set tuning grid 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_Nichtwahler1 <- train(Nichtwaehler ~ ., 
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
#mtry = xx, extratrees, min.node.size = x


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Nichtwahler1, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfNichtwahler$Nichtwaehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwaehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_Nichtwahler1 %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()


# ROC plot
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

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)
RF_Nichtwahler2 <- train(Nichtwaehler ~ ., 
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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Nichtwahler2, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfNichtwahler$Nichtwaehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwaehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_Nichtwahler2 %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()


#ROC plot
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

# Plot ROC curve

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
RF_Nichtwahler_fin <- RF_Nichtwahlerx


# Print models
RF_Nichtwahler_fin
summary(RF_Nichtwahler_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RF_Nichtwahler_fin)
plot(varImp(RF_Nichtwahler_fin), 20, main = "Nichtwaehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_Nichtwahler_fin, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfNichtwahler$Nichtwaehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwaehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: x
RF_Nichtwahler_fin %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Nichtwahler1,
                   Model2 = RF_Nichtwahler2)

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

imp <- importance(RF_Nichtwahler_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

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


