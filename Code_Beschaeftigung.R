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


#######################
#Religion: Categorical (7 Gruppen: Arbeitslos/-suchend, Auszubildende/r, Berufstätige/r, Hausfrau/-mann, Rentner/in, Schüler/in, Student/in)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Beschaeftigung <- data[,c(10, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Beschaeftigung$Beschaeftigung)) #122 NAs
data_Beschaeftigung <- data_Beschaeftigung %>% subset(data_Beschaeftigung$Beschaeftigung != "NA")


#ist die Variable unbalanced?
table(data_Beschaeftigung$Beschaeftigung) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Beschaeftigung$Beschaeftigung)/sum(table(data_Beschaeftigung$Beschaeftigung))) #no information rate 61%

#IV als Faktor:
data_Beschaeftigung$Beschaeftigung <- as.factor(data_Beschaeftigung$Beschaeftigung)

#Variablennamen anpassen für Analyse
data_Beschaeftigung <- data_Beschaeftigung %>% mutate(Beschaeftigung = case_when(Beschaeftigung == "Arbeitslos/-suchend" ~ 'Arbeitslos_suchend',
                                                                                 Beschaeftigung == "Auszubildende/r" ~ 'Auszubildende_r',
                                                                                 Beschaeftigung == "Berufstätige/r" ~ 'Berufstätige_r',
                                                                                 Beschaeftigung == "Hausfrau/-mann" ~ 'Hausfrau_mann',
                                                                                 Beschaeftigung == "Renter/in" ~ 'Renter_in',
                                                                                 Beschaeftigung == "Schüler/in" ~ 'Schüler_in',
                                                                                 Beschaeftigung == "Student/in" ~ 'Student_in'))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Beschaeftigung$Beschaeftigung, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBeschaeftigung <- data_Beschaeftigung[index,]
test_dfBeschaeftigung <- data_Beschaeftigung[-index,]



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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees
#use metric Kappa because of unbalanced dataset

#set random seed again 

set.seed(1997)
RFBeschaeftigung <- train(Beschaeftigung ~ ., 
                    data=train_dfBeschaeftigung,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFBeschaeftigung
summary(RFBeschaeftigung)
plot(RFBeschaeftigung)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBeschaeftigung, newdata=test_dfBeschaeftigung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBeschaeftigung$Beschaeftigung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model1 auc: 
RFBeschaeftigung %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFBeschaeftigung1 <- train(Beschaeftigung ~ ., 
                     data=train_dfBeschaeftigung, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFBeschaeftigung1
summary(RFBeschaeftigung1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBeschaeftigung1, newdata=test_dfBeschaeftigung)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBeschaeftigung$Beschaeftigung))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model auc: 
RFBeschaeftigung1 %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBeschaeftigungFinal <- RFBeschaeftigung1

# Print models
RFBeschaeftigungFinal 
summary(RFBeschaeftigungFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFBeschaeftigungFinal )
plot(varImp(RFBeschaeftigungFinal ), 20, main = "Beschaeftigung")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBeschaeftigungFinal , newdata=test_dfBeschaeftigung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBeschaeftigung$Beschaeftigung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model auc: 
RFBeschaeftigungFinal %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBeschaeftigungFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFBeschaeftigungFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Arbeitslos_suchend") %>%plotPartial (main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Student_in") %>%plotPartial(main = "Student_in")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Einkommen <- RFEinkommen_fin
saveRDS(besttree_Einkommen, "./tree_Einkommen.rds")

#load the model

besttree_Einkommen <- readRDS("./tree_Einkommen.rds")
print(besttree_Einkommen)





#######################
#über oder unter Durchschnittseinkommen (2000€): binär
######################
