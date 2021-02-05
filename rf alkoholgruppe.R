
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

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Alkoholgruppe <- data[,c(318, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Alkoholgruppe$Alkoholgruppe)) #122 NAs
data_Alkoholgruppe <- data_Alkoholgruppe %>% subset(data_Alkoholgruppe$Alkoholgruppe != "NA")


#ist die Variable unbalanced?
table(data_Alkoholgruppe$Alkoholgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Alkoholgruppe$Alkoholgruppe)/sum(table(data_Alkoholgruppe$Alkoholgruppe))) #no information rate 61%

#IV als Faktor:
data_Alkoholgruppe$Alkoholgruppe <- as.factor(data_Alkoholgruppe$Alkoholgruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Alkoholgruppe$Alkoholgruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfAlkoholgruppe <- data_Alkoholgruppe[index,]
test_dfAlkoholgruppe <- data_Alkoholgruppe[-index,]


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


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size
#use metric Kappa because of unbalanced dataset

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RFAlkoholgruppe <- train(Alkoholgruppe ~ ., 
                       data=train_dfAlkoholgruppe,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "Kappa",
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models to console

RFAlkoholgruppe
summary(RFAlkoholgruppe)
plot(RFAlkoholgruppe)
#mtry = 20, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFAlkoholgruppe, newdata=test_dfAlkoholgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppe %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFAlkoholgruppe1 <- train(Alkoholgruppe ~ ., 
                       data=train_dfAlkoholgruppe, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid,
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl1, 
                       importance = 'impurity')

# Print models
RFAlkoholgruppe1
summary(RFAlkoholgruppe1)
plot(RFAlkoholgruppe1)
#mtry = 20, extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFAlkoholgruppe1, newdata=test_dfAlkoholgruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppe1 %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()





####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen: grid übernehmen und num.tree anpassen

set.seed(1997)
RFAlkoholgruppefinal <- RFAlkoholgruppe1

# Print models
RFAlkoholgruppefinal
summary(RFAlkoholgruppefinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFAlkoholgruppefinal)
plot(varImp(RFAlkoholgruppefinal), 20, main = "Alkoholgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAlkoholgruppefinal, newdata=test_dfAlkoholgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppefinal %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAlkoholgruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFAlkoholgruppefinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main= "Niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main= "Hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Einkommen <- RFEinkommen_fin
saveRDS(besttree_Einkommen, "./tree_Einkommen.rds")

#load the model

besttree_Einkommen <- readRDS("./tree_Einkommen.rds")
print(besttree_Einkommen)

