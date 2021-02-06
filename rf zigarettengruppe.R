
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
data_Zigarettengruppe <- data[,c(320, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Zigarettengruppe$Zigarettengruppe)) #122 NAs
data_Zigarettengruppe <- data_Zigarettengruppe %>% subset(data_Zigarettengruppe$Zigarettengruppe != "NA")


#ist die Variable unbalanced?
table(data_Zigarettengruppe$Zigarettengruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Zigarettengruppe$Zigarettengruppe)/sum(table(data_Zigarettengruppe$Zigarettengruppe))) #no information rate 61%

#IV als Faktor:
data_Zigarettengruppe$Zigarettengruppe <- as.factor(data_Zigarettengruppe$Zigarettengruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Zigarettengruppe$Zigarettengruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfZigarettengruppe <- data_Zigarettengruppe[index,]
test_dfZigarettengruppe <- data_Zigarettengruppe[-index,]


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
RFZigarettengruppe <- train(Zigarettengruppe ~ ., 
                         data=train_dfZigarettengruppe,
                         tuneGrid = myGrid,
                         method="ranger", 
                         metric= "Kappa",
                         num.tree = 500,
                         trControl = myControl1, 
                         na.action = na.omit,
                         importance = 'impurity')

# Print models to console

RFZigarettengruppe
summary(RFZigarettengruppe)
plot(RFZigarettengruppe)
#mtry = 18, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFZigarettengruppe, newdata=test_dfZigarettengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfZigarettengruppe$Zigarettengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppe %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFZigarettengruppe1 <- train(Zigarettengruppe ~ ., 
                          data=train_dfZigarettengruppe, 
                          method="ranger", metric= "Kappa",
                          tuneGrid = myGrid,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl1, 
                          importance = 'impurity')

# Print models
RFZigarettengruppe1
summary(RFZigarettengruppe1)
plot(RFZigarettengruppe1)
#mtry = 13, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFZigarettengruppe1, newdata=test_dfZigarettengruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfZigarettengruppe$Zigarettengruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppe1 %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()





####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen: grid übernehmen und num.tree anpassen

set.seed(1997)
RFZigarettengruppefinal <- RFZigarettengruppe

# Print models
RFZigarettengruppefinal
summary(RFZigarettengruppefinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFZigarettengruppefinal)
plot(varImp(RFZigarettengruppefinal), 20, main = "Zigarettengruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFZigarettengruppefinal, newdata=test_dfZigarettengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfZigarettengruppe$Zigarettengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppefinal %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFZigarettengruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFZigarettengruppefinal

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

