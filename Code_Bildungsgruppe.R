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
#Einkommensgruppe: Categorical (3 Gruppen: hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Bildungsgruppe <- data[,c(314, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Bildungsgruppe$Bildungsgruppe)) #122 NAs
data_Bildungsgruppe <- data_Bildungsgruppe %>% subset(data_Bildungsgruppe$Bildungsgruppe != "NA")


#ist die Variable unbalanced?
table(data_Bildungsgruppe$Bildungsgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Bildungsgruppe$Bildungsgruppe)/sum(table(data_Bildungsgruppe$Bildungsgruppe))) #no information rate 61%

#IV als Faktor:
data_Bildungsgruppe$Bildungsgruppe <- as.factor(data_Bildungsgruppe$Bildungsgruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Bildungsgruppe$Bildungsgruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBildungsgruppe <- data_Bildungsgruppe[index,]
test_dfBildungsgruppe <- data_Bildungsgruppe[-index,]



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
  #sampling = "smote", 
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
RFBildungsgruppe <- train(Bildungsgruppe ~ ., 
                    data=train_dfBildungsgruppe,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFBildungsgruppe
summary(RFBildungsgruppe)
plot(RFBildungsgruppe)
#mtry = 15, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBildungsgruppe, newdata=test_dfBildungsgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBildungsgruppe$Bildungsgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1 auc: 
RFBildungsgruppe %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()

# AUC model 1 0.744

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFBildungsgruppe1 <- train(Bildungsgruppe ~ ., 
                     data=train_dfBildungsgruppe, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFBildungsgruppe1
summary(RFBildungsgruppe1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBildungsgruppe1, newdata=test_dfBildungsgruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBildungsgruppe$Bildungsgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFBildungsgruppe1 %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBildungsgruppeFinal <- RFBildungsgruppeXX

# Print models
RFBildungsgruppeFinal 
summary(RFBildungsgruppeFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFBildungsgruppeFinal )
plot(varImp(RFBildungsgruppeFinal ), 20, main = "Bildungsgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBildungsgruppeFinal , newdata=test_dfBildungsgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBildungsgruppe$Bildungsgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFBildungsgruppeFinal %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBildungsgruppeFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFBildungsgruppeFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial (main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")


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
