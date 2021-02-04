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
data_AgeRange <- data[,c(312, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_AgeRange$Age_Range)) #122 NAs
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#ist die Variable unbalanced?
table(data_AgeRange$Age_Range) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_AgeRange$Age_Range)/sum(table(data_AgeRange$Age_Range))) #no information rate 61%

#IV als Faktor:
data_AgeRange$Age_Range <- as.factor(data_AgeRange$Age_Range)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AgeRange$Age_Range, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfAgeRange <- data_AgeRange[index,]
test_dfAgeRange <- data_AgeRange[-index,]



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
RFAgeRange <- train(Age_Range ~ ., 
                       data=train_dfAgeRange,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "Kappa",
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models to console

RFAgeRange
summary(RFAgeRange)
plot(RFAgeRange)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFAgeRange, newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfAgeRange$Age_Range))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1 auc: 
RFAgeRange %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFAgeRange1 <- train(Age_Range ~ ., 
                       data=train_dfAgeRange, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid,
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl1, 
                       importance = 'impurity')

# Print models
RFAgeRange1
summary(RFAgeRange1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFAgeRange1, newdata=test_dfAgeRange)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAgeRange$Age_Range))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFAgeRange1 %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFAgeRangeFinal <- RFEinkommen_x

# Print models
RFAgeRangeFinal 
summary(RFAgeRangeFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFAgeRangeFinal )
plot(varImp(RFAgeRangeFinal ), 20, main = "Einkommensgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAgeRangeFinal , newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAgeRange$Age_Range))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRangeFinal %>%
  test_roc(data = test_dfAge_Range) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAgeRangeFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFAgeRangeFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "hohes.Alter") %>%plotPartial (main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")


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

