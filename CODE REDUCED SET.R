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
install.packages("naniar")


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
library(naniar)


options(max.print = 100000)


#------------------------Analysis Reduced Dataset------------------------------

# load data 

load("data_reduced.RData")

data <- reduced_set

cols_names <- names(data)  
cols_names



#define tuning grid
myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))





#######################
#Alter: Age Ranges Categorical (hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_AgeRange <- data[,c(312, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_AgeRange$Age_Range))
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#ist die Variable unbalanced?
table(data_AgeRange$Age_Range) 
max(table(data_AgeRange$Age_Range)/sum(table(data_AgeRange$Age_Range)))

#IV als Faktor:
data_AgeRange$Age_Range <- as.factor(data_AgeRange$Age_Range)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AgeRange$Age_Range, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

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


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 13, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFAgeRange, newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfAgeRange$Age_Range))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRange %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()

#AUC 0.7889
#Accuracy 0.6554


#save model to disk 

tree500_AgeRange <- RFAgeRange
saveRDS(tree500_AgeRange, "./tree_500AgeRange.rds")



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
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRange1 %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()


#save model to disk 

tree1000_AgeRange <- RFAgeRange1
saveRDS(tree1000_AgeRange, "./tree1000_AgeRange.rds")


#model: 500 trees performs better
#AUC

####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFAgeRangeFinal <- RFAgeRange1

# Print models
RFAgeRangeFinal 
summary(RFAgeRangeFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFAgeRangeFinal )
plot(varImp(RFAgeRangeFinal ), 20, main = "Age_Range")


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
  test_roc(data = test_dfAgeRange) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAgeRangeFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

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

besttree_AgeRange <- RFAgeRangeFinal
saveRDS(besttree_AgeRange, "./besttree_AgeRange.rds")

#load the model

besttree_AgeRange <- readRDS("./besttree_AgeRange.rds")
print(besttree_AgeRange)


#---------------------------------------------------------------------------------------------------

#######################
#Alter: numerisch
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Gibt es NAs in der DV?
sum(is.na(data_Alter$Alter)) #keine NAs

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds ; here no sampling
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(667)
modelAlterRF <- train(Alter ~ ., 
                      data=train_dfAlter,
                      tuneGrid = myGrid,
                      method="ranger",
                      metric= "RMSE",  
                      na.action = na.omit,
                      num.tree = 500,
                      trControl = myControl, 
                      importance = 'impurity')

# Print model to console

modelAlterRF
summary(modelAlterRF)
plot(modelAlterRF)


#best mtry = 14, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterRF

spearmanAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterRF

#save model to disk 

tree500_Alter <- modelAlterRF
saveRDS(tree500_Alter, "./tree500_Alter.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAlterRF1 <- train(Alter ~ ., 
                       data=train_dfAlter,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "RMSE", 
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelAlterRF1
summary(modelAlterRF1)
plot(modelAlterRF1)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF1, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlter1

spearmanAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlter1

#save model to disk 

tree1000_Alter <- modelAlterRF1
saveRDS(tree1000_Alter, "./tree1000_Alter.rds")

#fit model with num.trees = xx trees (better performance)


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelAlterfinal <- modelAlterRF1

# Print model
print(modelAlterfinal)

#output in terms of regression coefficients
summary(modelAlterfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
varImp(modelAlterfinal)
plot(varImp(modelAlterfinal), 20, main = "Alter")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlterfinal, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterfinal

spearmanAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables


imp <- importance(modelAlterfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAlterfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Alter")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Alter <- modelAlterfinal
saveRDS(besttree_Alter, "./besttree_Alter.rds")

#load the model

besttree_Alter <- readRDS("./besttree_Alter.rds")
print(besttree_Alter)




##################
#Geschlecht: binär
##################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_GeschlechtMW <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich))  
data_GeschlechtMW <- data_GeschlechtMW %>% subset(data_GeschlechtMW$weiblich_maennlich != "NA")


#ist die Variable unbalanced?
table(data_GeschlechtMW$weiblich_maennlich) #Verteilung in Ordnung
max(table(data_GeschlechtMW$weiblich_maennlich)/sum(table(data_GeschlechtMW$weiblich_maennlich)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_GeschlechtMW$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfGeschlechtMW <- data_GeschlechtMW[index,]
test_dfGeschlechtMW <- data_GeschlechtMW[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "all",
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlechtMW,
                           tuneGrid = myGrid,
                           method="ranger",
                           metric= "ROC",
                           na.action = na.omit,
                           num.tree = 500,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console

modelGeschlechtRF
summary(modelGeschlechtRF)
plot(modelGeschlechtRF)

#best mtry = xx, splitrule = xx, min.node.size = xx

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

modelGeschlechtRF %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelGeschlechtRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

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

#save model to disk 

tree500_Geschlecht <- modelGeschlechtRF
saveRDS(tree500_Geschlecht, "./tree500_Geschlecht.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelGeschlechtRF1 <- train(weiblich_maennlich ~ ., 
                            data=train_dfGeschlechtMW,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "ROC", 
                            na.action = na.omit,
                            num.tree = 1000,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelGeschlechtRF1
summary(modelGeschlechtRF1)
plot(modelGeschlechtRF1)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelGeschlechtRF1, newdata=test_dfGeschlechtMW)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

modelGeschlechtRF1 %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelGeschlechtRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

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

custom_col <- c("#CC79A7", "#000000", "#009E73", "#0072B2", "#D55E00")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Geschlecht <- modelGeschlechtRF1
saveRDS(tree1000_Geschlecht, "./tree1000_Geschlecht.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

modelGeschlechtFinal <- modelGeschlechtRF1

# Print model
print(modelGeschlechtFinal)

#output in terms of regression coefficients
summary(modelGeschlechtFinal)

#evaluate variable importance 
varImp(modelGeschlechtFinal)
plot(varImp(modelGeschlechtFinal), 20, main = "weiblich_maennlich")

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelGeschlechtFinal, newdata=test_dfGeschlechtMW)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

#model auc: 
modelGeschlechtFinal %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC plots
model_list <- list(ModelFinal = modelGeschlechtFinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

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




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelGeschlechtFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelGeschlechtFinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "weiblich") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Geschlecht <- modelGeschlechtFinal
saveRDS(besttree_Geschlecht, "./besttree_Geschlecht.rds")

#load the model

besttree_Geschlecht <- readRDS("./besttree_Geschlecht.rds")
print(besttree_Geschlecht)



################
#Ost-West: binär
################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Ost_West <- data[,c(311, 27:255)]

data_Ost_West$Ost_West <- as.factor(data_Ost_West$Ost_West)

#Gibt es NAs in der DV?
sum(is.na(data_Ost_West$Ost_West))
data_Ost_West <- data_Ost_West %>% subset(data_Ost_West$Ost_West != "NA")


#ist die Variable unbalanced?
table(data_Ost_West$Ost_West)  
max(table(data_Ost_West$Ost_West)/sum(table(data_Ost_West$Ost_West)))



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Ost_West$Ost_West, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfOst_West <- data_Ost_West[index,]
test_dfOst_West <- data_Ost_West[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)

####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelOst_West <- train(Ost_West ~ ., 
                       data=train_dfOst_West,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelOst_West
summary(modelOst_West)
plot(modelOst_West)
#best mtry =13, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_West, newdata=test_dfOst_West)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_West %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelOst_West)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

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

#save model to disk 

tree500_OstWest <- modelOst_West
saveRDS(tree500_OstWest, "./tree500_OstWest.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelOst_West1 <- train(Ost_West ~ ., 
                        data=train_dfOst_West,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelOst_West1
summary(modelOst_West1)
plot(modelOst_West1)
#best mtry =11, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_West1, newdata=test_dfOst_West)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)



#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_West1 %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOst_West1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

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


#save model to disk 

tree1000_Ost_West <- modelOst_West1
saveRDS(tree1000_Ost_West, "./tree1000_OstWest.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelOst_Westfinal <- modelOst_West1

# Print model
print(modelOst_Westfinal)

#output in terms of regression coefficients
summary(modelOst_Westfinal)

#evaluate variable importance 
varImp(modelOst_Westfinal)
plot(varImp(modelOst_Westfinal), 20, main = "Ost_West")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_Westfinal, newdata=test_dfOst_West)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_Westfinal %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelOst_Westfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOst_Westfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOst_Westfinal

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

besttree_OstWest <- modelOst_Westfinal
saveRDS(besttree_OstWest, "./besttree_OstWest.rds")

#load the model

besttree_OstWest <- readRDS("./besttree_OstWest.rds")
print(besttree_OstWest)




################
#Extraversion1: numerisch
################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Extraversion<- data[,c(295, 27:255)]

data_Extraversion$Extraversion <- as.numeric(data_Extraversion$Extraversion)

#Gibt es NAs in der DV?
sum(is.na(data_Extraversion$Extraversion))  
data_Extraversion <- data_Extraversion%>% filter(Extraversion != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Extraversion$Extraversion, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfExtraversion <- data_Extraversion[index,]
test_dfExtraversion <- data_Extraversion[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelExtraversionRF <- train(Extraversion ~ ., 
                             data=train_dfExtraversion,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "RMSE",  
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelExtraversionRF
summary(modelExtraversionRF)
plot(modelExtraversionRF)


#best mtry = 10, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionRF, newdata=test_dfExtraversion)

#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionRF

spearmanExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionRF

#save model to disk 

tree500_Extraversion1 <- modelExtraversionRF
saveRDS(tree500_Extraversion1, "./tree500_Extraversion1.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelExtraversionRF1 <- train(Extraversion ~ ., 
                              data=train_dfExtraversion,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelExtraversionRF1
summary(modelExtraversionRF1)
plot(modelExtraversionRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionRF1, newdata=test_dfExtraversion)


#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values

pearsonExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversion1

spearmanExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversion1


#save model to disk 

tree1000_Extraversion1 <- modelExtraversionRF1
saveRDS(tree1000_Extraversion1, "./tree1000_Extraversion1.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#finales Model definieren
set.seed(1997)
modelExtraversionfinal <- modelExtraversionRF1

# Print model
print(modelExtraversionfinal)

#output in terms of regression coefficients
summary(modelExtraversionfinal)

#evaluate variable importance 
varImp(modelExtraversionfinal)
plot(varImp(modelExtraversionfinal), 20, main = "Extraversion")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionfinal, newdata=test_dfExtraversion)

#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionfinal

spearmanExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelExtraversionfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelExtraversionfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Extraversion")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Extraversion1 <- modelExtraversionfinal
saveRDS(besttree_Extraversion1, "./besttree_Extraversion1.rds")

#load the model

besttree_Extraversion1 <- readRDS("./besttree_Extraversion1.rds")
print(besttree_Extraversion1)




###############
#Extraversion 2: binary
##############

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Extraversion2 <- data[,c(300, 27:255)]

data_Extraversion2$Extraversion2 <- as.factor(data_Extraversion2$Extraversion2)

#Gibt es NAs in der DV?
sum(is.na(data_Extraversion2$Extraversion2)) 
data_Extraversion2 <- data_Extraversion2 %>% subset(data_Extraversion2$Extraversion2 != "NA")


#ist die Variable unbalanced?
table(data_Extraversion2$Extraversion2) 
max(table(data_Extraversion2$Extraversion2)/sum(table(data_Extraversion2$Extraversion2))) 



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Extraversion2$Extraversion2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfExtraversion2 <- data_Extraversion2[index,]
test_dfExtraversion2 <- data_Extraversion2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelExtraversion2 <- train(Extraversion2 ~ ., 
                            data=train_dfExtraversion2,
                            tuneGrid = myGrid,
                            method="ranger",
                            metric= "ROC",
                            na.action = na.omit,
                            num.tree = 500,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelExtraversion2
summary(modelExtraversion2)
plot(modelExtraversion2)
#best mtry = 10, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2, newdata=test_dfExtraversion2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)



#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2 %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelExtraversion2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

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

#save model to disk 

tree500_Extraversion2 <- modelExtraversion2
saveRDS(tree500_Extraversion2, "./tree500_Extraversion2.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelExtraversion2_1 <- train(Extraversion2 ~ ., 
                              data=train_dfExtraversion2,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "ROC", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelExtraversion2_1
summary(modelExtraversion2_1)
plot(modelExtraversion2_1)
#best mtry = 10, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2_1, newdata=test_dfExtraversion2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2_1 %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelExtraversion2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

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

#save model to disk 

tree1000_Extraversion2 <- modelExtraversion2_1
saveRDS(tree1000_Extraversion2, "./tree1000_Extraversion2.rds")


####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelExtraversion2final <- modelExtraversion2

# Print model
print(modelExtraversion2final)

#output in terms of regression coefficients
summary(modelExtraversion2final)

#evaluate variable importance 
varImp(modelExtraversion2final)
plot(varImp(modelExtraversion2final), 20, main = "Extraversion2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2final, newdata=test_dfExtraversion2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2final %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelExtraversion2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

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

# Plot ROC
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables


imp <- importance(modelExtraversion2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelExtraversion2final

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

besttree_Extraversion2 <- modelExtraversion2final
saveRDS(besttree_Extraversion2, "./besttree_Extraversion2.rds")

#load the model

besttree_Extraversion2 <- readRDS("./besttree_Extraversion2.rds")
print(besttree_Extraversion2)



#####################
#Agreeableness 1: numeric
####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Agreeableness<- data[,c(296, 27:255)]

data_Agreeableness$Agreeableness <- as.numeric(data_Agreeableness$Agreeableness)

#Gibt es NAs in der DV?
sum(is.na(data_Agreeableness$Agreeableness))  
data_Agreeableness <- data_Agreeableness%>% filter(Agreeableness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

index <- createDataPartition(data_Agreeableness$Agreeableness, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAgreeableness <- data_Agreeableness[index,]
test_dfAgreeableness <- data_Agreeableness[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelAgreeablenessRF <- train(Agreeableness ~ ., 
                              data=train_dfAgreeableness,
                              tuneGrid = myGrid,
                              method="ranger",
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 500,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelAgreeablenessRF
summary(modelAgreeablenessRF)
plot(modelAgreeablenessRF)
#best mtry = 11, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessRF, newdata=test_dfAgreeableness)

#check performance measures
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessRF

spearmanAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessRF

#save model to disk 

tree500_Agreeableness1 <- modelAgreeablenessRF
saveRDS(tree500_Agreeableness1, "./tree500_Agreeableness1.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAgreeablenessRF1 <- train(Agreeableness ~ ., 
                               data=train_dfAgreeableness,
                               tuneGrid = myGrid,
                               method="ranger", 
                               metric= "RMSE", 
                               na.action = na.omit,
                               num.tree = 1000,
                               trControl = myControl, 
                               importance = 'impurity')

# Print model to console

modelAgreeablenessRF1
summary(modelAgreeablenessRF1)
plot(modelAgreeablenessRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessRF1, newdata=test_dfAgreeableness)


#check performance measures  
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeableness1

spearmanAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeableness1

#save model to disk 

tree1000_Agreeableness1 <- modelAgreeablenessRF1
saveRDS(tree1000_Agreeableness1, "./tree1000_Agreeableness1.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
modelAgreeablenessfinal <- modelAgreeablenessRF

# Print model
print(modelAgreeablenessfinal)

#output in terms of regression coefficients
summary(modelAgreeablenessfinal)

#evaluate variable importance 
varImp(modelAgreeablenessfinal)
plot(varImp(modelAgreeablenessfinal), 20, main = "Agreeableness")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessfinal, newdata=test_dfAgreeableness)


#check performance measures
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessfinal

spearmanAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAgreeablenessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAgreeablenessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Agreeableness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Agreeableness1 <- modelAgreeablenessfinal
saveRDS(besttree_Agreeableness1, "./besttree_Agreeableness1.rds")

#load the model

besttree_Agreeableness1 <- readRDS("./besttree_Agreeableness1.rds")
print(besttree_Agreeableness1)




#####################
#Agreeableness2: binary
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Agreeableness2 <- data[,c(301, 27:255)]

data_Agreeableness2$Agreeableness2 <- as.factor(data_Agreeableness2$Agreeableness2)

#Gibt es NAs in der DV?
sum(is.na(data_Agreeableness2$Agreeableness2))  
data_Agreeableness2 <- data_Agreeableness2 %>% subset(data_Agreeableness2$Agreeableness2 != "NA")


#ist die Variable unbalanced?
table(data_Agreeableness2$Agreeableness2)
max(table(data_Agreeableness2$Agreeableness2)/sum(table(data_Agreeableness2$Agreeableness2)))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Agreeableness2$Agreeableness2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAgreeableness2 <- data_Agreeableness2[index,]
test_dfAgreeableness2 <- data_Agreeableness2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelAgreeableness2 <- train(Agreeableness2 ~ ., 
                             data=train_dfAgreeableness2,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "ROC",
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelAgreeableness2
summary(modelAgreeableness2)
plot(modelAgreeableness2)
#best mtry = 18, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2, newdata=test_dfAgreeableness2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2 %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelAgreeableness2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

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


#save model to disk 

tree500_Agreeableness2 <- modelAgreeableness2
saveRDS(tree500_Agreeableness2, "./tree500_Agreeableness2.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelAgreeableness2_1 <- train(Agreeableness2 ~ ., 
                               data=train_dfAgreeableness2,
                               tuneGrid = myGrid,
                               method="ranger", 
                               metric= "ROC", 
                               na.action = na.omit,
                               num.tree = 1000,
                               trControl = myControl, 
                               importance = 'impurity')

# Print model to console

modelAgreeableness2_1
summary(modelAgreeableness2_1)
plot(modelAgreeableness2_1)
#best mtry = 18, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2_1, newdata=test_dfAgreeableness2)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2_1 %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelAgreeableness2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

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


#save model to disk 

tree1000_Agreeableness2 <- modelAgreeableness2_1
saveRDS(tree1000_Agreeableness2, "./tree1000_Agreeableness2.rds")



####-------tree 3: Final --------------------------------------------------

#final Model definieren
set.seed(1997)
modelAgreeableness2final <- modelAgreeableness2

# Print model
print(modelAgreeableness2final)

#output in terms of regression coefficients
summary(modelAgreeableness2final)

#evaluate variable importance 
varImp(modelAgreeableness2final)
plot(varImp(modelAgreeableness2final), 20, main = "Agreeableness2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2final, newdata=test_dfAgreeableness2)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2final %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAgreeableness2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAgreeableness2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAgreeableness2final

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

besttree_Agreeableness2 <- modelAgreeableness2final
saveRDS(besttree_Agreeableness2, "./besttree_Agreeableness2.rds")

#load the model

besttree_Agreeableness2 <- readRDS("./besttree_Agreeableness2.rds")
print(besttree_Agreeableness2)




###################
#Conscentiousness1: numeric
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Conscientiousness<- data[,c(297, 27:255)]

data_Conscientiousness$Conscientiousness <- as.numeric(data_Conscientiousness$Conscientiousness)

#Gibt es NAs in der DV?
sum(is.na(data_Conscientiousness$Conscientiousness))  
data_Conscientiousness <- data_Conscientiousness%>% filter(Conscientiousness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Conscientiousness$Conscientiousness, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfConscientiousness <- data_Conscientiousness[index,]
test_dfConscientiousness <- data_Conscientiousness[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelConscientiousnessRF <- train(Conscientiousness ~ ., 
                                  data=train_dfConscientiousness,
                                  tuneGrid = myGrid,
                                  method="ranger",
                                  metric= "RMSE",  
                                  na.action = na.omit,
                                  num.tree = 500,
                                  trControl = myControl, 
                                  importance = 'impurity')

# Print model to console

modelConscientiousnessRF
summary(modelConscientiousnessRF)
plot(modelConscientiousnessRF)
#best mtry = 10, splitrule = extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessRF, newdata=test_dfConscientiousness)

#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessRF

spearmanConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessRF


#save model to disk 

tree500_Conscentiousness1 <- modelConscientiousnessRF
saveRDS(tree500_Conscentiousness1, "./tree500_Conscentiousness1.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelConscientiousnessRF1 <- train(Conscientiousness ~ ., 
                                   data=train_dfConscientiousness,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "RMSE", 
                                   na.action = na.omit,
                                   num.tree = 1000,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console
modelConscientiousnessRF1
summary(modelConscientiousnessRF1)
plot(modelConscientiousnessRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessRF1, newdata=test_dfConscientiousness)

#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousness1

spearmanConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousness1


#save model to disk 

tree1000_Conscentiousness1 <- modelConscientiousnessRF1
saveRDS(tree1000_Conscentiousness1, "./tree1000_Conscentiousness1.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#define final model
set.seed(1997)
modelConscientiousnessfinal <- modelConscientiousnessRF

# Print model
print(modelConscientiousnessfinal)

#output in terms of regression coefficients
summary(modelConscientiousnessfinal)

#evaluate variable importance 
varImp(modelConscientiousnessfinal)
plot(varImp(modelConscientiousnessfinal), 20, main = "Conscientiousness")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessfinal, newdata=test_dfConscientiousness)


#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessfinal

spearmanConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelConscientiousnessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelConscientiousnessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Conscientiousness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Conscentiousness1 <- modelConscientiousnessfinal
saveRDS(besttree_Conscentiousness1, "./besttree_Conscentiousness1.rds")

#load the model

besttree_Conscentiousness1 <- readRDS("./besttree_Conscentiousness1.rds")
print(besttree_Conscentiousness1)




#######################
#Conscentiousness2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Conscientiousness2 <- data[,c(302, 27:255)]

data_Conscientiousness2$Conscientiousness2 <- as.factor(data_Conscientiousness2$Conscientiousness2)

#Gibt es NAs in der DV?
sum(is.na(data_Conscientiousness2$Conscientiousness2))  
data_Conscientiousness2 <- data_Conscientiousness2 %>% subset(data_Conscientiousness2$Conscientiousness2 != "NA")


#ist die Variable unbalanced?
table(data_Conscientiousness2$Conscientiousness2) 
max(table(data_Conscientiousness2$Conscientiousness2)/sum(table(data_Conscientiousness2$Conscientiousness2)))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Conscientiousness2$Conscientiousness2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfConscientiousness2 <- data_Conscientiousness2[index,]
test_dfConscientiousness2 <- data_Conscientiousness2[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelConscientiousness2 <- train(Conscientiousness2 ~ ., 
                                 data=train_dfConscientiousness2,
                                 tuneGrid = myGrid,
                                 method="ranger",
                                 metric= "ROC",  
                                 na.action = na.omit,
                                 num.tree = 500,
                                 trControl = myControl, 
                                 importance = 'impurity')

# Print model to console

modelConscientiousness2
summary(modelConscientiousness2)
plot(modelConscientiousness2)
#best mtry = 20, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2, newdata=test_dfConscientiousness2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2 %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

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


#save model to disk 

tree500_Conscentiousness2 <- modelConscientiousness2
saveRDS(tree500_Conscentiousness2, "./tree500_Conscentiousness2.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelConscientiousness2_1 <- train(Conscientiousness2 ~ ., 
                                   data=train_dfConscientiousness2,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC", 
                                   na.action = na.omit,
                                   num.tree = 1000,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console
modelConscientiousness2_1
summary(modelConscientiousness2_1)
plot(modelConscientiousness2_1)
#best mtry = 20, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2_1, newdata=test_dfConscientiousness2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2_1 %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

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

#save model to disk 

tree1000_Conscentiousness2 <- modelConscientiousness2_1
saveRDS(tree1000_Conscentiousness2, "./tree1000_Conscentiousness2.rds")



####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelConscientiousness2final <- modelConscientiousness2

# Print model
print(modelConscientiousness2final)

#output in terms of regression coefficients
summary(modelConscientiousness2final)

#evaluate variable importance 
varImp(modelConscientiousness2final)
plot(varImp(modelConscientiousness2final), 20, main = "Conscientiousness2")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2final, newdata=test_dfConscientiousness2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)

#check for AUC 

test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2final %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelConscientiousness2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelConscientiousness2final

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

besttree_Conscentiousness2 <- modelConscientiousness2final
saveRDS(besttree_Conscentiousness2, "./besttree_Conscentiousness2.rds")

#load the model

besttree_Conscentiousness2 <- readRDS("./besttree_Conscentiousness2.rds")
print(besttree_Conscentiousness2)



###################
#Emotional Stability1: numeric
###################
#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Emotional_stablity<- data[,c(298, 27:255)]

data_Emotional_stablity$Emotional_stablity <- as.numeric(data_Emotional_stablity$Emotional_stablity)

#Gibt es NAs in der DV?
sum(is.na(data_Emotional_stablity$Emotional_stablity))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Emotional_stablity$Emotional_stablity, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfEmotional_stablity <- data_Emotional_stablity[index,]
test_dfEmotional_stablity <- data_Emotional_stablity[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelEmotional_stablityRF <- train(Emotional_stablity ~ ., 
                                   data=train_dfEmotional_stablity,
                                   tuneGrid = myGrid,
                                   method="ranger",
                                   metric= "RMSE",  
                                   na.action = na.omit,
                                   num.tree = 500,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console

modelEmotional_stablityRF
summary(modelEmotional_stablityRF)
plot(modelEmotional_stablityRF)
#best mtry = 10, splitrule = extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityRF, newdata=test_dfEmotional_stablity)

#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityRF

spearmanEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityRF

#save model to disk 

tree500_EmotionalStability <- modelEmotional_stablityRF
saveRDS(tree500_EmotionalStability, "./tree500_EmotionalStability.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelEmotional_stablityRF1 <- train(Emotional_stablity ~ ., 
                                    data=train_dfEmotional_stablity,
                                    tuneGrid = myGrid,
                                    method="ranger", 
                                    metric= "RMSE", 
                                    na.action = na.omit,
                                    num.tree = 1000,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelEmotional_stablityRF1
summary(modelEmotional_stablityRF1)
plot(modelEmotional_stablityRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityRF1, newdata=test_dfEmotional_stablity)

#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablity1

spearmanEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablity1

#save model to disk 

tree1000_EmotionalStability <- modelEmotional_stablityRF1
saveRDS(tree1000_EmotionalStability, "./tree1000_EmotionalStability.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelEmotional_stablityfinal <- modelEmotional_stablityRF

# Print model
print(modelEmotional_stablityfinal)

#output in terms of regression coefficients
summary(modelEmotional_stablityfinal)

#evaluate variable importance 
varImp(modelEmotional_stablityfinal)
plot(varImp(modelEmotional_stablityfinal), 20, main = "Emotional_stablity")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityfinal, newdata=test_dfEmotional_stablity)


#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityfinal

spearmanEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelEmotional_stablityfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelEmotional_stablityfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Emotional_stablity")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_EmotionalStability <- modelEmotional_stablityfinal
saveRDS(besttree_EmotionalStability, "./besttree_EmotionalStability.rds")

#load the model

besttree_EmotionalStability <- readRDS("./besttree_EmotionalStability.rds")
print(besttree_EmotionalStability)




######################
#Emotional Stability2: binary
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Emotional_stablity2 <- data[,c(303, 27:255)]

data_Emotional_stablity2$Emotional_stablity2 <- as.factor(data_Emotional_stablity2$Emotional_stablity2)

#Gibt es NAs in der DV?
sum(is.na(data_Emotional_stablity2$Emotional_stablity2))
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Emotional_stablity2 <- data_Emotional_stablity2 %>% subset(data_Emotional_stablity2$Emotional_stablity2 != "NA")


#ist die Variable unbalanced?
table(data_Emotional_stablity2$Emotional_stablity2)  
max(table(data_Emotional_stablity2$Emotional_stablity2)/sum(table(data_Emotional_stablity2$Emotional_stablity2)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Emotional_stablity2$Emotional_stablity2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfEmotional_stablity2 <- data_Emotional_stablity2[index,]
test_dfEmotional_stablity2 <- data_Emotional_stablity2[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelEmotional_stablity2 <- train(Emotional_stablity2 ~ ., 
                                  data=train_dfEmotional_stablity2,
                                  tuneGrid = myGrid,
                                  method="ranger",
                                  metric= "ROC",  
                                  na.action = na.omit,
                                  num.tree = 500,
                                  trControl = myControl, 
                                  importance = 'impurity')

# Print model to console
modelEmotional_stablity2
summary(modelEmotional_stablity2)
plot(modelEmotional_stablity2)
#best mtry = 17, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2, newdata=test_dfEmotional_stablity2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2 %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelEmotional_stablity2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

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

#save model to disk 

tree500_EmotionalStability2 <- modelEmotional_stablity2
saveRDS(tree500_EmotionalStability2, "./tree500_EmotionalStability2.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelEmotional_stablity2_1 <- train(Emotional_stablity2 ~ ., 
                                    data=train_dfEmotional_stablity2,
                                    tuneGrid = myGrid,
                                    method="ranger", 
                                    metric= "ROC", 
                                    na.action = na.omit,
                                    num.tree = 1000,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelEmotional_stablity2_1
summary(modelEmotional_stablity2_1)
plot(modelEmotional_stablity2_1)
#best mtry = 17, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2_1, newdata=test_dfEmotional_stablity2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2_1 %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

###nur für binär
#ROC plot
model_list <- list(M1 = modelEmotional_stablity2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

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

#save model to disk 

tree1000_EmotionalStability2 <- modelEmotional_stablity2_1
saveRDS(tree1000_EmotionalStability2, "./tree1000_EmotionalStability2.rds")



####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelEmotional_stablity2final <- modelEmotional_stablity2_1

# Print model
print(modelEmotional_stablity2final)

#output in terms of regression coefficients
summary(modelEmotional_stablity2final)

#evaluate variable importance 
varImp(modelEmotional_stablity2final)
plot(varImp(modelEmotional_stablity2final), 20, main = "Emotional_stablity2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2final, newdata=test_dfEmotional_stablity2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2final %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#ROC plot
model_list <- list(M1 = modelEmotional_stablity2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelEmotional_stablity2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelEmotional_stablity2final

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Stable") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_EmotionalStability2 <- modelEmotional_stablity2final
saveRDS(besttree_EmotionalStability2, "./besttree_EmotionalStability2.rds")

#load the model

besttree_EmotionalStability2 <- readRDS("./besttree_EmotionalStability2.rds")
print(besttree_EmotionalStability2)




#####################
#Openness to Experiences1: numeric
####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Openness_to_Experiences<- data[,c(299, 27:255)]

data_Openness_to_Experiences$Openness_to_Experiences <- as.numeric(data_Openness_to_Experiences$Openness_to_Experiences)

#Gibt es NAs in der DV?
sum(is.na(data_Openness_to_Experiences$Openness_to_Experiences))  
data_Openness_to_Experiences <- data_Openness_to_Experiences%>% filter(Openness_to_Experiences != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Openness_to_Experiences$Openness_to_Experiences, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfOpenness_to_Experiences <- data_Openness_to_Experiences[index,]
test_dfOpenness_to_Experiences <- data_Openness_to_Experiences[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelOpenness_to_ExperiencesRF <- train(Openness_to_Experiences ~ ., 
                                        data=train_dfOpenness_to_Experiences,
                                        tuneGrid = myGrid,
                                        method="ranger",
                                        metric= "RMSE",  
                                        na.action = na.omit,
                                        num.tree = 500,
                                        trControl = myControl, 
                                        importance = 'impurity')

# Print model to console
modelOpenness_to_ExperiencesRF
summary(modelOpenness_to_ExperiencesRF)
plot(modelOpenness_to_ExperiencesRF)
#best mtry = 11, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_ExperiencesRF, newdata=test_dfOpenness_to_Experiences)

#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_ExperiencesRF <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_ExperiencesRF

spearmanOpenness_to_ExperiencesRF <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_ExperiencesRF


#save model to disk 

tree500_OpennessToExperiences1 <- modelOpenness_to_ExperiencesRF
saveRDS(tree500_OpennessToExperiences1, "./tree500_OpennessToExperiences1.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelOpenness_to_ExperiencesRF1 <- train(Openness_to_Experiences ~ ., 
                                         data=train_dfOpenness_to_Experiences,
                                         tuneGrid = myGrid,
                                         method="ranger", 
                                         metric= "RMSE", 
                                         na.action = na.omit,
                                         num.tree = 1000,
                                         trControl = myControl, 
                                         importance = 'impurity')

# Print model to console
modelOpenness_to_ExperiencesRF1
summary(modelOpenness_to_ExperiencesRF1)
plot(modelOpenness_to_ExperiencesRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_ExperiencesRF1, newdata=test_dfOpenness_to_Experiences)


#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_Experiences1 <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_Experiences1

spearmanOpenness_to_Experiences1 <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_Experiences1

#save model to disk 

tree1000_OpennessToExperiences1 <- modelOpenness_to_ExperiencesRF1
saveRDS(tree1000_OpennessToExperiences1, "./tree1000_OpennessToExperiences1.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelOpenness_to_Experiencesfinal <- modelOpenness_to_ExperiencesRF1

# Print model
print(modelOpenness_to_Experiencesfinal)

#output in terms of regression coefficients
summary(modelOpenness_to_Experiencesfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
varImp(modelOpenness_to_Experiencesfinal)
plot(varImp(modelOpenness_to_Experiencesfinal), 20, main = "Openness_to_Experiences")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_Experiencesfinal, newdata=test_dfOpenness_to_Experiences)


#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_Experiencesfinal <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_Experiencesfinal

spearmanOpenness_to_Experiencesfinal <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_Experiencesfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOpenness_to_Experiencesfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOpenness_to_Experiencesfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Openness_to_Experiences")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_OpennessToExperiences1 <- modelOpenness_to_Experiencesfinal
saveRDS(besttree_OpennessToExperiences1, "./besttree_OpennessToExperiences1.rds")

#load the model

besttree_OpennessToExperiences1 <- readRDS("./besttree_OpennessToExperiences1.rds")
print(besttree_OpennessToExperiences1)





#######################
#Openness to Experiences2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Openness_Experiences2 <- data[,c(304, 27:255)]

data_Openness_Experiences2$Openness_Experiences2 <- as.factor(data_Openness_Experiences2$Openness_Experiences2)

#Gibt es NAs in der DV?
sum(is.na(data_Openness_Experiences2$Openness_Experiences2))  
data_Openness_Experiences2 <- data_Openness_Experiences2 %>% subset(data_Openness_Experiences2$Openness_Experiences2 != "NA")


#ist die Variable unbalanced?
table(data_Openness_Experiences2$Openness_Experiences2)  
max(table(data_Openness_Experiences2$Openness_Experiences2)/sum(table(data_Openness_Experiences2$Openness_Experiences2)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Openness_Experiences2$Openness_Experiences2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfOpenness_Experiences2 <- data_Openness_Experiences2[index,]
test_dfOpenness_Experiences2 <- data_Openness_Experiences2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelOpenness_Experiences2 <- train(Openness_Experiences2 ~ ., 
                                    data=train_dfOpenness_Experiences2,
                                    tuneGrid = myGrid,
                                    method="ranger",
                                    metric= "ROC",  
                                    na.action = na.omit,
                                    num.tree = 500,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelOpenness_Experiences2
summary(modelOpenness_Experiences2)
plot(modelOpenness_Experiences2)
#best mtry = 12, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2 %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

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


#save model to disk 

tree500_OpennessToExperiences2 <- modelOpenness_Experiences2
saveRDS(tree500_OpennessToExperiences2, "./tree500_OpennessToExperiences2.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelOpenness_Experiences2_1 <- train(Openness_Experiences2 ~ ., 
                                      data=train_dfOpenness_Experiences2,
                                      tuneGrid = myGrid,
                                      method="ranger", 
                                      metric= "ROC", 
                                      na.action = na.omit,
                                      num.tree = 1000,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console
modelOpenness_Experiences2_1
summary(modelOpenness_Experiences2_1)
plot(modelOpenness_Experiences2_1)
#best mtry = 11, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2_1, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2_1 %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

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

#save model to disk 

trees1000_OpennessToExperiences2 <- modelOpenness_Experiences2_1
saveRDS(trees1000_OpennessToExperiences2, "./trees1000_OpennessToExperiences2.rds")


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelOpenness_Experiences2final <- modelOpenness_Experiences2_1

# Print model
print(modelOpenness_Experiences2final)

#output in terms of regression coefficients
summary(modelOpenness_Experiences2final)

#evaluate variable importance 
varImp(modelOpenness_Experiences2final)
plot(varImp(modelOpenness_Experiences2final), 20, main = "Openness_Experiences2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2final, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2final %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOpenness_Experiences2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOpenness_Experiences2final

PartialPlots %>% partial(pred.var = impvar[1],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20],which.class = "Open") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_OpennessToExperiences2 <- modelOpenness_Experiences2final
saveRDS(besttree_OpennessToExperiences2, "./besttree_OpennessToExperiences2.rds")

#load the model

besttree_OpennessToExperiences2 <- readRDS("./besttree_OpennessToExperiences2.rds")
print(besttree_OpennessToExperiences2)




##################
#Alkoholgruppe
##################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Alkoholgruppe <- data[,c(318, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Alkoholgruppe$Alkoholgruppe))
data_Alkoholgruppe <- data_Alkoholgruppe %>% subset(data_Alkoholgruppe$Alkoholgruppe != "NA")


#ist die Variable unbalanced?
table(data_Alkoholgruppe$Alkoholgruppe) 
max(table(data_Alkoholgruppe$Alkoholgruppe)/sum(table(data_Alkoholgruppe$Alkoholgruppe)))  

#IV als Faktor:
data_Alkoholgruppe$Alkoholgruppe <- as.factor(data_Alkoholgruppe$Alkoholgruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Alkoholgruppe$Alkoholgruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

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

#save model to disk 

tree500_Alkoholgruppe <- RFAlkoholgruppe
saveRDS(tree500_Alkoholgruppe, "./tree500_Alkoholgruppe.rds")



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


#save model to disk 

tree1000_Alkoholgruppe <- RFAlkoholgruppe1
saveRDS(tree1000_Alkoholgruppe, "./tree1000_Alkoholgruppe.rds")


####-------tree 3: Final --------------------------------------------------

#final Model definieren
set.seed(1997)
RFAlkoholgruppefinal <- RFAlkoholgruppe1

# Print models
RFAlkoholgruppefinal
summary(RFAlkoholgruppefinal)

#evaluate variable importance 
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

besttree_Alkoholgruppe <- RFAlkoholgruppefinal
saveRDS(besttree_Alkoholgruppe, "./besttree_Alkoholgruppe.rds")

#load the model

besttree_Alkoholgruppe <- readRDS("./besttree_Alkoholgruppe.rds")
print(besttree_Alkoholgruppe)




######################
#Alkohol ja nein: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Alk_ja_nein <- data[,c(319, 27:255)]

data_Alk_ja_nein$Alkohol_ja_nein <- as.factor(data_Alk_ja_nein$Alkohol_ja_nein)

#Gibt es NAs in der DV?
sum(is.na(data_Alk_ja_nein$Alkohol_ja_nein))  
data_Alk_ja_nein <- data_Alk_ja_nein %>% subset(data_Alk_ja_nein$Alkohol_ja_nein != "NA")


#ist die Variable unbalanced?
table(data_Alk_ja_nein$Alkohol_ja_nein)  
max(table(data_Alk_ja_nein$Alkohol_ja_nein)/sum(table(data_Alk_ja_nein$Alkohol_ja_nein)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alk_ja_nein$Alkohol_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlk_ja_nein <- data_Alk_ja_nein[index,]
test_dfAlk_ja_nein <- data_Alk_ja_nein[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelAlk_ja_nein <- train(Alkohol_ja_nein ~ ., 
                          data=train_dfAlk_ja_nein,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelAlk_ja_nein
summary(modelAlk_ja_nein)
plot(modelAlk_ja_nein)
#best mtry = 10, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_nein, newdata=test_dfAlk_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_nein %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

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


#save model to disk 

tree500_Alk_janein <- modelAlk_ja_nein
saveRDS(tree500_Alk_janein, "./tree500_Alk_janein.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAlk_ja_nein1 <- train(Alkohol_ja_nein ~ ., 
                           data=train_dfAlk_ja_nein,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console
modelAlk_ja_nein1
summary(modelAlk_ja_nein1)
plot(modelAlk_ja_nein1)
#best mtry = 10, splitrule = extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_nein1, newdata=test_dfAlk_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_nein1 %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

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


#save model to disk 

tree1000_Alk_janein <- modelAlk_ja_nein1
saveRDS(tree1000_Alk_janein, "./tree1000_Alk_janein.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
modelAlk_ja_neinfinal <- modelAlk_ja_nein1

# Print model
print(modelAlk_ja_neinfinal)

#output in terms of regression coefficients
summary(modelAlk_ja_neinfinal)

#evaluate variable importance 
varImp(modelAlk_ja_neinfinal)
plot(varImp(modelAlk_ja_neinfinal), 20, main = "Alkohol_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_neinfinal, newdata=test_dfAlk_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_neinfinal %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAlk_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAlk_ja_neinfinal

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

besttree_Alk_janein <- modelAlk_ja_neinfinal
saveRDS(besttree_Alk_janein, "./besttree_Alk_janein.rds")

#load the model

besttree_Alk_janein <- readRDS("./besttree_Alk_janein.rds")
print(besttree_Alk_janein)




###################
#Zigarettengruppe: kategorisch
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Zigarettengruppe <- data[,c(320, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Zigarettengruppe$Zigarettengruppe))
data_Zigarettengruppe <- data_Zigarettengruppe %>% subset(data_Zigarettengruppe$Zigarettengruppe != "NA")


#ist die Variable unbalanced?
table(data_Zigarettengruppe$Zigarettengruppe) 
max(table(data_Zigarettengruppe$Zigarettengruppe)/sum(table(data_Zigarettengruppe$Zigarettengruppe)))  

#IV als Faktor:
data_Zigarettengruppe$Zigarettengruppe <- as.factor(data_Zigarettengruppe$Zigarettengruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Zigarettengruppe$Zigarettengruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

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

#set random seed again 

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

#save model to disk 

tree500_Zigarettengruppe <- RFZigarettengruppe
saveRDS(tree500_Zigarettengruppe, "./tree500_Zigarettengruppe.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#num.tree ausprobieren --> ist mehr besser?

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

#save model to disk 

tree1000_Zigarettengruppe <- RFZigarettengruppe1
saveRDS(tree1000_Zigarettengruppe, "./tree1000_Zigarettengruppe.rds")




####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RFZigarettengruppefinal <- RFZigarettengruppe

# Print models
RFZigarettengruppefinal
summary(RFZigarettengruppefinal)

#evaluate variable importance 
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

besttree_Zigarettengruppe <- RFZigarettengruppefinal
saveRDS(besttree_Zigarettengruppe, "./besttree_Zigarettengruppe.rds")

#load the model

besttree_Zigarettengruppe <- readRDS("./besttree_Zigarettengruppe.rds")
print(besttree_Zigarettengruppe)




###################
#Zigaretten ja nein: binär
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Zig_ja_nein <- data[,c(321, 27:255)]

data_Zig_ja_nein$Zigaretten_ja_nein <- as.factor(data_Zig_ja_nein$Zigaretten_ja_nein)

#Gibt es NAs in der DV?
sum(is.na(data_Zig_ja_nein$Zigaretten_ja_nein))  
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Zig_ja_nein <- data_Zig_ja_nein %>% subset(data_Zig_ja_nein$Zigaretten_ja_nein != "NA")


#ist die Variable unbalanced?
table(data_Zig_ja_nein$Zigaretten_ja_nein)  
max(table(data_Zig_ja_nein$Zigaretten_ja_nein)/sum(table(data_Zig_ja_nein$Zigaretten_ja_nein)))  


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Zig_ja_nein$Zigaretten_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfZig_ja_nein <- data_Zig_ja_nein[index,]
test_dfZig_ja_nein <- data_Zig_ja_nein[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelZig_ja_nein <- train(Zigaretten_ja_nein ~ ., 
                          data=train_dfZig_ja_nein,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelZig_ja_nein
summary(modelZig_ja_nein)
plot(modelZig_ja_nein)
#best mtry = 10, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_nein, newdata=test_dfZig_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_nein %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

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


#save model to disk 

tree500_Zigaretten_janein <- modelZig_ja_nein
saveRDS(tree500_Zigaretten_janein, "./tree500_Zigaretten_janein.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelZig_ja_nein1 <- train(Zigaretten_ja_nein ~ ., 
                           data=train_dfZig_ja_nein,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console
modelZig_ja_nein1
summary(modelZig_ja_nein1)
plot(modelZig_ja_nein1)
#best mtry = 10, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_nein1, newdata=test_dfZig_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_nein1 %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

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


#save model to disk 

tree1000_Zigaretten_janein <- modelZig_ja_nein1
saveRDS(tree1000_Zigaretten_janein, "./tree1000_Zigaretten_janein.rds")



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
modelZig_ja_neinfinal <- modelZig_ja_nein


# Print model
print(modelZig_ja_neinfinal)

#output in terms of regression coefficients
summary(modelZig_ja_neinfinal)

#evaluate variable importance 
varImp(modelZig_ja_neinfinal)
plot(varImp(modelZig_ja_neinfinal), 20, main = "Zigaretten_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_neinfinal, newdata=test_dfZig_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_neinfinal %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

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


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelZig_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelZig_ja_neinfinal

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

besttree_Zigaretten_janein <- modelZig_ja_neinfinal
saveRDS(besttree_Zigaretten_janein, "./besttree_Zigaretten_janein.rds")

#load the model

besttree_Zigaretten_janein <- readRDS("./besttree_Zigaretten_janein.rds")
print(besttree_Zigaretten_janein)




######################
#Drogengruppe: kategorisch
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Drogengruppe <- data[,c(322, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Drogengruppe$Drogengruppe))
data_Drogengruppe <- data_Drogengruppe %>% subset(data_Drogengruppe$Drogengruppe != "NA")


#ist die Variable unbalanced?
table(data_Drogengruppe$Drogengruppe) 
max(table(data_Drogengruppe$Drogengruppe)/sum(table(data_Drogengruppe$Drogengruppe)))  

#IV als Faktor:
data_Drogengruppe$Drogengruppe <- as.factor(data_Drogengruppe$Drogengruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Drogengruppe$Drogengruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfDrogengruppe <- data_Drogengruppe[index,]
test_dfDrogengruppe <- data_Drogengruppe [-index,]


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

set.seed(1997)
RFDrogengruppe <- train(Drogengruppe ~ ., 
                        data=train_dfDrogengruppe,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "Kappa",
                        num.tree = 500,
                        trControl = myControl1, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print models to console

RFDrogengruppe
summary(RFDrogengruppe)
plot(RFDrogengruppe)
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFDrogengruppe, newdata=test_dfDrogengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfDrogengruppe$Drogengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppe %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()

#save model to disk 

tree500_Drogengruppe <- RFDrogengruppe
saveRDS(tree500_Drogengruppe, "./tree500_Drogengruppe.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFDrogengruppe1 <- train(Drogengruppe ~ ., 
                         data=train_dfDrogengruppe, 
                         method="ranger", metric= "Kappa",
                         tuneGrid = myGrid,
                         na.action = na.omit,
                         num.tree = 1000,
                         trControl = myControl1, 
                         importance = 'impurity')

# Print models
RFDrogengruppe1
summary(RFDrogengruppe1)
plot(RFDrogengruppe1)
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDrogengruppe1, newdata=test_dfDrogengruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDrogengruppe$Drogengruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppe1 %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()


#save model to disk 

tree1000_Drogengruppe <- RFDrogengruppe1
saveRDS(tree1000_Drogengruppe, "./tree1000_Drogengruppe.rds")



####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RFDrogengruppefinal <- train(Drogengruppe ~ ., 
                             data=train_dfDrogengruppe, 
                             method="ranger", metric= "Kappa",
                             tuneGrid = myGrid,
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl1, 
                             importance = 'impurity')

# Print models
RFDrogengruppefinal
summary(RFDrogengruppefinal)

#evaluate variable importance 
varImp(RFDrogengruppefinal)
plot(varImp(RFDrogengruppefinal), 20, main = "Drogengruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFDrogengruppefinal, newdata=test_dfDrogengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfDrogengruppe$Drogengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppefinal %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFDrogengruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFDrogengruppefinal

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

besttree_Drogengruppe <- RFDrogengruppefinal
saveRDS(besttree_Drogengruppe, "./besttree_Drogengruppe.rds")

#load the model

besttree_Drogengruppe <- readRDS("./besttree_Drogengruppe.rds")
print(besttree_Drogengruppe)




#########################
#Drogen ja nein: binär
########################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Drogen_ja_nein <- data[,c(323, 27:255)]

data_Drogen_ja_nein$Drogen_ja_nein <- as.factor(data_Drogen_ja_nein$Drogen_ja_nein)

#Gibt es NAs in der DV?
sum(is.na(data_Drogen_ja_nein$Drogen_ja_nein))  
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Drogen_ja_nein <- data_Drogen_ja_nein %>% subset(data_Drogen_ja_nein$Drogen_ja_nein != "NA")

#ist die Variable unbalanced?
table(data_Drogen_ja_nein$Drogen_ja_nein)  
max(table(data_Drogen_ja_nein$Drogen_ja_nein)/sum(table(data_Drogen_ja_nein$Drogen_ja_nein)))  


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Drogen_ja_nein$Drogen_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_dfDrogen_ja_nein & test_dfDrogen_ja_nein
train_dfDrogen_ja_nein <- data_Drogen_ja_nein[index,]
test_dfDrogen_ja_nein <- data_Drogen_ja_nein[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelDrogen_ja_nein <- train(Drogen_ja_nein ~ ., 
                             data=train_dfDrogen_ja_nein,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "ROC",  
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console
modelDrogen_ja_nein
summary(modelDrogen_ja_nein)
plot(modelDrogen_ja_nein)
#best mtry = 11, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_nein, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_nein %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

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

#save model to disk 

tree500_Drogen_janein <- modelDrogen_ja_nein
saveRDS(tree500_Drogen_janein, "./tree500_Drogen_janein.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelDrogen_ja_nein1 <- train(Drogen_ja_nein ~ ., 
                              data=train_dfDrogen_ja_nein,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "ROC", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelDrogen_ja_nein1
summary(modelDrogen_ja_nein1)
plot(modelDrogen_ja_nein1)
#best mtry = 11, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_nein1, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_nein1 %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

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

#save model to disk 

tree1000_Drogen_janein <- modelDrogen_ja_nein1
saveRDS(tree1000_Drogen_janein, "./tree1000_Drogen_janein.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#final Model definieren
set.seed(1997)
modelDrogen_ja_neinfinal <- modelDrogen_ja_nein1

# Print model
print(modelDrogen_ja_neinfinal)

#output in terms of regression coefficients
summary(modelDrogen_ja_neinfinal)

#evaluate variable importance 
varImp(modelDrogen_ja_neinfinal)
plot(varImp(modelDrogen_ja_neinfinal), 20, main = "Drogen_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_neinfinal, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_neinfinal %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelDrogen_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelDrogen_ja_neinfinal

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

besttree_Drogen_janein <- modelDrogen_ja_neinfinal
saveRDS(besttree_Drogen_janein, "./besttree_Drogen_janein.rds")

#load the model

besttree_Drogen_janein <- readRDS("./besttree_Drogen_janein.rds")
print(besttree_Drogen_janein)



#######################
#Parteien: categorical, alle (9 Gruppen, Sonstige ausschließen --> 8 Gruppen)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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

#model auc: 0.6527
RFPartei_1 %>%
  test_roc(data = test_dfPartei) %>%
  auc()

#save model to disk 

tree500_WahlPartei <- RFPartei_1
saveRDS(tree500_WahlPartei, "./tree500_WahlPartei.rds")



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
plot(RFPartei_2)
#mtry = 15, extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFPartei_2, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfPartei$Wahl_Partei))

#check for auc: 0,6421
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model auc
RFPartei_2 %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#save model to disk 

tree1000_WahlPartei <- RFPartei_2
saveRDS(tree1000_WahlPartei, "./tree1000_WahlPartei.rds")


#model: 1000 trees performs better at predicting correctly


####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RFPartei_fin <- RFPartei_2

# Print models
RFPartei_fin
summary(RFPartei_fin)

#evaluate variable importance 
varImp(RFPartei_fin)
plot(varImp(RFPartei_fin), 20, main = "Wahl_Partei")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFPartei_fin, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfPartei$Wahl_Partei))


#check for auc: 0,6421
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model AUC: 
RFPartei_fin %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFPartei_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFPartei_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "AfD") %>%plotPartial(main = "AfD")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "FDP") %>%plotPartial(main = "FDP")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "SPD") %>%plotPartial(main = "SPD")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Partei <- RFPartei_fin
saveRDS(besttree_Partei, "./besttree_Partei.rds")

#load the model

besttree_Partei <- readRDS("./besttree_Partei.rds")
print(besttree_Partei)





#######################
#AfD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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


#check for auc: 0,8374
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

#save model to disk 

tree500_AfD <- RfAfD_1
saveRDS(tree500_AfD, "./tree500_AfD.rds")


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
plot(RfAfD_2)
#mtry = 11, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RfAfD_2, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,8363
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


#save model to disk 

tree1000_AfD <- RfAfD_2
saveRDS(tree1000_AfD, "./tree1000_AfD.rds")



#better num.trees: 1000 trees (performs 1 person better in predicting Afd Voters)


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RFAfD_fin <- RfAfD_2

# Print models
RFAfD_fin
summary(RFAfD_fin)



#evaluate variable importance 
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


#checking direction of the 20 most important variables

imp <- importance(RFAfD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFAfD_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_AfD <- RFAfD_fin
saveRDS(besttree_AfD, "./besttree_AfD.rds")

#load the model

Tree_AfD <- readRDS("./besttree_AfD.rds")
print(Tree_AfD)







#######################
#Die Grünen: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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

#model auc: 0,7591
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


#save model to disk 

tree500_Gruene <- RF_Gruene1
saveRDS(tree500_Gruene, "./tree500_Gruene.rds")


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
plot(RF_Gruene2)
#mtry = 12, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Gruene2, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,7619
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


#save model to disk 

tree1000_Gruene <- RF_Gruene2
saveRDS(tree1000_Gruene, "./tree1000_Gruene.rds")

#better num.trees: 500 trees --> better at classifying voters correctly


####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RFGruene_fin <- RF_Gruene1

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

#model auc: 0,7591
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


#checking direction of the 20 most important variables

imp <- importance(RFGruene_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGruene_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Grün <- RFGruene_fin
saveRDS(besttree_Grün, "./besttree_Grün.rds")

#load the model

besttree_Grün <- readRDS("./besttree_Grün.rds")
print(besttree_Grün)





#######################
#CDU/CSU: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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


#save model to disk 

tree500_CDU <- RF_CDU1
saveRDS(tree500_CDU, "./tree500_CDU.rds")


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
plot(RF_CDU2)
#mtry = 16, extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_CDU2, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6278
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

#save model to disk 

tree1000_CDU <- RF_CDU2
saveRDS(tree1000_CDU, "./tree1000_CDU.rds")


#better num.trees: 500 trees (model converges afterwards)


####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RF_CDU_fin <- RF_CDU1

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

#model auc
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


#checking direction of the 20 most important variables

imp <- importance(RF_CDU_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_CDU_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_CDU <- RF_CDU_fin
saveRDS(besttree_CDU, "./besttree_Grün.rds")

#load the model

besttree_CDU <- readRDS("./besttree_Grün.rds")
print(besttree_CDU)




#######################
#Die Linke: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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

#save model to disk 

tree500_Linke <- RF_Linke1
saveRDS(tree500_Linke, "./tree500_Linke.rds")


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
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Linke2, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,7459
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


#save model to disk 

tree1000_Linke <- RF_Linke2
saveRDS(tree1000_Linke, "./tree1000_Linke.rds")

#better num.trees: 500 trees has slightly better auc, the rest is similar


####-------tree 3: Final --------------------------------------------------

#final Model definieren

set.seed(1997)
RF_Linke_fin <- RF_Linke1


# Print models
RF_Linke_fin
summary(RF_Linke_fin)

#evaluate variable importance 
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


#checking direction of the 20 most important variables

imp <- importance(RF_Linke_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_Linke_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Linke <- RF_Linke_fin
saveRDS(besttree_Linke, "./besttree_Linke.rds")

#load the model

besttree_Linke <- readRDS("./besttree_Linke.rds")
print(besttree_Linke)





#######################
#SPD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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
#mtry = 18, extratrees, min.node.size = 10


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

#save model to disk 

tree500_SPD <- RF_SPD1
saveRDS(tree500_SPD, "./tree500_SPD.rds")



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
plot(RF_SPD2)
#mtry = 18, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_SPD2, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,5879
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


#save model to disk 

tree1000_SPD <- RF_SPD2
saveRDS(tree1000_SPD, "./tree1000_SPD.rds")


#better num.trees: 500 trees because of better sensitivity and AUC


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RF_SPD_fin <- RF_SPD1

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

#model auc: 0,5921
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


#checking direction of the 20 most important variables

imp <- importance(RF_SPD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_SPD_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_SPD <- RF_SPD_fin
saveRDS(besttree_SPD, "./besttree_SPD.rds")

#load the model

besttree_SPD <- readRDS("./besttree_SPD.rds")
print(besttree_SPD)




#######################
#FDP: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

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

# Create train_df & test_df

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

#save model to disk 

tree500_FDP <- RF_FDP1
saveRDS(tree500_FDP, "./tree500_FDP.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)
RF_FDP2 <- train(FDP_Waehler ~ ., 
                 data=train_dfFDP,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_FDP2
summary(RF_FDP2)
plot(RF_FDP2)
#mtry = 13, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_FDP2, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6319
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


#save model to disk 

tree1000_FDP <- RF_FDP2
saveRDS(tree1000_FDP, "./tree1000_FDP.rds")


#better num.trees: 1000 trees 


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_FDP_fin <- RF_FDP2


# Print models
RF_FDP_fin
summary(RF_FDP_fin)


#evaluate variable importance 
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

#model auc: 0,6319
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


#checking direction of the 20 most important variables

imp <- importance(RF_FDP_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_FDP_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_FDP <- RF_FDP_fin
saveRDS(besttree_FDP, "./besttree_FDP.rds")

#load the model

besttree_FDP <- readRDS("./besttree_FDP.rds")
print(besttree_FDP)





#######################
#Nichtwähler: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Nichtwahler <- data[,c(343, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Nichtwahler$Nichtwahler)) #181 NAs
data_Nichtwahler <- data_Nichtwahler %>% subset(data_Nichtwahler$Nichtwahler != "NA")
data_Nichtwahler$Nichtwahler <- as.factor(data_Nichtwahler$Nichtwahler)


#ist die Variable unbalanced?
table(data_Nichtwahler$Nichtwahler) #very imbalanced
max(table(data_Nichtwahler$Nichtwahler)/sum(table(data_Nichtwahler$Nichtwahler))) #no information rate 0,8913%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Nichtwahler$Nichtwahler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfNichtwahler <- data_Nichtwahler[index,]
test_dfNichtwahler <- data_Nichtwahler[-index,]


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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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
#mtry = 12, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Nichtwahler1, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfNichtwahler$Nichtwahler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6801
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

#save model to disk 

tree500_Nichtwahler <- RF_Nichtwahler1
saveRDS(tree500_Nichtwahler, "./tree500_Nichtwahler.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)
RF_Nichtwahler2 <- train(Nichtwahler ~ ., 
                         data=train_dfNichtwahler,
                         tuneGrid = myGrid,
                         method="ranger", 
                         metric= "ROC",
                         num.tree = 1000,
                         na.action = na.omit,
                         trControl = myControl1, 
                         importance = 'impurity')

# Print models to console

RF_Nichtwahler2
summary(RF_Nichtwahler2)
plot(RF_Nichtwahler2)
#mtry = 12, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Nichtwahler2, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfNichtwahler$Nichtwahler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6804
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


#save model to disk 

tree1000_Nichtwahler <- RF_Nichtwahler2
saveRDS(tree1000_Nichtwahler, "./tree1000_Nichtwahler.rds")


#better num.trees: 500 trees --> worse overall accuracy but better for predicting Nichtwähler 


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RF_Nichtwahler_fin <- RF_Nichtwahler1


# Print models
RF_Nichtwahler_fin
summary(RF_Nichtwahler_fin)


#evaluate variable importance 
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

#model auc: 0,6804
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


#checking direction of the 20 most important variables

imp <- importance(RF_Nichtwahler_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_Nichtwahler_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Nichtwahler <- RF_Nichtwahler_fin
saveRDS(besttree_Nichtwahler, "./besttree_Nichtwahler.rds")

#load the model

besttree_Nichtwahler <- readRDS("./besttree_Nichtwahler.rds")
print(besttree_Nichtwahler)





#######################
#Corona Hardliner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Hardliner <- data[,c(307, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Hardliner$Corona_Hardliner)) #no NAs
data_Hardliner <- data_Hardliner %>% subset(data_Hardliner$Corona_Hardliner != "NA")


#ist die Variable unbalanced?
table(data_Hardliner$Corona_Hardliner) #in Ordnung
max(table(data_Hardliner$Corona_Hardliner)/sum(table(data_Hardliner$Corona_Hardliner))) #no information rate 57%

#IV als Faktor:
data_Hardliner$Corona_Hardliner <- as.factor(data_Hardliner$Corona_Hardliner)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Hardliner$Corona_Hardliner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfHardliner <- data_Hardliner[index,]
test_dfHardliner <- data_Hardliner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# no sampling needed

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size


set.seed(1997)
RFHardliner1 <- train(Corona_Hardliner ~ ., 
                      data=train_dfHardliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFHardliner1
summary(RFHardliner1)
plot(RFHardliner1)
#mtry = 13, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHardliner1, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0.5857
RFHardliner1 %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#check ROC plots
model_list <- list(Model1 = RFHardliner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

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


#save model to disk 

tree500_Corona_Hardliner <- RFHardliner1
saveRDS(tree500_Corona_Hardliner, "./tree500_Corona_Hardliner.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)
RFHardliner2 <- train(Corona_Hardliner ~ ., 
                      data=train_dfHardliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFHardliner2
summary(RFHardliner2)
plot(RFHardliner2)
#mtry = 13, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHardliner2, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,5843
RFHardliner2 %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFHardliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

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

# best model with 500

tree1000_Corona_Hardliner <- RFHardliner2
saveRDS(tree1000_Corona_Hardliner, "./tree1000_Corona_Hardliner.rds")


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFHardliner_fin <- RFHardliner1

# Print models
RFHardliner_fin
summary(RFHardliner_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFHardliner_fin)
plot(varImp(RFHardliner_fin), 20, main = "Corona_Hardliner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFHardliner_fin, newdata=test_dfHardliner)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFHardliner_fin %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFHardliner1,
                   Model2 = RFHardliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

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


#checking direction of the 20 most important variables

imp <- importance(RFHardliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFHardliner_fin

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

besttree_Hardliner <- RFHardliner_fin
saveRDS(besttree_Hardliner, "./besttree_Hardliner_janein.rds")

#load the model

besttree_Hardliner <- readRDS("./besttree_Hardliner_janein.rds")
print(besttree_Hardliner)






#######################
#Corona Softliner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Softliner <- data[,c(308, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Softliner$Corona_Softliner)) #no NAs
data_Softliner <- data_Softliner %>% subset(data_Softliner$Corona_Softliner != "NA")


#ist die Variable unbalanced?
table(data_Softliner$Corona_Softliner) #should correct for imbalancedness
max(table(data_Softliner$Corona_Softliner)/sum(table(data_Softliner$Corona_Softliner))) #no information rate 74%

#IV als Faktor:
data_Softliner$Corona_Softliner <- as.factor(data_Softliner$Corona_Softliner)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Softliner$Corona_Softliner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSoftliner <- data_Softliner[index,]
test_dfSoftliner <- data_Softliner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# apply smote sampling and 10-fold CV

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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSoftliner1 <- train(Corona_Softliner ~ ., 
                      data=train_dfSoftliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSoftliner1
summary(RFSoftliner1)
plot(RFSoftliner1)
#mtry = 13, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSoftliner1, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6115
RFSoftliner1 %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFSoftliner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

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

#save model to disk 

tree500_Corona_Softliner <- RFSoftliner1
saveRDS(tree500_Corona_Softliner, "./tree500_Corona_Softliner.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFSoftliner2 <- train(Corona_Softliner ~ ., 
                      data=train_dfSoftliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSoftliner2
summary(RFSoftliner2)
plot(RFSoftliner2)
#mtry = 13, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoftliner2, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6137
RFSoftliner2 %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSoftliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

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

#better num.trees: 500 trees ist minimal besser

#save model to disk 

tree1000_Corona_Softliner <- RFSoftliner2
saveRDS(tree1000_Corona_Softliner, "./tree1000_Corona_Softliner.rds")


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFSoftliner_fin <- RFSoftliner1

# Print models
RFSoftliner_fin
summary(RFSoftliner_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSoftliner_fin)
plot(varImp(RFSoftliner_fin), 20, main = "Corona_Softliner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSoftliner_fin, newdata=test_dfSoftliner)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6721
RFSoftliner_fin %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSoftliner1,
                   Model2 = RFSoftliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

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


#checking direction of the 20 most important variables

imp <- importance(RFSoftliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFSoftliner_fin

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

besttree_Softliner <- RFSoftliner_fin
saveRDS(besttree_Softliner, "./besttree_Softlinerjanein.rds")

#load the model

RFSoftliner_fin <- readRDS("./besttree_Softlinerjanein.rds")
print(RFSoftliner_fin)






#######################
#Corona Skeptiker: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Skeptiker <- data[,c(309, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Skeptiker$Corona_Skeptiker)) #0 NAs
data_Skeptiker <- data_Skeptiker %>% subset(data_Skeptiker$Corona_Skeptiker != "NA")


#ist die Variable unbalanced?
table(data_Skeptiker$Corona_Skeptiker) #imbalanced tow. "Nein"
max(table(data_Skeptiker$Corona_Skeptiker)/sum(table(data_Skeptiker$Corona_Skeptiker))) #no information rate 85,47%

#IV als Faktor:
data_Skeptiker$Corona_Skeptiker <- as.factor(data_Skeptiker$Corona_Skeptiker)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Skeptiker$Corona_Skeptiker, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSkeptiker <- data_Skeptiker[index,]
test_dfSkeptiker <- data_Skeptiker[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# sample with smote

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

# test for 500 trees

set.seed(1997)
RFSkeptiker1 <- train(Corona_Skeptiker ~ ., 
                      data=train_dfSkeptiker,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSkeptiker1
summary(RFSkeptiker1)
plot(RFSkeptiker1)
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSkeptiker1, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6864
RFSkeptiker1 %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#check ROC plots
model_list <- list(Model1 = RFSkeptiker1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

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

#save model to disk 

tree500_Corona_Skeptiker <- RFSkeptiker1
saveRDS(tree500_Corona_Skeptiker, "./tree500_Corona_Skeptiker.rds")

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 

set.seed(1997)
RFSkeptiker2 <- train(Corona_Skeptiker ~ ., 
                      data=train_dfSkeptiker,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSkeptiker2
summary(RFSkeptiker2)
plot(RFSkeptiker2)
#mtry = 11, extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSkeptiker2, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6921
RFSkeptiker2 %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#ROC plots
model_list <- list(Model1 = RFSkeptiker2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

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

#better num.trees: 500 trees --> better predictions for Skeptiker

tree1000_Corona_Skeptiker <- RFSkeptiker2
saveRDS(tree1000_Corona_Skeptiker, "./tree1000_Corona_Skeptiker.rds")

####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFSkeptiker_fin <- RFSkeptiker1

# Print models
RFSkeptiker_fin
summary(RFSkeptiker_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSkeptiker_fin)
plot(varImp(RFSkeptiker_fin), 20, main = "Corona_Skeptiker")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSkeptiker_fin, newdata=test_dfSkeptiker)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFSkeptiker_fin %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSkeptiker1,
                   Model2 = RFSkeptiker2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

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


#checking direction of the 20 most important variables


imp <- importance(RFSkeptiker_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSkeptiker_fin

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

besttree_Skeptiker <- RFSkeptiker_fin
saveRDS(besttree_Skeptiker, "./besttree_Skeptikerjanein.rds")

#load the model

besttree_Skeptiker <- readRDS("./besttree_Skeptikerjanein.rds")
print(besttree_Skeptiker)





#######################
#Corona Leugner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------



#define data for analysis
data_Leugner <- data[,c(310, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Leugner$Corona_Leugner)) #0 NAs
data_Leugner <- data_Leugner %>% subset(data_Leugner$Corona_Leugner != "NA")


#ist die Variable unbalanced?
table(data_Leugner$Corona_Leugner) #very imbalanced!!
max(table(data_Leugner$Corona_Leugner)/sum(table(data_Leugner$Corona_Leugner))) #no information rate 0,9385

#IV als Faktor:
data_Leugner$Corona_Leugner <- as.factor(data_Leugner$Corona_Leugner)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Leugner$Corona_Leugner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfLeugner <- data_Leugner[index,]
test_dfLeugner <- data_Leugner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# 10-fold CV + resampling

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

# test of 500 trees

set.seed(1997)
RFLeugner1 <- train(Corona_Leugner ~ ., 
                    data=train_dfLeugner,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFLeugner1
summary(RFLeugner1)
plot(RFLeugner1)
#mtry = 13, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFLeugner1, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6736
RFLeugner1 %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFLeugner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

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

#save model to disk 

tree500_Corona_Leugner <- RFLeugner1
saveRDS(tree500_Corona_Leugner, "./tree500_Corona_Leugner.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFLeugner2 <- train(Corona_Leugner ~ ., 
                    data=train_dfLeugner,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFLeugner2
summary(RFLeugner2)
plot(RFLeugner2)
#mtry = 13, extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFLeugner2, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6743
RFLeugner2 %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFLeugner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

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

#better num.trees: 1000 trees --> better predictions


#save model to disk 

tree1000_CoronaLeugner <- RFLeugner2
saveRDS(tree1000_Corona_Leugner, "./tree100_Corona_Leugner.rds")


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFLeugner_fin <- RFLeugner2

# Print models
RFLeugner_fin
summary(RFLeugner_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFLeugner_fin)
plot(varImp(RFLeugner_fin), 20, main = "Corona_Leugner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFLeugner_fin, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFLeugner_fin %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFLeugner1,
                   Model2 = RFLeugner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

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

# Plot ROC curves

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFLeugner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFLeugner_fin

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

besttree_Leugner <- RFLeugner_fin
saveRDS(besttree_Leugner, "./besttree_Leugner_janein.rds")

#load the model

besttree_Leugner <- readRDS("./besttree_Leugner_janein.rds")
print(besttree_Leugner)





#######################
#Hardliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Hardliner_num <- data[,c(278, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein))

#ist die Variable unbalanced?
table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein) #okay
max(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein)/sum(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein))) #no information rate 26,82%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfHard_num <- data_Hardliner_num[index,]
test_dfHard_num <- data_Hardliner_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFHard_num1 <- train(Corona_Massnahmen_muessten_haerter_sein ~ ., 
                     data=train_dfHard_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 500,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFHard_num1
summary(RFHard_num1)
plot(RFHard_num1)

#best mtry:10
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHard_num1, newdata=test_dfHard_num)

MAE(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearson_Hard1 <- cor.test(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearson_Hard1

spearmanHard1 <- cor.test(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard1


#save model to disk 

tree500_CoronaHardliner_num <- RFHard_num1
saveRDS(tree500_CoronaHardliner_num, "./tree500_CoronaHardliner_num.rds")

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)

RFHard_num2 <- train(Corona_Massnahmen_muessten_haerter_sein ~ ., 
                     data=train_dfHard_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 1000,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFHard_num2
summary(RFHard_num2)
plot(RFHard_num2)
#best mtry:10
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHard_num2, newdata=test_dfHard_num)


MAE(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonHard2 <- cor.test(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearsonHard2

spearmanHard2 <- cor.test(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard2

#both quite bad but 1000 is a bit better

#save model to disk 

tree1000_CoronaHardliner_num <- RFHard_num2
saveRDS(tree1000_CoronaHardliner_num, "./tree1000_CoronaHardliner_num.rds")


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)

RFHard_num_fin <- RFHard_num2

# Print model
RFHard_num_fin
summary(RFHard_num_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFHard_num_fin)
plot(varImp(RFHard_num_fin), 20, main = "Corona_Massnahmen_muessten_haerter_sein")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFHard_num_fin, newdata=test_dfHard_num)

MAE(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonHard3 <- cor.test(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearsonHard3

spearmanHard3 <- cor.test(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard3


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFHard_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- RFHard_num_fin

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

besttree_Hardliner_num <- RFHard_num_fin
saveRDS(besttree_Hardliner_num, "./besttree_Hard_num.rds")

#load the model

besttree_Hardliner_num <- readRDS("./besttree_Hard_num.rds")
print(besttree_Hardliner_num)







#######################
#Softliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Softliner_num <- data[,c(277, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Softliner_num$Corona_Massnahmen_uebertrieben)) #0 NAs

#ist die Variable unbalanced?
table(data_Softliner_num$Corona_Massnahmen_uebertrieben) #Überhang zu niedrigeren Werten, aber noch ok
max(table(data_Softliner_num$Corona_Massnahmen_uebertrieben)/sum(table(data_Softliner_num$Corona_Massnahmen_uebertrieben))) #no information rate 34,22%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Softliner_num$Corona_Massnahmen_uebertrieben, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSoft_num <- data_Softliner_num[index,]
test_dfSoft_num <- data_Softliner_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSoft_num1 <- train(Corona_Massnahmen_uebertrieben ~ ., 
                     data=train_dfSoft_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 500,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFSoft_num1
summary(RFSoft_num1)
plot(RFSoft_num1)

#best mtry:11
#splitrule: extratrees
#min.node.size used: 5

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFSoft_num1, newdata=test_dfSoft_num)

MAE(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft1 <- cor.test(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft1

spearmanSoft1 <- cor.test(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft1

#save model to disk 

tree500_CoronaSoftliner_num <- RFSoft_num1
saveRDS(tree500_CoronaSoftliner_num, "./tree500_CoronaSoftliner_num.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFSoft_num2 <- train(Corona_Massnahmen_uebertrieben ~ ., 
                     data=train_dfSoft_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 1000,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFSoft_num2
summary(RFSoft_num2)
plot(RFSoft_num2)
#best mtry:10
#splitrule: extratrees
#min.node.size used: 5

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoft_num2, newdata=test_dfSoft_num)


MAE(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft2 <- cor.test(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft2

spearmanSoft2 <- cor.test(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft2

#num.trees 1000 performs slightly better

#save model to disk 

tree1000_CoronaSoftliner_num <- RFSoft_num2
saveRDS(tree1000_CoronaSoftliner_num, "./tree1000_CoronaSoftliner_num.rds")


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)

RFSoft_num_fin <- RFSoft_num2

# Print model
RFSoft_num_fin
summary(RFSoft_num_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSoft_num_fin)
plot(varImp(RFSoft_num_fin), 20, main = "Corona_Massnahmen_uebertrieben")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSoft_num_fin, newdata=test_dfSoft_num)

MAE(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft3 <- cor.test(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft3

spearmanSoft3 <- cor.test(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft3


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSoft_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSoft_num_fin

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

besttree_Softliner_num <- RFSoft_num_fin
saveRDS(besttree_Softliner_num, "./besttree_SoftNum.rds")

#load the model

besttree_Softliner_num <- readRDS("./besttree_SoftNum.rds")
print(besttree_Softliner_num)








#######################
#Skeptiker 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Skeptik_num <- data[,c(279, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe)) #0 NAs

#ist die Variable unbalanced?
table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe) #Überhang zu 1
max(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe)/sum(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe))) #no information rate 52%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_Skeptiker_num <- data_Skeptik_num[index,]
test_Skeptiker_num <- data_Skeptik_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSkeptiker_num1 <- train(Corona_ist_harmlos_gleich_Grippe ~ ., 
                          data=train_Skeptiker_num,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "RMSE",
                          num.tree = 500,
                          trControl = myControl, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print model to console

RFSkeptiker_num1
summary(RFSkeptiker_num1)
plot(RFSkeptiker_num1)

#best mtry:10
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSkeptiker_num1, newdata=test_Skeptiker_num)

MAE(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker1 <- cor.test(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker1

spearmanSkeptiker1 <- cor.test(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker1

#save model to disk 

tree500_CoronaSkeptiker_num <- RFSkeptiker_num1
saveRDS(tree500_CoronaSkeptiker_num, "./tree500_CoronaSkeptiker_num.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)

RFSkeptikernum_2 <- train(Corona_ist_harmlos_gleich_Grippe ~ ., 
                          data=train_Skeptiker_num,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "RMSE",
                          num.tree = 1000,
                          trControl = myControl, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print model to console

RFSkeptikernum_2
summary(RFSkeptikernum_2)
plot(RFSkeptiker_num2)

#best mtry:10
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSkeptikernum_2, newdata=test_Skeptiker_num)


MAE(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker2 <- cor.test(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker2

spearmanSkeptiker2 <- cor.test(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker2

#num.trees 1000 performs better

#save model to disk 

tree1000_CoronaSkeptiker_num <- RFSkeptiker_num2
saveRDS(tree1000_CoronaSkeptiker_num, "./tree1000_CoronaSkeptiker_num.rds")

####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)

RFSkeptikernum_fin <- RFSkeptikernum_2

# Print model
RFSkeptikernum_fin
summary(RFSkeptikernum_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSkeptikernum_fin)
plot(varImp(RFSkeptikernum_fin), 20, main = "Corona_ist_harmlos_gleich_Grippe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSkeptikernum_fin, newdata=test_Skeptiker_num)

MAE(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker_fin <- cor.test(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker_fin

spearmanSkeptiker_fin <- cor.test(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker_fin


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSkeptikernum_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFSkeptikernum_fin

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

besttree_Skeptiker_num <- RFSkeptikernum_fin
saveRDS(besttree_Skeptiker_num, "./besttree_Skeptik_num.rds")

#load the model

besttree_Skeptiker_num <- readRDS("./besttree_Skeptik_num.rds")
print(besttree_Skeptiker_num)








#######################
#Leugner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Leugner_num <- data[,c(280, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Leugner_num$Glaube_nicht_an_Corona)) #0 NAs

#ist die Variable unbalanced?
table(data_Leugner_num$Glaube_nicht_an_Corona) #imbalanced!
max(table(data_Leugner_num$Glaube_nicht_an_Corona)/sum(table(data_Leugner_num$Glaube_nicht_an_Corona))) #no information rate 76%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Leugner_num$Glaube_nicht_an_Corona, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfLeugner_num <- data_Leugner_num[index,]
test_dfLeugner_num <- data_Leugner_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFLeugner_num1 <- train(Glaube_nicht_an_Corona ~ ., 
                        data=train_dfLeugner_num,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "RMSE",
                        num.tree = 500,
                        trControl = myControl, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print model to console

RFLeugner_num1
summary(RFLeugner_num1)
plot(RFLeugner_num1)

#best mtry:11
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFLeugner_num1, newdata=test_dfLeugner_num)

MAE(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner1 <- cor.test(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner1

spearmanLeugner1 <- cor.test(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner1

#save model to disk 

tree500_CoronaLeugner_num <- RFLeugner_num1
saveRDS(tree500_CoronaLeugner_num, "./tree500_CoronaLeugner_num.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFLeugner_num2 <- train(Glaube_nicht_an_Corona ~ ., 
                        data=train_dfLeugner_num,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "RMSE",
                        num.tree = 1000,
                        trControl = myControl, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print model to console

RFLeugner_num2
summary(RFLeugner_num2)
plot(RFLeugner_num2)

#best mtry:10
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFLeugner_num2, newdata=test_dfLeugner_num)


MAE(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner2 <- cor.test(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner2

spearmanLeugner2 <- cor.test(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner2

#num.trees 1000 performs better

#save model to disk 

tree1000_CoronaLeugner_num <- RFLeugner_num2
saveRDS(tree1000_CoronaLeugner_num, "./tree1000_CoronaLeugner_num.rds")


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)

RFLeugner_num_fin <- RFLeugner_num2

# Print model
RFLeugner_num_fin
summary(RFLeugner_num_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFLeugner_num_fin)
plot(varImp(RFLeugner_num_fin), 20, main = "Glaube_nicht_an_Corona")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFLeugner_num_fin, newdata=test_dfLeugner_num)

MAE(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner_fin <- cor.test(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner_fin

spearmanLeugner_fin <- cor.test(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner_fin


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFLeugner_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFLeugner_num_fin

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

besttree_Leugner_num <- RFLeugner_num_fin
saveRDS(besttree_Leugner_num, "./besttree_Leugner_num.rds")

#load the model

besttree_Leugner_num <- readRDS("./besttree_Leugner_num.rds")
print(besttree_Leugner_num)




#######################
#Einkommensgruppe: Categorical (3 Gruppen: hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Einkommen <- data[,c(315, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Einkommen$Einkommensgruppe)) #121 NAs
data_Einkommen <- data_Einkommen %>% subset(data_Einkommen$Einkommensgruppe != "NA")


#ist die Variable unbalanced?
table(data_Einkommen$Einkommensgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Einkommen$Einkommensgruppe)/sum(table(data_Einkommen$Einkommensgruppe)))  

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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFEinkommen_1 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "Kappa",
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models to console

RFEinkommen_1
summary(RFEinkommen_1)
plot(RFEinkommen_1)
#mtry = 17, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFEinkommen_1, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 0,6772
RFEinkommen_1 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#save model to disk 

tree500_Einkommen <- RFEinkommen_1
saveRDS(tree500_Einkommen, "./tree500_Einkommen.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFEinkommen_2 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid,
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl1, 
                       importance = 'impurity')

# Print models
RFEinkommen_2
summary(RFEinkommen_2)
plot(RFEinkommen_2)
#mtry = 15, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFEinkommen_2, newdata=test_dfEinkommen)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))


#check for auc: 0,6715
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFEinkommen_2 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()


#model1: 500 trees performs better 

#save model to disk 

tree1000_Einkommen <- RFEinkommen_2
saveRDS(tree1000_Einkommen, "./tree1000_Einkommen.rds")

####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFEinkommen_fin <- RFEinkommen_1

# Print models
RFEinkommen_fin
summary(RFEinkommen_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFEinkommen_fin)
plot(varImp(RFEinkommen_fin), 20, main = "Einkommensgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFEinkommen_fin, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 0,6772
RFEinkommen_fin %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFEinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFEinkommen_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main = "niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittel") %>%plotPartial(main = "mittel")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main = "hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Einkommen <- RFEinkommen_fin
saveRDS(besttree_Einkommen, "./besttree_Einkommen.rds")

#load the model

besttree_Einkommen <- readRDS("./besttree_Einkommen.rds")
print(besttree_Einkommen)





#######################
#über oder unter Durchschnittseinkommen (2000€): binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Durchschnittseinkommen <- data[,c(344, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Durchschnittseinkommen$Durchschnittseinkommen)) #121 NAs
data_Durchschnittseinkommen <- data_Durchschnittseinkommen %>% subset(data_Durchschnittseinkommen$Durchschnittseinkommen != "NA")


#ist die Variable unbalanced?
table(data_Durchschnittseinkommen$Durchschnittseinkommen) #in Ordnung, ca. 1:2
max(table(data_Durchschnittseinkommen$Durchschnittseinkommen)/sum(table(data_Durchschnittseinkommen$Durchschnittseinkommen)))

#IV als Faktor:
data_Durchschnittseinkommen$Durchschnittseinkommen <- as.factor(data_Durchschnittseinkommen$Durchschnittseinkommen)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Durchschnittseinkommen$Durchschnittseinkommen, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfDurchschnittseinkommen <- data_Durchschnittseinkommen[index,]
test_dfDurchschnittseinkommen <- data_Durchschnittseinkommen[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# no sampling needed

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)




####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFDurchschnittseinkommen1 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 500,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen1
summary(RFDurchschnittseinkommen1)
plot(RFDurchschnittseinkommen1)
#mtry = 10, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFDurchschnittseinkommen1, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc: 0,7385
RFDurchschnittseinkommen1 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFDurchschnittseinkommen1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

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

#save model to disk 

tree500_Durchschnittseinkommen <- RFDurchschnittseinkommen1
saveRDS(tree500_Durchschnittseinkommen, "./tree500_Durchschnittseinkommen.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFDurchschnittseinkommen2 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen2
summary(RFDurchschnittseinkommen2)
pot(RFDurchschnittseinkommen2)
#mtry = 10, extratrees, min.node.size = 15



# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDurchschnittseinkommen2, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc: 0,7407
RFDurchschnittseinkommen2 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFDurchschnittseinkommen2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

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

#grows very similar trees, thus keep 500 --> we don't need to grow 1000

#save model to disk 

tree1000_Durchschnittseinkommen <- RFDurchschnittseinkommen2
saveRDS(tree1000_Durchschnittseinkommen, "./tree1000_Durchschnittseinkommen.rds")



####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFDurchschnittseinkommen_fin <- RFDurchschnittseinkommen1

# Print models
RFDurchschnittseinkommen_fin
summary(RFDurchschnittseinkommen_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFDurchschnittseinkommen_fin)
plot(varImp(RFDurchschnittseinkommen_fin), 20, main = "Durchschnittseinkommen")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFDurchschnittseinkommen_fin, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc: 
RFDurchschnittseinkommen_fin %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFDurchschnittseinkommen1,
                   Model2 = RFDurchschnittseinkommen2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

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

# Plot ROC curves

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFDurchschnittseinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFDurchschnittseinkommen_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Durchschnittseinkommen <- RFDurchschnittseinkommen_fin
saveRDS(besttree_Durchschnittseinkommen, "./besttree_Durchschnittseinkommen.rds")

#load the model

besttree_Durchschnittseinkommen <- readRDS("./besttree_Durchschnittseinkommen.rds")
print(besttree_Durchschnittseinkommen)




#######################
#Green Values: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green1 <- data[,c(305, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green1$Green_Values)) #keine NAs

#ist die Variable unbalanced?
table(data_Green1$Green_Values) #Überhang zu höheren Werten, aber nicht zu stark (mean: 4,97)
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values))) #no information rate 7,65%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green1$Green_Values, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen1 <- data_Green1[index,]
test_dfGreen1 <- data_Green1[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen1_1 <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 500,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_1
summary(RFGreen1_1)
plot(RFGreen1_1)

#best mtry:11
#splitrule: extratrees
#min.node.size used: 15

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_1, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_1

spearmanGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_1

#save model to disk 

tree500_GreenValues_num <- RFGreen1_1
saveRDS(tree500_GreenValues_num, "./tree500_GreenValues_num.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 num.tree ausprobieren --> ist mehr besser?

set.seed(1997)

RFGreen1_2 <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 1000,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_2
summary(RFGreen1_2)
plot(RFGreen1_2)
#mtry = 10, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_2, newdata=test_dfGreen1)


MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_2

spearmanGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_2

#num.trees 1000 performs slightly better

#save model to disk 

tree1000_GreenValues_num <- RFGreen1_2
saveRDS(tree1000_GreenValues_num, "./tree1000_GreenValues_num.rds")



####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)

RFGreen1_fin <- RFGreen1_2

# Print model
RFGreen1_fin
summary(RFGreen1_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFGreen1_fin)
plot(varImp(RFGreen1_fin), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_fin, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_fin

spearmanGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_fin


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFGreen1_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen1_fin

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

besttree_Green1 <- RFGreen1_fin
saveRDS(besttree_Green1, "./besttree_Green1.rds")

#load the model

besttree_Green1 <- readRDS("./besttree_Green1.rds")
print(besttree_Green1)








#######################
#Green Values 2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green2 <- data[,c(306, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green2$Green2))  

#ist die Variable unbalanced?
table(data_Green2$Green2) #Verteilung ca 1:6 --> in Tests mit beachten!
max(table(data_Green2$Green2)/sum(table(data_Green2$Green2))) #no information rate 84%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green2$Green2, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen2 <- data_Green2[index,]
test_dfGreen2 <- data_Green2[-index,]


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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen2_1 <- train(Green2 ~ ., 
                    data=train_dfGreen2,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFGreen2_1
summary(RFGreen2_1)
plot(RFGreen2_1)
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFGreen2_1, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6469
RFGreen2_1 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFGreen2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

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

#save model to disk 

tree500_GreenValues_bin <- RFGreen2_1
saveRDS(tree500_GreenValues_bin, "./tree500_GreenValues_bin.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)
RFGreen2_2 <- train(Green2 ~ ., 
                    data=train_dfGreen2,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFGreen2_2
summary(RFGreen2_2)
plot(RFGreen2_2)
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFGreen2_2, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,646
RFGreen2_2 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFGreen2_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

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

#better num.trees: 1000 trees --> better at predicting imbalanced class

#save model to disk 

tree1000_GreenValues_bin <- RFGreen2_2
saveRDS(tree1000_GreenValues_bin, "./tree1000_GreenValues_bin.rds")



####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFGreen2_fin <- RFGreen2_2

# Print models
RFGreen2_fin
summary(RFGreen2_fin)


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFGreen2_fin)
plot(varImp(RFGreen2_fin), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFGreen2_fin, newdata=test_dfGreen2)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,646
RFGreen2_fin %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFGreen2_1,
                   Model2 = RFGreen2_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

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




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFGreen2_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen2_fin

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

besttree_Green2 <- RFGreen2_fin
saveRDS(besttree_Green2, "./besttree_Green2.rds")

#load the model

besttree_Green2 <- readRDS("./besttree_Green2.rds")
print(besttree_Green2)




#######################
#Beschäftigung: Categorical (7 Gruppen: Arbeitslos/-suchend, Auszubildende/r, Berufstätige/r, Hausfrau/-mann, Rentner/in, Schüler/in, Student/in)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Beschaeftigung <- data[,c(10, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Beschaeftigung$Beschaeftigung)) #17 NAs
data_Beschaeftigung <- data_Beschaeftigung %>% subset(data_Beschaeftigung$Beschaeftigung != "NA")


#ist die Variable unbalanced?
table(data_Beschaeftigung$Beschaeftigung) 
max(table(data_Beschaeftigung$Beschaeftigung)/sum(table(data_Beschaeftigung$Beschaeftigung))) 

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


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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

#model auc: 
RFBeschaeftigung %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()

#save model to disk 

tree500_Beschaeftigung <- RFBeschaeftigung
saveRDS(tree500_Beschaeftigung, "./tree500_Beschaeftigung.rds")



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

#save model to disk 

tree1000_Beschaeftigung <- RFBeschaeftigung1
saveRDS(tree1000_Beschaeftigung, "./tree1000_Beschaeftigung.rds")


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


besttree_Beschaeftigung <- RFBeschaeftigungFinal
saveRDS(besttree_Beschaeftigung, "./besttree_Beschaeftigung.rds")





#####################
#Arbeitend oder nicht: binär
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Arbeitend_oder_nicht <- data[,c(317, 27:255)]

# Convert to factor
data_Arbeitend_oder_nicht$Arbeitend_oder_nicht = as.factor(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)


#Gibt es NAs in der DV?
sum(is.na(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)) 
data_Arbeitend_oder_nicht <- data_Arbeitend_oder_nicht %>% subset(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht != "NA")


#ist die Variable unbalanced?
table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)
max(table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)/sum(table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)))



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht, p=.8, list= FALSE, times= 1)

# split into train and test set

train_dfArbeitend_oder_nicht <- data_Arbeitend_oder_nicht[index,]
test_dfArbeitend_oder_nicht <- data_Arbeitend_oder_nicht[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelArbeitend_oder_nichtRF <- train(Arbeitend_oder_nicht ~ ., 
                                     data=train_dfArbeitend_oder_nicht,
                                     tuneGrid = myGrid,
                                     method="ranger",
                                     metric= "ROC",  
                                     na.action = na.omit,
                                     num.tree = 500,
                                     trControl = myControl, 
                                     importance = 'impurity')

# Print model to console

modelArbeitend_oder_nichtRF
summary(modelArbeitend_oder_nichtRF)
plot(modelArbeitend_oder_nichtRF)

#best mtry = 18, splitrule = extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtRF, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtRF %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

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

#save model to disk 

tree500_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtRF
saveRDS(tree500_Arbeitend_oder_nicht, "./tree500_Arbeitend_oder_nicht.rds")

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelArbeitend_oder_nichtRF1 <- train(Arbeitend_oder_nicht ~ ., 
                                      data=train_dfArbeitend_oder_nicht,
                                      tuneGrid = myGrid,
                                      method="ranger", 
                                      metric= "ROC", 
                                      na.action = na.omit,
                                      num.tree = 1000,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console

modelArbeitend_oder_nichtRF1
summary(modelArbeitend_oder_nichtRF1)
plot(modelArbeitend_oder_nichtRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtRF1, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtRF1 %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

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


#save model to disk 

tree1000_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtRF1
saveRDS(tree1000_Arbeitend_oder_nicht, "./tree1000_Arbeitend_oder_nicht.rds")


#fit model with num.trees = 500 trees (better performance)

####-------tree 3: Final --------------------------------------------------


modelArbeitend_oder_nichtfinal <- modelArbeitend_oder_nichtRF


# Print model
print(modelArbeitend_oder_nichtfinal)

#output in terms of regression coefficients
summary(modelArbeitend_oder_nichtfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelArbeitend_oder_nichtfinal)
plot(varImp(modelArbeitend_oder_nichtfinal), 20, main = "Arbeitend oder Nicht")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtfinal, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtfinal %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelArbeitend_oder_nichtfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelArbeitend_oder_nichtfinal

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

final_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtfinal
saveRDS(final_Arbeitend_oder_nicht, "./final_Arbeitend_oder_nicht.rds")




################
#Bildungsgruppe: kategorisch
###############


#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Bildungsgruppe <- data[,c(314, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Bildungsgruppe$Bildungsgruppe))
data_Bildungsgruppe <- data_Bildungsgruppe %>% subset(data_Bildungsgruppe$Bildungsgruppe != "NA")


#ist die Variable unbalanced?
table(data_Bildungsgruppe$Bildungsgruppe) 
max(table(data_Bildungsgruppe$Bildungsgruppe)/sum(table(data_Bildungsgruppe$Bildungsgruppe))) 

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



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

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

#model auc: 
RFBildungsgruppe %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()

# AUC model 1 0.744


#save model to disk 

tree500_Bildungsgruppe <- RFBildungsgruppe
saveRDS(tree500_Bildungsgruppe, "./tree500_Bildungsgruppe.rds")


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


#model1: 1000 trees performs better

#save model to disk 

tree1000_Bildungsgruppe <- RFBildungsgruppe1
saveRDS(tree1000_Bildungsgruppe, "./tree1000_Bildungsgruppe.rds")

####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBildungsgruppeFinal <- RFBildungsgruppe1

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

besttree_Bildungsgruppe <- RFBildungsgruppeFinal
saveRDS(besttree_Bildungsgruppe, "./besttree_Bildungsgruppe.rds")








#######################
#Religion: Categorical (4 Gruppen: Christentum, Ich fühle mich keiner Religion zugehörig, Islam, Judentum)
######################

#-------------------------------------Data Pre-Processing---------------------------------------------


#define data for analysis
data_Religion <- data[,c(12, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Religion$Religion)) 
#Sonstige auch als NA, um sie aus Analyse auszuschließen:
data_Religion <- data_Religion %>% replace_with_na_all(condition = ~.x == "Sonstige:")
#Datenset ohne NAs
data_Religion <- data_Religion %>% subset(data_Religion$Religion != "NA")

#ist die Variable unbalanced?
table(data_Religion$Religion) 
max(table(data_Religion$Religion)/sum(table(data_Religion$Religion)))

#IV als Faktor:
data_Religion$Religion <- as.factor(data_Religion$Religion)


#Variablennamen anpassen für Analyse
data_Religion <- data_Religion %>% mutate(Religion = case_when(Religion == "Christentum" ~ 'Christentum',
                                                               Religion == "Ich fühle mich keiner Religion zugehörig" ~ 'Nicht_zugehörig',
                                                               Religion == "Islam" ~ 'Islam',
                                                               Religion == "Judentum" ~ 'Judentum'))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Religion$Religion, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfReligion <- data_Religion[index,]
test_dfReligion <- data_Religion[-index,]



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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 
set.seed(1997)
RFReligion <- train(Religion ~ ., 
                    data=train_dfReligion,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFReligion
summary(RFReligion)
plot(RFReligion)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFReligion, newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligion %>%
  test_roc(data = test_dfReligion) %>%
  auc()

#save model to disk 

tree500_Religion <- RFReligion
saveRDS(tree500_Religion, "./tree500_Religion.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFReligion1 <- train(Religion ~ ., 
                     data=train_dfReligion, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFReligion1
summary(RFReligion1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFReligion1, newdata=test_dfReligion)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfReligion$Religion))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligion1 %>%
  test_roc(data = test_dfReligion) %>%
  auc()


#model1: 500 trees performs better

#save model to disk 

tree1000_Religion <- RFReligion1
saveRDS(tree1000_Religion, "./tree1000_Religion.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFReligionFinal <- RFReligion1

# Print models
RFReligionFinal 
summary(RFReligionFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFReligionFinal )
plot(varImp(RFReligionFinal ), 20, main = "Religion")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFReligionFinal , newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligionFinal %>%
  test_roc(data = test_dfReligion) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFReligionFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFReligionFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Christentum") %>%plotPartial (main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Christentum") %>%plotPartial(main = "Christentum")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Islam") %>%plotPartial(main = "Islam")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Judentum") %>%plotPartial(main = "Judentum")

#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Religion <- RFReligionFinal
saveRDS(besttree_Religion, "./besttree_Religion.rds")





#######################
#Religioes ja nein: binär
######################


#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Religioes <- data[,c(333, 27:255)]


# Convert DV to factor
data_Religioes$Religioes = as.factor(data_Religioes$Religioes)


#Gibt es NAs in der DV?
sum(is.na(data_Religioes$Religioes))  
data_Religioes <- data_Religioes %>% subset(data_Religioes$Religioes != "NA")


#ist die Variable unbalanced?
table(data_Religioes$Religioes) #Verteilung in Ordnung
max(table(data_Religioes$Religioes)/sum(table(data_Religioes$Religioes))) 



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Religioes$Religioes, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht
train_dfReligioes <- data_Religioes[index,]
test_dfReligioes <- data_Religioes[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelReligioesRF <- train(Religioes ~ ., 
                          data=train_dfReligioes,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelReligioesRF
summary(modelReligioesRF)
plot(modelReligioesRF)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesRF, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesRF %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

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

#save model to disk 

tree500_Religioes <- modelReligioesRF
saveRDS(tree500_Religioes, "./tree500_Religioes.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelReligioesRF1 <- train(Religioes ~ ., 
                           data=train_dfReligioes,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console

modelReligioesRF1
summary(modelReligioesRF1)
plot(modelReligioesRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesRF1, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesRF1 %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

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


#save model to disk 

tree1000_Religioes <- modelReligioesRF1
saveRDS(tree1000_Religioes, "./tree1000_Religioes.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------


modelReligioesfinal <- modelReligioesRF1
# Print model
print(modelReligioesfinal)

#output in terms of regression coefficients
summary(modelReligioesfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelReligioesfinal)
plot(varImp(modelReligioesfinal), 20, main = "Religioes")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesfinal, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesfinal %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelReligioesfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelReligioesfinal

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

besttree_Religioes <- modelReligioesfinal
saveRDS(besttree_Religioes, "./besttree_Religioes.rds")



####################
#Christentum oder Islam: binär
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_IslamChrist <- data[,c(334, 27:255)]


# Convert IV to factor
data_IslamChrist$Islam_oder_Christ = as.factor(data_IslamChrist$Islam_oder_Christ)


#Gibt es NAs in der DV?
sum(is.na(data_IslamChrist$Islam_oder_Christ)) #810 NAs
data_IslamChrist <- data_IslamChrist %>% subset(data_IslamChrist$Islam_oder_Christ != "NA")


#ist die Variable unbalanced?
table(data_IslamChrist$Islam_oder_Christ)
max(table(data_IslamChrist$Islam_oder_Christ)/sum(table(data_IslamChrist$Islam_oder_Christ)))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_IslamChrist$Islam_oder_Christ, p=.8, list= FALSE, times= 1)


### name anpassen an DV

train_dfIslamChrist <- data_IslamChrist[index,]
test_dfIslamChrist <- data_IslamChrist[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelIslamChristRF <- train(Islam_oder_Christ ~ ., 
                            data=train_dfIslamChrist,
                            tuneGrid = myGrid,
                            method="ranger",
                            metric= "ROC",  
                            na.action = na.omit,
                            num.tree = 500,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelIslamChristRF
summary(modelIslamChristRF)
plot(modelIslamChristRF)

#best mtry = 18, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristRF, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristRF %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#ROC plot
model_list <- list(M1 = modelIslamChristRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

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

#save model to disk 

tree500_Islam_Christ <- modelIslamChristRF
saveRDS(tree500_Islam_Christ , "./tree500_Islam_Christ .rds")




####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelIslamChristRF1 <- train(Islam_oder_Christ ~ ., 
                             data=train_dfIslamChrist,
                             tuneGrid = myGrid,
                             method="ranger", 
                             metric= "ROC", 
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelIslamChristRF1
summary(modelIslamChristRF1)
plot(modelIslamChristRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristRF1, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristRF1 %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelIslamChristRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

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


#save model to disk 

tree1000_Islam_Christ <- modelIslamChristRF1
saveRDS(tree1000_Islam_Christ , "./tree1000_Islam_Christ .rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------


modelIslamChristfinal <- modelIslamChristRF

# Print model
print(modelIslamChristfinal)

#output in terms of regression coefficients
summary(modelIslamChristfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelIslamChristfinal)
plot(varImp(modelIslamChristfinal), 20, main = "Islam_oder_Christ")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristfinal, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristfinal %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelIslamChristfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables


imp <- importance(modelIslamChristfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- modelIslamChristfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Islam") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 
bestree_Islam_Christ <- modelIslamChristRFfinal
saveRDS(besttree_Islam_Christ , "./besttree_Islam_Christ .rds")



####################
#Migrationshintergrund: binär
#####################

#---------------------------------------------------DATA PRE-PROCESSING-----------------------------------------------------

#define data for analysis
data_Migrationshintergrund <- data[,c(14, 27:255)]


# Convert  DV to factor
data_Migrationshintergrund$Migrationshintergrund = as.factor(data_Migrationshintergrund$Migrationshintergrund)


#Gibt es NAs in der DV?
sum(is.na(data_Migrationshintergrund$Migrationshintergrund))  
data_Migrationshintergrund <- data_Migrationshintergrund %>% subset(data_Migrationshintergrund$Migrationshintergrund != "NA")


#ist die Variable unbalanced?
table(data_Migrationshintergrund$Migrationshintergrund) #Verteilung in Ordnung
max(table(data_Migrationshintergrund$Migrationshintergrund)/sum(table(data_Migrationshintergrund$Migrationshintergrund)))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Migrationshintergrund$Migrationshintergrund, p=.8, list= FALSE, times= 1)

# Create train & test dataset

train_dfMigrationshintergrund <- data_Migrationshintergrund[index,]
test_dfMigrationshintergrund <- data_Migrationshintergrund[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelMigrationshintergrundRF <- train(Migrationshintergrund ~ ., 
                                      data=train_dfMigrationshintergrund,
                                      tuneGrid = myGrid,
                                      method="ranger",
                                      metric= "ROC",  
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console

modelMigrationshintergrundRF
summary(modelMigrationshintergrundRF)
plot(modelMigrationshintergrundRF)

#best mtry = 18, splitrule = extratrees, min.node.size = 10

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundRF, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundRF %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

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

#save model to disk 

tree500_Migrationshintergrund <- modelMigrationshintergrundRF
saveRDS(tree500_Migrationshintergrund, "./tree500_Migrationshintergrund.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelMigrationshintergrundRF1 <- train(Migrationshintergrund ~ ., 
                                       data=train_dfMigrationshintergrund,
                                       tuneGrid = myGrid,
                                       method="ranger", 
                                       metric= "ROC", 
                                       na.action = na.omit,
                                       num.tree = 1000,
                                       trControl = myControl, 
                                       importance = 'impurity')

# Print model to console

modelMigrationshintergrundRF1
summary(modelMigrationshintergrundRF1)
plot(modelMigrationshintergrundRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundRF1, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundRF1 %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

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

#save model to disk 

tree1000_Migrationshintergrund <- modelMigrationshintergrundRF1
saveRDS(tree1000_Migrationshintergrund, "./tree1000_Migrationshintergrund.rds")



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------


modelMigrationshintergrundfinal <- modelMigrationshintergrundRF1


# Print model
print(modelMigrationshintergrundfinal)

#output in terms of regression coefficients
summary(modelMigrationshintergrundfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelMigrationshintergrundfinal)
plot(varImp(modelMigrationshintergrundfinal), 20, main = "Migrationshintergrund")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundfinal, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundfinal %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

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


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelMigrationshintergrundfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelMigrationshintergrundfinal

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
bestree_Migrationshintergrund <- modelMigrationshintergrundRFfinal
saveRDS(besttree_Migrationshintergrund, "./besttree_Migrationshintergrund.rds")



#######################
#Sexuelle_Orientierung: Categorical (3 Gruppen: Bisexuell, Heterosexuell, Homosexuell)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Sexuelle_Orientierung <- data[,c(16, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Sexuelle_Orientierung$Sexuelle_Orientierung)) #122 NAs
#Sonstige auch als NA, um sie aus Analyse auszuschließen:
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% replace_with_na_all(condition = ~.x == "Sonstiges:")
#Datenset ohne NAs
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% subset(data_Sexuelle_Orientierung$Sexuelle_Orientierung != "NA")

#ist die Variable unbalanced?
table(data_Sexuelle_Orientierung$Sexuelle_Orientierung) 
max(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung)/sum(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung)))  

#DV als Faktor:
data_Sexuelle_Orientierung$Sexuelle_Orientierung <- as.factor(data_Sexuelle_Orientierung$Sexuelle_Orientierung)

#Variablennamen anpassen für Analyse
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% mutate(Sexuelle_Orientierung = case_when(Sexuelle_Orientierung == "1" ~ 'Bisexuell',
                                                                                                      Sexuelle_Orientierung == "2" ~ 'Heterosexuell',
                                                                                                      Sexuelle_Orientierung == "3" ~ 'Homosexuell'))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Sexuelle_Orientierung$Sexuelle_Orientierung, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[index,]
test_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[-index,]



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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFSexuelle_Orientierung <- train(Sexuelle_Orientierung ~ ., 
                                 data=train_dfSexuelle_Orientierung,
                                 tuneGrid = myGrid,
                                 method="ranger", 
                                 metric= "Kappa",
                                 num.tree = 500,
                                 trControl = myControl1, 
                                 na.action = na.omit,
                                 importance = 'impurity')

# Print models to console

RFSexuelle_Orientierung
summary(RFSexuelle_Orientierung)
plot(RFSexuelle_Orientierung)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFSexuelle_Orientierung, newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_Orientierung %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()

#save model to disk 

tree500_Sexuelle_Orientierung <- RFSexuelle_Orientierung
saveRDS(tree500_Sexuelle_Orientierung, "./tree500_Sexuelle_Orientierung.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFSexuelle_Orientierung1 <- train(Sexuelle_Orientierung ~ ., 
                                  data=train_dfSexuelle_Orientierung, 
                                  method="ranger", metric= "Kappa",
                                  tuneGrid = myGrid,
                                  na.action = na.omit,
                                  num.tree = 1000,
                                  trControl = myControl1, 
                                  importance = 'impurity')

# Print models
RFSexuelle_Orientierung1
summary(RFSexuelle_Orientierung1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSexuelle_Orientierung1, newdata=test_dfSexuelle_Orientierung)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_Orientierung1 %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()


#model1: 500 trees performs better

#save model to disk 

tree1000_Sexuelle_Orientierung <- RFSexuelle_Orientierung1
saveRDS(tree1000_Sexuelle_Orientierung, "./tree1000_Sexuelle_Orientierung.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFSexuelle_OrientierungFinal <- RFSexuelle_Orientierung

# Print models
RFSexuelle_OrientierungFinal 
summary(RFSexuelle_OrientierungFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSexuelle_OrientierungFinal )
plot(varImp(RFSexuelle_OrientierungFinal ), 20, main = "Sexuelle_Orientierung")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSexuelle_OrientierungFinal , newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_OrientierungFinal %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFSexuelle_OrientierungFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- RFSexuelle_OrientierungFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Bisexuell") %>%plotPartial (main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Sexuelle_Orientierung <- RFSexuelle_OrientierungFinal
saveRDS(besttree_Sexuelle_Orientierung, "./besttree_Sexuelle_Orientierung.rds")






#####################
#Heterosexuell ja nein: binär
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#
data_Hetero <- data[,c(335, 27:255)]


# Convert DV to factor
data_Hetero$Heterosexuell = as.factor(data_Hetero$Heterosexuell)


#Gibt es NAs in der DV?
sum(is.na(data_Hetero$Heterosexuell))  
data_Hetero <- data_Hetero %>% subset(data_Hetero$Heterosexuell != "NA")


#ist die Variable unbalanced?
table(data_Hetero$Heterosexuell) #Verteilung in Ordnung
max(table(data_Hetero$Heterosexuell)/sum(table(data_Hetero$Heterosexuell)))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Hetero$Heterosexuell, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfHetero <- data_Hetero[index,]
test_dfHetero <- data_Hetero[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelHeteroRF <- train(Heterosexuell ~ ., 
                       data=train_dfHetero,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",  
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelHeteroRF
summary(modelHeteroRF)
plot(modelHeteroRF)

#best mtry = 18, splitrule = extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeteroRF, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)



#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF %>%
  test_roc(data = test_dfHetero) %>%
  auc()


#ROC plot
model_list <- list(M1 = modelHeteroRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

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

#save model to disk 

tree500_Hetero <- modelHeteroRF
saveRDS(tree500_Hetero, "./tree500_Hetero.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelHeteroRF1 <- train(Heterosexuell ~ ., 
                        data=train_dfHetero,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelHeteroRF1
summary(modelHeteroRF1)
plot(modelHeteroRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeteroRF1, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF1 %>%
  test_roc(data = test_dfHetero) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelHeteroRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

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


#save model to disk 

tree1000_Hetero <- modelHeteroRF1
saveRDS(tree1000_Hetero, "./tree1000_Hetero.rds")


#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

#finalmodel
modelHeterofinal <- modelHeteroRF

# Print model
print(modelHeterofinal)

#output in terms of regression coefficients
summary(modelHeterofinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelHeterofinal)
plot(varImp(modelHeterofinal), 20, main = "Heterosexuell")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeterofinal, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeterofinal %>%
  test_roc(data = test_dfHetero) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelHeterofinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelHeterofinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelHeterofinal

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

besttree_Hetero <- modelHeterofinal
saveRDS(besttree_Hetero, "./besttree_Hetero.rds")

#load the model

besttree_Hetero <- readRDS("./besttree_Hetero.rds")
print(besttree_Hetero)


####################
#Allein vs. in Beziehung: binär
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# Columns wählen
data_AlleinBeziehung <- data[,c(316, 27:255)]


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_AlleinBeziehung$Allein_vs_Beziehung = as.factor(data_AlleinBeziehung$Allein_vs_Beziehung)


#Gibt es NAs in der DV?
sum(is.na(data_AlleinBeziehung$Allein_vs_Beziehung)) #17 NAs
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_AlleinBeziehung <- data_AlleinBeziehung %>% subset(data_AlleinBeziehung$Allein_vs_Beziehung != "NA")


#ist die Variable unbalanced?
table(data_AlleinBeziehung$Allein_vs_Beziehung) #Verteilung in Ordnung
max(table(data_AlleinBeziehung$Allein_vs_Beziehung)/sum(table(data_AlleinBeziehung$Allein_vs_Beziehung))) #no information rate 65%



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AlleinBeziehung$Allein_vs_Beziehung, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAlleinBeziehung <- data_AlleinBeziehung[index,]
test_dfAlleinBeziehung <- data_AlleinBeziehung[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  search = "grid",
)

####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelAlleinBeziehungRF <- train(Allein_vs_Beziehung ~ ., 
                                data=train_dfAlleinBeziehung,
                                tuneGrid = myGrid,
                                method="ranger",
                                metric= "ROC", 
                                na.action = na.omit,
                                num.tree = 500,
                                trControl = myControl, 
                                importance = 'impurity')

# Print model to console

modelAlleinBeziehungRF
summary(modelAlleinBeziehungRF)
plot(modelAlleinBeziehungRF)

#best mtry = 10, splitrule = extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlleinBeziehungRF, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlleinBeziehung$Allein_vs_Beziehung)



#check for AUC: 0,6163
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungRF %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC-plot
model_list <- list(M1 = modelAlleinBeziehungRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

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

#save model to disk 

tree500_AlleinBeziehung <- modelAlleinBeziehungRF
saveRDS(tree500_AlleinBeziehung, "./tree500_AlleinBeziehung.rds")


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelAlleinBeziehungRF1 <- train(Allein_vs_Beziehung ~ ., 
                                 data=train_dfAlleinBeziehung,
                                 tuneGrid = myGrid,
                                 method="ranger", 
                                 metric= "ROC", 
                                 na.action = na.omit,
                                 num.tree = 1000,
                                 trControl = myControl, 
                                 importance = 'impurity')

# Print model to console

modelAlleinBeziehungRF1
summary(modelAlleinBeziehungRF1)
plot(modelAlleinBeziehungRF1)
#best mtry = 12, splitrule = extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelAlleinBeziehungRF1, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions1, test_dfAlleinBeziehung$Allein_vs_Beziehung)


#check for AUC: 0,612
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungRF1 %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC-plot
model_list <- list(M1 = modelAlleinBeziehungRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

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


#save model to disk 

tree1000_AlleinBeziehung <- modelAlleinBeziehungRF1
saveRDS(tree1000_AlleinBeziehung, "./tree1000_AlleinBeziehung.rds")



#fit model with num.trees = 500 trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

modelAlleinBeziehungfinal <- modelAlleinBeziehungRF

# Print model
print(modelAlleinBeziehungfinal)

#output in terms of regression coefficients
summary(modelAlleinBeziehungfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelAlleinBeziehungfinal)
plot(varImp(modelAlleinBeziehungfinal), 20, main = "Allein_vs_Beziehung")


# predict outcome using model from train_df applied to the test_df

predictions2 <- predict(modelAlleinBeziehungfinal, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions2, test_dfAlleinBeziehung$Allein_vs_Beziehung)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungfinal %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC plot
model_list <- list(M1 = modelAlleinBeziehungfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAlleinBeziehungfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- modelAlleinBeziehungfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Allein") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_allein_beziehung <- modelAlleinBeziehungfinal
saveRDS(besttree_allein_beziehung, "./besttree_allein_beziehung.rds")

#load the model

besttree_allein_beziehung <- readRDS("./besttree_allein_beziehung.rds")
print(besttree_allein_beziehung)



#######################
#Beziehungsstatus: Categorical (5 Gruppen: Geschieden, In einer Beziehung, Single, Verheiratet, Verwitwet)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Beziehungsstatus <- data[,c(18, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Beziehungsstatus$Beziehungsstatus)) #17 NAs
data_Beziehungsstatus <- data_Beziehungsstatus %>% subset(data_Beziehungsstatus$Beziehungsstatus != "NA")


#ist die Variable unbalanced?
table(data_Beziehungsstatus$Beziehungsstatus)
max(table(data_Beziehungsstatus$Beziehungsstatus)/sum(table(data_Beziehungsstatus$Beziehungsstatus)))

#IV als Faktor:
data_Beziehungsstatus$Beziehungsstatus <- as.factor(data_Beziehungsstatus$Beziehungsstatus)

#Variablennamen anpassen für Analyse --> nur noch 3 Kategorien
data_Beziehungsstatus <- data_Beziehungsstatus %>% mutate(Beziehungsstatus = case_when(Beziehungsstatus == "Geschieden" ~ 'Single',
                                                                                       Beziehungsstatus == "In einer Beziehung" ~ 'In_Beziehung',
                                                                                       Beziehungsstatus == "Single" ~ 'Single',
                                                                                       Beziehungsstatus == "Verheiratet" ~ 'Verheiratet',
                                                                                       Beziehungsstatus == "Verwitwet" ~ 'Single'))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Beziehungsstatus$Beziehungsstatus, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBeziehungsstatus <- data_Beziehungsstatus[index,]
test_dfBeziehungsstatus <- data_Beziehungsstatus[-index,]



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
  search = "grid"
)



####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees
#use metric Kappa because of unbalanced dataset

#set random seed again 

set.seed(1997)
RFBeziehungsstatus <- train(Beziehungsstatus ~ ., 
                            data=train_dfBeziehungsstatus,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "Kappa",
                            num.tree = 500,
                            trControl = myControl1, 
                            na.action = na.omit,
                            importance = 'impurity')

# Print models to console

RFBeziehungsstatus
summary(RFBeziehungsstatus)
plot(RFBeziehungsstatus)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBeziehungsstatus, newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatus %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()

#save model to disk 

tree500_Beziehungsstatus <- RFBeziehungsstatus
saveRDS(tree500_Beziehungsstatus, "./tree500_Beziehungsstatus.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFBeziehungsstatus1 <- train(Beziehungsstatus ~ ., 
                             data=train_dfBeziehungsstatus, 
                             method="ranger", metric= "Kappa",
                             tuneGrid = myGrid,
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl1, 
                             importance = 'impurity')

# Print models
RFBeziehungsstatus1
summary(RFBeziehungsstatus1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBeziehungsstatus1, newdata=test_dfBeziehungsstatus)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatus1 %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()


#model1: 500 trees performs better


#save model to disk 

tree1000_Beziehungsstatus <- RFBeziehungsstatus1
saveRDS(tree1000_Beziehungsstatus, "./tree1000_Beziehungsstatus.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBeziehungsstatusFinal <- RFBeziehungsstatus

# Print models
RFBeziehungsstatusFinal 
summary(RFBeziehungsstatusFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFBeziehungsstatusFinal )
plot(varImp(RFBeziehungsstatusFinal ), 20, main = "Beziehungsstatus")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBeziehungsstatusFinal , newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatusFinal %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBeziehungsstatusFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFBeziehungsstatusFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Single") %>%plotPartial(main = "Single")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Beziehungsstatus <- RFBeziehungsstatusFinal
saveRDS(besttree_Beziehungsstatus, "./besttree_Beziehungsstatus.rds")

#load the model

besttree_Beziehungsstatus <- readRDS("./besttree_Beziehungsstatus.rds")
print(besttree_Beziehungsstatus)



#######################
#Kinder: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# choose relevant columns
data_Kinder <- data[,c(288, 27:255)]


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_Kinder$Kinder = as.character(data_Kinder$Kinder)
data_Kinder$Kinder[data_Kinder$Kinder == "2"] = "Nein"
data_Kinder$Kinder[data_Kinder$Kinder == "1"] = "Ja"
data_Kinder$Kinder = as.factor(data_Kinder$Kinder)


# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative
levels(data_Kinder$Kinder)
data_Kinder$Kinder = factor(data_Kinder$Kinder, levels = c("Ja", "Nein"))
levels(data_Kinder$Kinder)


#Gibt es NAs in der DV?
sum(is.na(data_Kinder$Kinder)) #keine NAs
data_Kinder <- data_Kinder %>% subset(data_Kinder$Kinder != "NA")


#ist die Variable unbalanced?
table(data_Kinder$Kinder) #ja, unbalanced
max(table(data_Kinder$Kinder)/sum(table(data_Kinder$Kinder))) #no information rate 70%



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Kinder$Kinder, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_df

train_dfKinder <- data_Kinder[index,]
test_dfKinder <- data_Kinder[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelKinderRF <- train(Kinder ~ ., 
                       data=train_dfKinder,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",  
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelKinderRF
summary(modelKinderRF)
plot(modelKinderRF)

#best mtry = 10, splitrule = extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelKinderRF, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfKinder$Kinder)

#check for AUC : 0,7814
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderRF %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#  ROC-plot
model_list <- list(M1 = modelKinderRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

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

#save model to disk 

tree500_Kinder <- modelKinderRF
saveRDS(tree500_Kinder, "./tree500_Kinder.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelKinderRF1 <- train(Kinder ~ ., 
                        data=train_dfKinder,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelKinderRF1
summary(modelKinderRF1)
plot(modelKinderRF1)
#mtry = 10, extratrees, min.node.size = 5

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelKinderRF1, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions1, test_dfKinder$Kinder)


#check for AUC : 0,7809
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderRF1 %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelKinderRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

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


#save model to disk 

tree1000_Kinder <- modelKinderRF1
saveRDS(tree1000_Kinder, "./tree1000_Kinder.rds")



#fit model with num.trees = 1000 trees (better predictions)

####-------tree 3: Final --------------------------------------------------

#final model definieren
modelKinderFinal <- modelKinderRF1

# Print model
print(modelKinderFinal)

#output in terms of regression coefficients
summary(modelKinderFinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelKinderFinal)
plot(varImp(modelKinderFinal), 20, main = "weiblich_maennlich")

# predict outcome using model from train_df applied to the test_df

predictions3 <- predict(modelKinderFinal, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions3, test_dfKinder$Kinder)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderFinal %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#ROC plot
model_list <- list(ModelFinal = modelKinderFinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

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



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelKinderFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelKinderFinal

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

besttree_Kinder <- modelKinderFinal
saveRDS(besttree_Kinder, "./besttree_Kinder.rds")

#load the model

besttree_Kinder <- readRDS("./besttree_Kinder.rds")
print(besttree_Kinder)


