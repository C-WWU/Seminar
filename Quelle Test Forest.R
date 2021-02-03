
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
#Quelle: Categorical (3 Gruppen: Privat, Gapfish, Surveycircle)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Quelle <- data[,c(20, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Quelle$Quelle)) #no NAs


#ist die Variable unbalanced?
table(data_Quelle$Quelle) #Gapfish is very dominant, adjust for imbalancedness
max(table(data_Quelle$Quelle)/sum(table(data_Quelle$Quelle))) #no information rate 0,645

#IV als Faktor:
data_Quelle$Quelle <- as.factor(data_Quelle$Quelle)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Quelle$Quelle, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfQuelle <- data_Quelle[index,]
test_dfQuelle <- data_Quelle[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation; control for imbalancedness

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
#set random seed again 

set.seed(1997)
RFQuelle1 <- train(Quelle ~ ., 
                       data=train_dfQuelle,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "Kappa",
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models to console

RFQuelle1
summary(RFQuelle1)
plot(RFQuelle1)
#mtry = 16, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFQuelle1, newdata=test_dfQuelle)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfQuelle$Quelle))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfQuelle$Quelle,
                 predict(model, data, type = "prob")[, "Privat"])
  
}

#model auc: 0.732
RFQuelle1 %>%
  test_roc(data = test_dfQuelle) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 für num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFQuelle2 <- train(Quelle ~ ., 
                       data=train_dfQuelle, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid,
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl1, 
                       importance = 'impurity')

# Print models
RFQuelle2
summary(RFQuelle2)
plot(RFQuelle2)
#mtry = 20, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFQuelle2, newdata=test_dfQuelle)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfQuelle$Quelle))


#check for auc:
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfQuelle$Quelle,
                 predict(model, data, type = "prob")[, "Privat"])
  
}

#model auc: 0,7335
RFQuelle2 %>%
  test_roc(data = test_dfQuelle) %>%
  auc()


#model: 1000 trees performs better


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFQuelle_fin <- RFQuelle2

# Print models
RFQuelle_fin
summary(RFQuelle_fin)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFQuelle_fin)
plot(varImp(RFQuelle_fin), 20, main = "Quelle")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFQuelle_fin, newdata=test_dfQuelle)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfQuelle$Quelle))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfQuelle$Quelle,
                 predict(model, data, type = "prob")[, "Privat"])
  
}

#model auc: 0,7335
RFQuelle_fin %>%
  test_roc(data = test_dfQuelle) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFQuelle_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFQuelle_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Privat") %>%plotPartial(main = "Privat")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Privat") %>%plotPartial(main = "Privat")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Surveycircle") %>%plotPartial(main = "Surveycircle")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Gapfish") %>%plotPartial(main = "Gapfish")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Quelle <- RFQuelle_fin
saveRDS(besttree_Quelle, "./tree_Quelle.rds")

#load the model

besttree_Quelle <- readRDS("./tree_Quelle.rds")
print(besttree_Quelle)
