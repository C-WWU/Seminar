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


#save model to disk 

tree1000_AgeRange <- RFAgeRange1
saveRDS(tree1000_AgeRange, "./tree1000_AgeRange.rds")



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


#save model to disk 

tree1000_Alter <- modelAlterRF1
saveRDS(tree1000_Alter, "./tree1000_Alter.rds")




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


#save model to disk 

tree1000_Geschlecht <- modelGeschlechtRF1
saveRDS(tree1000_Geschlecht, "./tree1000_Geschlecht.rds")



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


#save model to disk 

tree1000_Ost_West <- modelOst_West1
saveRDS(tree1000_Ost_West, "./tree1000_OstWest.rds")




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


#save model to disk 

tree1000_Extraversion1 <- modelExtraversionRF1
saveRDS(tree1000_Extraversion1, "./tree1000_Extraversion1.rds")



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


#save model to disk 

tree1000_Extraversion2 <- modelExtraversion2_1
saveRDS(tree1000_Extraversion2, "./tree1000_Extraversion2.rds")




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


#save model to disk 

tree1000_Agreeableness1 <- modelAgreeablenessRF1
saveRDS(tree1000_Agreeableness1, "./tree1000_Agreeableness1.rds")


#fit model with num.trees = xx trees (better performance)




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


#save model to disk 

tree1000_Agreeableness2 <- modelAgreeableness2_1
saveRDS(tree1000_Agreeableness2, "./tree1000_Agreeableness2.rds")





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


#save model to disk 

tree1000_Conscentiousness1 <- modelConscientiousnessRF1
saveRDS(tree1000_Conscentiousness1, "./tree1000_Conscentiousness1.rds")




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

#save model to disk 

tree1000_Conscentiousness2 <- modelConscientiousness2_1
saveRDS(tree1000_Conscentiousness2, "./tree1000_Conscentiousness2.rds")






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

#save model to disk 

tree1000_EmotionalStability <- modelEmotional_stablityRF1
saveRDS(tree1000_EmotionalStability, "./tree1000_EmotionalStability.rds")





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

#save model to disk 

tree1000_EmotionalStability2 <- modelEmotional_stablity2_1
saveRDS(tree1000_EmotionalStability2, "./tree1000_EmotionalStability2.rds")





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

#save model to disk 

tree1000_OpennessToExperiences1 <- modelOpenness_to_ExperiencesRF1
saveRDS(tree1000_OpennessToExperiences1, "./tree1000_OpennessToExperiences1.rds")



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


#save model to disk 

trees1000_OpennessToExperiences2 <- modelOpenness_Experiences2_1
saveRDS(trees1000_OpennessToExperiences2, "./trees1000_OpennessToExperiences2.rds")






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


#save model to disk 

tree1000_Alkoholgruppe <- RFAlkoholgruppe1
saveRDS(tree1000_Alkoholgruppe, "./tree1000_Alkoholgruppe.rds")



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


#save model to disk 

tree1000_Alk_janein <- modelAlk_ja_nein1
saveRDS(tree1000_Alk_janein, "./tree1000_Alk_janein.rds")



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

#save model to disk 

tree1000_Zigarettengruppe <- RFZigarettengruppe1
saveRDS(tree1000_Zigarettengruppe, "./tree1000_Zigarettengruppe.rds")




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

#save model to disk 

tree1000_Zigaretten_janein <- modelZig_ja_nein1
saveRDS(tree1000_Zigaretten_janein, "./tree1000_Zigaretten_janein.rds")




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

#save model to disk 

tree1000_Drogengruppe <- RFDrogengruppe1
saveRDS(tree1000_Drogengruppe, "./tree1000_Drogengruppe.rds")






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

#save model to disk 

tree1000_Drogen_janein <- modelDrogen_ja_nein1
saveRDS(tree1000_Drogen_janein, "./tree1000_Drogen_janein.rds")




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
table(data_Partei$Wahl_Partei) 
max(table(data_Partei$Wahl_Partei)/sum(table(data_Partei$Wahl_Partei))) 

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


#save model to disk 

tree1000_WahlPartei <- RFPartei_2
saveRDS(tree1000_WahlPartei, "./tree1000_WahlPartei.rds")






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
table(data_AfD$AfD_Waehler)
max(table(data_AfD$AfD_Waehler)/sum(table(data_AfD$AfD_Waehler))) 


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


#save model to disk 

tree1000_AfD <- RfAfD_2
saveRDS(tree1000_AfD, "./tree1000_AfD.rds")







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
table(data_Gruen$Gruene_Waehler)
max(table(data_Gruen$Gruene_Waehler)/sum(table(data_Gruen$Gruene_Waehler))) 


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


#save model to disk 

tree1000_Gruene <- RF_Gruene2
saveRDS(tree1000_Gruene, "./tree1000_Gruene.rds")







#######################
#CDU/CSU: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_CDU <- data[,c(337, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_CDU$CDU_CSU_Waehler)) 
data_CDU <- data_CDU %>% subset(data_CDU$CDU_CSU_Waehler != "NA")
data_CDU$CDU_CSU_Waehler <- as.factor(data_CDU$CDU_CSU_Waehler)


#ist die Variable unbalanced?
table(data_CDU$CDU_CSU_Waehler) 
max(table(data_CDU$CDU_CSU_Waehler)/sum(table(data_CDU$CDU_CSU_Waehler))) 


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


#save model to disk 

tree1000_CDU <- RF_CDU2
saveRDS(tree1000_CDU, "./tree1000_CDU.rds")





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
max(table(data_Linke$Linke_Waehler)/sum(table(data_Linke$Linke_Waehler))) 


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


#save model to disk 

tree1000_Linke <- RF_Linke2
saveRDS(tree1000_Linke, "./tree1000_Linke.rds")






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
table(data_SPD$SPD_Waehler) 
max(table(data_SPD$SPD_Waehler)/sum(table(data_SPD$SPD_Waehler))) 


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


#save model to disk 

tree1000_SPD <- RF_SPD2
saveRDS(tree1000_SPD, "./tree1000_SPD.rds")





#######################
#FDP: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_FDP <- data[,c(340, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_FDP$FDP_Waehler)) 
data_FDP <- data_FDP %>% subset(data_FDP$FDP_Waehler != "NA")
data_FDP$FDP_Waehler <- as.factor(data_FDP$FDP_Waehler)


#ist die Variable unbalanced?
table(data_FDP$FDP_Waehler) #very imbalanced
max(table(data_FDP$FDP_Waehler)/sum(table(data_FDP$FDP_Waehler))) 


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

#save model to disk 

tree1000_FDP <- RF_FDP2
saveRDS(tree1000_FDP, "./tree1000_FDP.rds")


#better num.trees: 1000 trees 






#######################
#Nichtwähler: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Nichtwahler <- data[,c(343, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Nichtwahler$Nichtwahler)) 
data_Nichtwahler <- data_Nichtwahler %>% subset(data_Nichtwahler$Nichtwahler != "NA")
data_Nichtwahler$Nichtwahler <- as.factor(data_Nichtwahler$Nichtwahler)


#ist die Variable unbalanced?
table(data_Nichtwahler$Nichtwahler) #very imbalanced
max(table(data_Nichtwahler$Nichtwahler)/sum(table(data_Nichtwahler$Nichtwahler))) 


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


#save model to disk 

tree1000_Nichtwahler <- RF_Nichtwahler2
saveRDS(tree1000_Nichtwahler, "./tree1000_Nichtwahler.rds")






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
table(data_Hardliner$Corona_Hardliner)
max(table(data_Hardliner$Corona_Hardliner)/sum(table(data_Hardliner$Corona_Hardliner))) 

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


#save model to disk

tree1000_Corona_Hardliner <- RFHardliner2
saveRDS(tree1000_Corona_Hardliner, "./tree1000_Corona_Hardliner.rds")




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
max(table(data_Softliner$Corona_Softliner)/sum(table(data_Softliner$Corona_Softliner))) 

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


#save model to disk 

tree1000_Corona_Softliner <- RFSoftliner2
saveRDS(tree1000_Corona_Softliner, "./tree1000_Corona_Softliner.rds")






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
max(table(data_Skeptiker$Corona_Skeptiker)/sum(table(data_Skeptiker$Corona_Skeptiker))) 

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

# save model to disk

tree1000_Corona_Skeptiker <- RFSkeptiker2
saveRDS(tree1000_Corona_Skeptiker, "./tree1000_Corona_Skeptiker.rds")





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


#save model to disk 

tree1000_CoronaLeugner <- RFLeugner2
saveRDS(tree1000_Corona_Leugner, "./tree100_Corona_Leugner.rds")





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
max(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein)/sum(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein)))


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


#save model to disk 

tree1000_CoronaHardliner_num <- RFHard_num2
saveRDS(tree1000_CoronaHardliner_num, "./tree1000_CoronaHardliner_num.rds")






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


#save model to disk 

tree1000_CoronaSoftliner_num <- RFSoft_num2
saveRDS(tree1000_CoronaSoftliner_num, "./tree1000_CoronaSoftliner_num.rds")






#######################
#Skeptiker 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Skeptik_num <- data[,c(279, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe))

#ist die Variable unbalanced?
table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe) #Überhang zu 1
max(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe)/sum(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe))) 


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

#save model to disk 

tree1000_CoronaSkeptiker_num <- RFSkeptiker_num2
saveRDS(tree1000_CoronaSkeptiker_num, "./tree1000_CoronaSkeptiker_num.rds")



#######################
#Leugner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Leugner_num <- data[,c(280, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Leugner_num$Glaube_nicht_an_Corona))

#ist die Variable unbalanced?
table(data_Leugner_num$Glaube_nicht_an_Corona) #imbalanced!
max(table(data_Leugner_num$Glaube_nicht_an_Corona)/sum(table(data_Leugner_num$Glaube_nicht_an_Corona)))


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

#save model to disk 

tree1000_CoronaLeugner_num <- RFLeugner_num2
saveRDS(tree1000_CoronaLeugner_num, "./tree1000_CoronaLeugner_num.rds")





#######################
#Einkommensgruppe: Categorical (3 Gruppen: hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Einkommen <- data[,c(315, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Einkommen$Einkommensgruppe)) 
data_Einkommen <- data_Einkommen %>% subset(data_Einkommen$Einkommensgruppe != "NA")


#ist die Variable unbalanced?
table(data_Einkommen$Einkommensgruppe) 
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


#save model to disk 

tree1000_Einkommen <- RFEinkommen_2
saveRDS(tree1000_Einkommen, "./tree1000_Einkommen.rds")





#######################
#über oder unter Durchschnittseinkommen (2000€): binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Durchschnittseinkommen <- data[,c(346, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Durchschnittseinkommen$Durchschnittseinkommen)) #122 NAs
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


#save model to disk 

tree1000_Durchschnittseinkommen <- RFDurchschnittseinkommen2
saveRDS(tree1000_Durchschnittseinkommen, "./tree1000_Durchschnittseinkommen.rds")





#######################
#Green Values: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green1 <- data[,c(305, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green1$Green_Values)) #keine NAs

#ist die Variable unbalanced?
table(data_Green1$Green_Values) 
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values))) 


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


#save model to disk 

tree1000_GreenValues_num <- RFGreen1_2
saveRDS(tree1000_GreenValues_num, "./tree1000_GreenValues_num.rds")





#######################
#Green Values 2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green2 <- data[,c(306, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green2$Green2))  

#ist die Variable unbalanced?
table(data_Green2$Green2) 
max(table(data_Green2$Green2)/sum(table(data_Green2$Green2))) 


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

#save model to disk 

tree1000_GreenValues_bin <- RFGreen2_2
saveRDS(tree1000_GreenValues_bin, "./tree1000_GreenValues_bin.rds")




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

#save model to disk 

tree1000_Beschaeftigung <- RFBeschaeftigung1
saveRDS(tree1000_Beschaeftigung, "./tree1000_Beschaeftigung.rds")





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


#save model to disk 

tree1000_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtRF1
saveRDS(tree1000_Arbeitend_oder_nicht, "./tree1000_Arbeitend_oder_nicht.rds")




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

#save model to disk 

tree1000_Bildungsgruppe <- RFBildungsgruppe1
saveRDS(tree1000_Bildungsgruppe, "./tree1000_Bildungsgruppe.rds")





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

#save model to disk 

tree1000_Religion <- RFReligion1
saveRDS(tree1000_Religion, "./tree1000_Religion.rds")





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

#save model to disk 

tree1000_Religioes <- modelReligioesRF1
saveRDS(tree1000_Religioes, "./tree1000_Religioes.rds")






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


#save model to disk 

tree1000_Islam_Christ <- modelIslamChristRF1
saveRDS(tree1000_Islam_Christ , "./tree1000_Islam_Christ .rds")



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

#save model to disk 

tree1000_Migrationshintergrund <- modelMigrationshintergrundRF1
saveRDS(tree1000_Migrationshintergrund, "./tree1000_Migrationshintergrund.rds")




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

#save model to disk 

tree1000_Sexuelle_Orientierung <- RFSexuelle_Orientierung1
saveRDS(tree1000_Sexuelle_Orientierung, "./tree1000_Sexuelle_Orientierung.rds")





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

#save model to disk 

tree1000_Hetero <- modelHeteroRF1
saveRDS(tree1000_Hetero, "./tree1000_Hetero.rds")





####################
#Allein vs. in Beziehung: binär
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# Columns wählen
data_AlleinBeziehung <- data[,c(316, 27:255)]


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_AlleinBeziehung$Allein_vs_Beziehung = as.factor(data_AlleinBeziehung$Allein_vs_Beziehung)


#Gibt es NAs in der DV?
sum(is.na(data_AlleinBeziehung$Allein_vs_Beziehung)) 
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_AlleinBeziehung <- data_AlleinBeziehung %>% subset(data_AlleinBeziehung$Allein_vs_Beziehung != "NA")


#ist die Variable unbalanced?
table(data_AlleinBeziehung$Allein_vs_Beziehung) #Verteilung in Ordnung
max(table(data_AlleinBeziehung$Allein_vs_Beziehung)/sum(table(data_AlleinBeziehung$Allein_vs_Beziehung)))



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



#save model to disk 

tree1000_AlleinBeziehung <- modelAlleinBeziehungRF1
saveRDS(tree1000_AlleinBeziehung, "./tree1000_AlleinBeziehung.rds")






#######################
#Beziehungsstatus: Categorical (5 Gruppen: Geschieden, In einer Beziehung, Single, Verheiratet, Verwitwet)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Beziehungsstatus <- data[,c(18, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Beziehungsstatus$Beziehungsstatus))
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


#save model to disk 

tree1000_Beziehungsstatus <- RFBeziehungsstatus1
saveRDS(tree1000_Beziehungsstatus, "./tree1000_Beziehungsstatus.rds")



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
max(table(data_Kinder$Kinder)/sum(table(data_Kinder$Kinder))) 



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


#save model to disk 

tree1000_Kinder <- modelKinderRF1
saveRDS(tree1000_Kinder, "./tree1000_Kinder.rds")

