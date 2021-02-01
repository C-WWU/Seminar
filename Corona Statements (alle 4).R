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
#Corona Hardliner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


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

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHardliner1, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "unter2000"])
  
}

#model1 auc: 0.6346
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



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = x)

set.seed(1997)
RFHardliner2 <- train(Corona_Hardliner ~ ., 
                                   data=train_dfHardliner,
                                   tuneGrid = myGrid1,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFHardliner2
summary(RFHardliner2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHardliner2, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "unter2000"])
  
}

#model1 auc
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

# Plot ROC curve for all 3 models

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
RFHardliner_fin <- train(Corona_Hardliner ~ ., 
                                      data=train_dfHardliner, 
                                      method="ranger", metric= "ROC",
                                      tuneGrid = myGrid1,
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl1, 
                                      importance = 'impurity')

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
      predict(model, data, type = "prob")[, "unter2000"])
  
}

#model1 auc
RFHardliner_fin %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFHardliner1,
                   Model2 = RFHardliner2,
                   Model3 = RFHardliner_fin)

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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFHardliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFHardliner_fin

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

besttree_Hardliner <- RFHardliner_fin
saveRDS(besttree_Hardliner, "./tree_Hardliner.rds")

#load the model

besttree_Hardliner <- readRDS("./tree_Hardliner.rds")
print(besttree_Hardliner)






#######################
#Corona Softliner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Softliner <- data[,c(308, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Softliner$Corona_Softliner)) #no NAs
data_Softliner <- data_Softliner %>% subset(data_Softliner$Corona_Softliner != "NA")


#ist die Variable unbalanced?
table(data_Softliner$Corona_Softliner) #should correct for imbalancedness
max(table(data_Softliner$Corona_Softliner)/sum(table(data_Softliner$Corona_Softliner))) #no information rate 61%

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

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSoftliner1, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc: 0.7385
RFSoftliner1 %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#check ROC plots
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



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RFSoftliner2 <- train(Corona_Softliner ~ ., 
                                   data=train_dfSoftliner,
                                   tuneGrid = myGrid1,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFSoftliner2
summary(RFSoftliner2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoftliner2, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc
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
RFSoftliner_fin <- train(Corona_Softliner ~ ., 
                                      data=train_dfSoftliner, 
                                      method="ranger", metric= "ROC",
                                      tuneGrid = myGrid1,
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl1, 
                                      importance = 'impurity')

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
      predict(model, data, type = "prob")[, "1"])
  
}

#model auc: 0.7436
RFSoftliner_fin %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSoftliner1,
                   Model2 = RFSoftliner2,
                   Model3 = RFSoftliner_fin)

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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFSoftliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFSoftliner_fin

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

besttree_Softliner <- RFSoftliner_fin
saveRDS(besttree_Softliner, "./tree_Softliner.rds")

#load the model

besttree_Softliner <- readRDS("./tree_Softliner.rds")
print(besttree_Softliner)






#######################
#Corona Skeptiker: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Durchschnittseinkommen <- data[,c(346, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Durchschnittseinkommen$Durchschnittseinkommen)) #122 NAs
data_Durchschnittseinkommen <- data_Durchschnittseinkommen %>% subset(data_Durchschnittseinkommen$Durchschnittseinkommen != "NA")


#ist die Variable unbalanced?
table(data_Durchschnittseinkommen$Durchschnittseinkommen) #in Ordnung, ca. 1:2
max(table(data_Durchschnittseinkommen$Durchschnittseinkommen)/sum(table(data_Durchschnittseinkommen$Durchschnittseinkommen))) #no information rate 61%

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
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc: 0.7385
RFDurchschnittseinkommen1 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#check ROC plots
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



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RFDurchschnittseinkommen2 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid1,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen2
summary(RFDurchschnittseinkommen2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDurchschnittseinkommen2, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc
RFDurchschnittseinkommen2 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
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
RFDurchschnittseinkommen_fin <- train(Durchschnittseinkommen ~ ., 
                                      data=train_dfDurchschnittseinkommen, 
                                      method="ranger", metric= "ROC",
                                      tuneGrid = myGrid1,
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl1, 
                                      importance = 'impurity')

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
      predict(model, data, type = "prob")[, "1"])
  
}

#model auc: 0.7436
RFDurchschnittseinkommen_fin %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFDurchschnittseinkommen1,
                   Model2 = RFDurchschnittseinkommen2,
                   Model3 = RFDurchschnittseinkommen_fin)

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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFDurchschnittseinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFDurchschnittseinkommen_fin

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

besttree_Durchschnittseinkommen <- RFDurchschnittseinkommen_fin
saveRDS(besttree_Durchschnittseinkommen, "./tree_Durchschnittseinkommen.rds")

#load the model

besttree_Durchschnittseinkommen <- readRDS("./tree_Durchschnittseinkommen.rds")
print(besttree_Durchschnittseinkommen)








#######################
#Corona Leugner: binär
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Durchschnittseinkommen <- data[,c(346, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Durchschnittseinkommen$Durchschnittseinkommen)) #122 NAs
data_Durchschnittseinkommen <- data_Durchschnittseinkommen %>% subset(data_Durchschnittseinkommen$Durchschnittseinkommen != "NA")


#ist die Variable unbalanced?
table(data_Durchschnittseinkommen$Durchschnittseinkommen) #in Ordnung, ca. 1:2
max(table(data_Durchschnittseinkommen$Durchschnittseinkommen)/sum(table(data_Durchschnittseinkommen$Durchschnittseinkommen))) #no information rate 61%

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

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFDurchschnittseinkommen1, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc: 0.7385
RFDurchschnittseinkommen1 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#check ROC plots
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



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

#set random seed again 
set.seed(1997)

myGrid1 <- expand.grid(mtry = xx, splitrule ="extratrees", min.node.size = xx)

set.seed(1997)
RFDurchschnittseinkommen2 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid1,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen2
summary(RFDurchschnittseinkommen2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDurchschnittseinkommen2, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "1"])
  
}

#model1 auc
RFDurchschnittseinkommen2 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
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
RFDurchschnittseinkommen_fin <- train(Durchschnittseinkommen ~ ., 
                                      data=train_dfDurchschnittseinkommen, 
                                      method="ranger", metric= "ROC",
                                      tuneGrid = myGrid1,
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl1, 
                                      importance = 'impurity')

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
      predict(model, data, type = "prob")[, "1"])
  
}

#model auc: 0.7436
RFDurchschnittseinkommen_fin %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFDurchschnittseinkommen1,
                   Model2 = RFDurchschnittseinkommen2,
                   Model3 = RFDurchschnittseinkommen_fin)

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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFDurchschnittseinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFDurchschnittseinkommen_fin

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

besttree_Durchschnittseinkommen <- RFDurchschnittseinkommen_fin
saveRDS(besttree_Durchschnittseinkommen, "./tree_Durchschnittseinkommen.rds")

#load the model

besttree_Durchschnittseinkommen <- readRDS("./tree_Durchschnittseinkommen.rds")
print(besttree_Durchschnittseinkommen)




