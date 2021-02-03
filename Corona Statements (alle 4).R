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

#set random seed again 
set.seed(1997)

#set grid for testing ideal mtry and min.node.size
myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))




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
#mtry = 14, extratrees, min.node.size = 5


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHardliner1, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc: 0.5903
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
#mtry = 11, extratrees, min.node.size = 15


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHardliner2, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc: 0,5819
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

#better num.trees: 500 trees


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


#checking direction of the 10 most important variables

imp <- importance(RFHardliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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
saveRDS(besttree_Hardliner, "./tree_Hardliner_janein.rds")

#load the model

besttree_Hardliner <- readRDS("./tree_Hardliner_janein.rds")
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
max(table(data_Softliner$Corona_Softliner)/sum(table(data_Softliner$Corona_Softliner))) #no information rate 73%

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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



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
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSoftliner1, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6721
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
#mtry = 11, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoftliner2, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6735
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


#checking direction of the 10 most important variables

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
saveRDS(besttree_Softliner, "./tree_Softlinerjanein.rds")

#load the model

besttree_Softliner <- readRDS("./tree_Softlinerjanein.rds")
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


#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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
#mtry = 19, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSkeptiker1, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0.7532
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
#mtry = 11, extratrees, min.node.size = 15

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSkeptiker2, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,7689
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

#better num.trees: 1000 trees --> better predictions!


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFSkeptiker_fin <- RFSkeptiker2

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


#checking direction of the 10 most important variables


imp <- importance(RFSkeptiker_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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
saveRDS(besttree_Skeptiker, "./tree_Skeptikerjanein.rds")

#load the model

besttree_Skeptiker <- readRDS("./tree_Skeptikerjanein.rds")
print(besttree_Skeptiker)








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
data_Leugner <- data[,c(310, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Leugner$Corona_Leugner)) #0 NAs
data_Leugner <- data_Leugner %>% subset(data_Leugner$Corona_Leugner != "NA")


#ist die Variable unbalanced?
table(data_Leugner$Corona_Leugner) #very imbalanced!!
max(table(data_Leugner$Corona_Leugner)/sum(table(data_Leugner$Corona_Leugner))) #no information rate 0,9385%

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

#set tuning grid 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#model1 auc: 0,6736
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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

imp <- importance(RFLeugner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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
saveRDS(besttree_Leugner, "./tree_Leugner_janein.rds")

#load the model

besttree_Leugner <- readRDS("./tree_Leugner_janein.rds")
print(besttree_Leugner)









#######################
#Hardliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

#define data for analysis
data_Hardliner_num <- data[,c(278, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein)) #0 NAs

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

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

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

#best mtry:11
#splitrule: extratrees
#min.node.size used: 10

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

#checking direction of the 10 most important variables

imp <- importance(RFHard_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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
saveRDS(besttree_Hardliner_num, "./tree_Hard_num.rds")

#load the model

besttree_Hardliner_num <- readRDS("./tree_Hard_num.rds")
print(besttree_Hardliner_num)







#######################
#Softliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

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
  sampling = "smote",
  search = "grid",
)

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#best mtry:13
#splitrule: extratrees
#min.node.size used: 10

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
#min.node.size used: 10

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

#checking direction of the 10 most important variables

imp <- importance(RFSoft_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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
saveRDS(besttree_Softliner_num, "./tree_SoftNum.rds")

#load the model

besttree_Softliner_num <- readRDS("./tree_SoftNum.rds")
print(besttree_Softliner_num)








#######################
#Skeptiker 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

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
  sampling = "smote",
  search = "grid",
)


#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#checking direction of the 10 most important variables

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
saveRDS(besttree_Skeptiker_num, "./tree_Skeptik_num.rds")

#load the model

besttree_Skeptiker_num <- readRDS("./tree_Skeptik_num.rds")
print(besttree_Skeptiker_num)








#######################
#Leugner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

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
  sampling = "smote",
  search = "grid",
)

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#num.trees 500 performs better


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)

RFLeugner_num_fin <- RFLeugner_num1

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

#checking direction of the 10 most important variables

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
saveRDS(besttree_Leugner_num, "./tree_Leugner_num.rds")

#load the model

besttree_Leugner_num <- readRDS("./tree_Leugner_num.rds")
print(besttree_Leugner_num)








