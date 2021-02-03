
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
data_Einkommen <- data[,c(315, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Einkommen$Einkommensgruppe)) #122 NAs
data_Einkommen <- data_Einkommen %>% subset(data_Einkommen$Einkommensgruppe != "NA")


#ist die Variable unbalanced?
table(data_Einkommen$Einkommensgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Einkommen$Einkommensgruppe)/sum(table(data_Einkommen$Einkommensgruppe))) #no information rate 61%

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
  #mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFEinkommen_1, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
      predict(model, data, type = "prob")[, "niedrig"])
  
}

#model1 auc: 
RFEinkommen_1 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()



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
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFEinkommen_2, newdata=test_dfEinkommen)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFEinkommen_2 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFEinkommen_fin <- RFEinkommen_x

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

#model auc: 
RFEinkommen_fin %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFEinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFEinkommen_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittel") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittel") %>%plotPartial


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial



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


#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))



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

#model1 auc: 
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
#mtry = xx, extratrees, min.node.size = xx



# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDurchschnittseinkommen2, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc
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

#better num.trees: xx trees 


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFDurchschnittseinkommen_fin <- RFDurchschnittseinkommen_x

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

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mehr2000") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mehr2000") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Durchschnittseinkommen <- RFDurchschnittseinkommen_fin
saveRDS(besttree_Durchschnittseinkommen, "./tree_Durchschnittseinkommen.rds")

#load the model

besttree_Durchschnittseinkommen <- readRDS("./tree_Durchschnittseinkommen.rds")
print(besttree_Durchschnittseinkommen)



