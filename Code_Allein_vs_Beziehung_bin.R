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

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

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
  summaryFunction = twoClassSummary, #nur für binär; Wenn das benutzt wird, auch ClassProbs = True setzen!
  classProbs = TRUE,
  allowParallel=TRUE,
  search = "grid",
)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

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

# Create confusion matrix --> nur für classification (binär oder categorical)
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


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelAlleinBeziehungRF1, newdata=test_dfAlleinBeziehung)

# Create confusion matrix --> nur für classification
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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions2 <- predict(modelAlleinBeziehungfinal, newdata=test_dfAlleinBeziehung)

# Create confusion matrix --> nur für classification
confusionMatrix(data=predictions2, test_dfAlleinBeziehung$Allein_vs_Beziehung)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungfinal %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#compare different ROC-plots
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


#checking direction of the 10 most important variables

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
saveRDS(besttree_allein_beziehung, "./allein_beziehung.rds")

#load the model

besttree_allein_beziehung <- readRDS("./allein_beziehung.rds")
print(besttree_allein_beziehung)