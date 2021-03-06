
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
#Green Values: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

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

#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#best mtry:10
#splitrule: extratrees
#min.node.size used: 10

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

#checking direction of the 10 most important variables

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
saveRDS(besttree_Green1, "./tree_Green1.rds")

#load the model

super_model <- readRDS("./tree_Green1.rds")
print(super_model)








#######################
#Green Values 2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#define data for analysis
data_Green2 <- data[,c(306, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green2$Green2)) #keine NAs

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


#set tuning grid

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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
#mtry = 13, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFGreen2_1, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
                 predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6346
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
#mtry = 13, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFGreen2_2, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 0,6341
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

#better num.trees: 500 trees sorts 1 person more correctly


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
RFGreen2_fin <- RFGreen2_1

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

#model auc: 0,6346
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


#checking direction of the 10 most important variables

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
saveRDS(besttree_Green2, "./tree_Green2.rds")

#load the model

besttree_Green2 <- readRDS("./tree_Green2.rds")
print(besttree_Green2)



