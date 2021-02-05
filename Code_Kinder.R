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

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names

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



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

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
  summaryFunction = twoClassSummary, #nur für binär; Wenn das benutzt wird, auch ClassProbs = True setzen!
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", #wenn sampling, dann hier anpassen und für alle drei Varianten ausprobieren!! (up, down, smote)
  search = "grid",
)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelKinderRF <- train(Kinder ~ ., 
                           data=train_dfKinder,
                           tuneGrid = myGrid,
                           method="ranger",
                           metric= "ROC", # numeric: RMSE; categorical: Kappa; binary: ROC
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

# Create confusion matrix --> nur für classification (binär oder categorical)
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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelKinderRF1, newdata=test_dfKinder)

# Create confusion matrix --> nur für classification
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




#fit model with num.trees = 1000 trees (better predictions)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

modelKinderFinal <- modelKinderRF1

# Print model
print(modelKinderFinal)

#output in terms of regression coefficients
summary(modelKinderFinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(modelKinderFinal)
plot(varImp(modelKinderFinal), 20, main = "weiblich_maennlich")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions3 <- predict(modelKinderFinal, newdata=test_dfKinder)

# Create confusion matrix --> nur für classification
confusionMatrix(data=predictions3, test_dfKinder$Kinder)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderFinal %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#compare different ROC-plots
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

# Plot ROC curve for all 5 models
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

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
saveRDS(besttree_Kinder, "./tree_Kinder.rds")

#load the model

besttree_Kinder <- readRDS("./tree_Kinder.rds")
print(besttree_Kinder)

