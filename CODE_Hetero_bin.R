
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

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable

# c(313 --> das ist hier die column wo die Dv drin ist, in dem Fall weiblich_maennlich)
# c(27:255 --> das sind unsere IV's, sprich die Accounts)
data_Hetero <- data[,c(335, 27:255)]


### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 
###nur für binär/categorical
cols_Hetero <- names(data_Hetero)


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_Hetero$Heterosexuell = as.factor(data_Hetero$Heterosexuell)


#Gibt es NAs in der DV?
sum(is.na(data_Hetero$Heterosexuell)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Hetero <- data_Hetero %>% subset(data_Hetero$Heterosexuell != "NA")


#ist die Variable unbalanced?
table(data_Hetero$Heterosexuell) #Verteilung in Ordnung
max(table(data_Hetero$Heterosexuell)/sum(table(data_Hetero$Heterosexuell))) #no information rate 61%



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Hetero$Heterosexuell, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

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
  summaryFunction = twoClassSummary, #nur für binär; Wenn das benutzt wird, auch ClassProbs = True setzen!
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", #wenn sampling, dann hier anpassen und für alle drei Varianten ausprobieren!! (up, down, smote)
  search = "grid",
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


modelHeteroRF <- train(Heterosexuell ~ ., 
                       data=train_dfHetero,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC", # numeric: RMSE; categorical: Kappa; binary: ROC
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelHeteroRF
summary(modelHeteroRF)
plot(modelHeteroRF)

#best mtry = 18, splitrule = extratrees, min.node.size = 10

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelHeteroRF, newdata=test_dfHetero)

# Create confusion matrix --> nur für classification (binär oder categorical)
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)

#save the best mtry 

bestmtry <- modelHeteroRF$bestTune$mtry

#check for AUC 
#####(nur binär und kategorisch)
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF %>%
  test_roc(data = test_dfHetero) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#compare different ROC-plots
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

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#------------------------------------------------Mit Resampling SMOTE-------------------------------------------------


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


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


modelKinderRFSMOTE <- train(Kinder ~ ., 
                            data=train_dfKinder,
                            tuneGrid = myGrid,
                            method="ranger",
                            metric= "ROC", # numeric: RMSE; categorical: Kappa; binary: ROC
                            na.action = na.omit,
                            num.tree = 500,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelKinderRFSMOTE
summary(modelKinderRFSMOTE)
plot(modelKinderRFSMOTE)

#best mtry = xx, splitrule = xx, min.node.size = xx

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelKinderRFSMOTE, newdata=test_dfKinder)

# Create confusion matrix --> nur für classification (binär oder categorical)
confusionMatrix(data=predictions, test_dfKinder$Kinder)

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

#check for AUC 
#####(nur binär und kategorisch)
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Yes"])
  
}

modelKinderRFSMOTE %>%
  test_roc(data = test_dfKinder) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#compare different ROC-plots
model_list <- list(M1 = modelKinderRF,
                   M2 = modelKinderRFSMOTE)

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

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!

set.seed(1997)

myGrid = expand.grid(mtry = 10,   #anpassen!
                     splitrule = "extratrees", #anpassen!
                     min.node.size = 15)   #anpassen!


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

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelHeteroRF1, newdata=test_dfHetero)

# Create confusion matrix --> nur für classification
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
#####(nur binär und kategorisch) (von hier bis Ende des Abschnitts)
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF1 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

###nur für binär
#compare different ROC-plots
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

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)




#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(1997)
myGrid <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
modelHeterofinal <- train(Heterosexuell ~ ., 
                           data=train_dfHetero, 
                           method="ranger", metric= "ROC", # hier bei metric kann man sich auch die Accuracy ausgeben lassen
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 500,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model
### hier den Model namen ändern
print(modelHeterofinal)

#output in terms of regression coefficients
summary(modelHeterofinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelHeterofinal)
plot(varImp(modelHeterofinal), 20, main = "Heterosexuell")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelHeterofinal, newdata=test_dfHetero)

# Create confusion matrix --> nur für classification
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
#####(nur binär und kategorisch)
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeterofinal %>%
  test_roc(data = test_dfHetero) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#compare different ROC-plots
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

# Plot ROC curve for all 5 models
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

###anpassen: name vom dataset

imp <- importance(modelGeschlechtRF$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

###Model umbenennen

PartialPlots <- modelGeschlechtRF

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

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)