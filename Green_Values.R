
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

###Miriam Stand: 
#Green 1: Code steht, muss angepasst werden
#Green2: Code steht, muss angepasst werden


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
table(data_Green1$Green_Values) #Überhang zu höheren Werten, aber nicht zu stark
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values))) #no information rate 7,65%


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green1$Green_Values, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen1 <- data_Green1[index,]
test_dfGreen1 <- data_Green1[-index,]


#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------


#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)



# Specify linear regression model with most important IV's

#--------------first regression: all parameters-----------------

set.seed(1997)

model1 <- train(Green_Values ~.,
                data=train_dfGreen1,
                method = "glm",
                metric = "RMSE", 
                na.action = na.omit,
                trControl=myControl)

print(model1)
summary(model1)

#variable Importance (predictor variables)

varImp(model1)

#look for most important variables
ImportanceAll1 <- varImp(model1)$importance
ImportanceAll1 <- arrange(ImportanceAll1, desc(Overall))
ImportanceAll1

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(model1, newdata=test_dfGreen1)


# Create confusion matrix

confusionMatrix(data=predictions1, test_dfGreen1$Green_Values)



#------second regression: ridge/lasso for shrinking model---------

set.seed(1998)

myGrid <- expand.grid(alpha = 0:1,
                      lambda = seq(0.0001, 1, length = 100))

model2 <- train(Green_Values ~ .,
                data=train_dfGreen1,
                method = "glmnet", 
                metric = "RMSE", 
                na.action = na.omit,
                tuneGrid = myGrid,
                trControl=myControl) 

print(model2)
summary(model2)
coef(model2$finalModel, model2$finalModel$lambdaOpt)


varImp(model2)

ImportanceAll2 <- varImp(model2)$importance
ImportanceAll2 <- arrange(ImportanceAll2, desc(Overall))
ImportanceAll2


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions2 <- predict(model2, newdata=test_dfGreen1)


# Create confusion matrix

confusionMatrix(data=predictions2, test_dfGreen1$Green_Values)


#------------third regression: specify ideal model--------------

set.seed(1999)

model3 <- train(Green_Values ~ SimplyV + Alice_Weidel + Mady_Morrison + AfD + Michael_Wendler + Montana_Black + Julien_Co + PETA_Deutschland + Christian_Lindner + Capital_Bra + Pflanzlich_stark + Fridays_for_Future + Evangelisch_de + Inscope21 + Luisa_Neubauer + Mero + Buendnis_90_Die_Gruenen + Lillydoo + Boehse_Onkelz + heute_show + Dein_Beichtstuhl + Dieter_Nuhr + Made_My_Day + Just_Spices + Sallys_Welt + Microsoft_Deutschland + adidas_Deutschland,
                data = train_dfGreen1,
                method = "lm", 
                metric = "RMSE",
                na.action = na.omit,
                trControl=myControl) 

print(model3)
summary(model3)

varImp(model3)

ImportanceAll3 <- varImp(model3)$importance
ImportanceAll3 <- arrange(ImportanceAll3, desc(Overall))
ImportanceAll3


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions3 <- predict(model3, newdata=test_dfGreen1)


# Create confusion matrix

confusionMatrix(data=predictions3, test_dfGreen1$Green_Values)
RMSE(predictions3, test_dfGreen1$Green_Values)
R2(predictions3, test_dfGreen1$Green_Values)

#----------save best regression model----------------------

bestregression_Green1 <- model3


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

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))


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

#best mtry:
#splitrule:
#min.node.size used:

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




#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)

myGrid <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)

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

#num.trees xx performs better


####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)
myGrid <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
RFGreen1_fin <- train(Green_Values ~ ., 
                           data=train_dfGreen1, 
                           method="ranger", metric= "RMSE",
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                      na.action = na.omit,
                           importance = 'impurity')

# Print model
RFGreen1_fin
summary(RFGreen1_fin)
plot(RFGreen1_fin)

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
###anpassen: name vom dataset


imp <- importance(RFGreen1_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

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



#--------------------------------------LOGISTIC REGRESSION/ LINEAR REGRESSION-----------------------------------------------------


#-----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

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

myControl2 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "up", 
  search = "grid"
)

myControl3 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "down", 
  search = "grid"
)




# Specify linear regression model with most important IV's

#--------------first regression with Control1: all parameters-----------------

set.seed(1997)

model1.1 <- train(Green2 ~.,
                data=train_dfGreen2,
                method = "glm", 
                metric = "ROC", 
                na.action = na.omit,
                trControl=myControl1)

print(model1.1)
summary(model1.1)

#variable Importance (predictor variables)

varImp(model1.1)

#look for most important variables
ImportanceAll1.1 <- varImp(model1.1)$importance
ImportanceAll1.1 <- arrange(ImportanceAll1.1, desc(Overall))
ImportanceAll1.1

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.1 <- predict(model1.1, newdata=test_dfGreen2)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.1), as.factor(test_dfGreen2$Green2))


#--------------first regression with Control2: all parameters-----------------

set.seed(1997)

model1.2 <- train(Green2 ~.,
                data=train_dfGreen2,
                method = "glm", 
                metric = "ROC", 
                na.action = na.omit,
                trControl=myControl2)

print(model1.2)
summary(model1.2)

#variable Importance (predictor variables)

varImp(model1.2)

#look for most important variables
ImportanceAll1.2 <- varImp(model1.2)$importance
ImportanceAll1.2 <- arrange(ImportanceAll1.2, desc(Overall))
ImportanceAll1.2

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.2 <- predict(model1.2, newdata=test_dfGreen2)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.2), as.factor(test_dfGreen2$Green2))


#--------------first regression with Control3: all parameters-----------------

set.seed(1997)

model1.3 <- train(Green2 ~.,
                data=train_dfGreen2,
                method = "glm", 
                metric = "ROC", 
                na.action = na.omit,
                trControl=myControl3)

print(model1.3)
summary(model1.3)

#variable Importance (predictor variables)

varImp(model1.3)

#look for most important variables
ImportanceAll1.3 <- varImp(model1.3)$importance
ImportanceAll1.3 <- arrange(ImportanceAll1.3, desc(Overall))
ImportanceAll1.3

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions1.3 <- predict(model1.3, newdata=test_dfGreen2)


# Create confusion matrix

confusionMatrix(data=as.factor(predictions1.3), as.factor(test_dfGreen2$Green2))

##upsampling (Control2) works best, will be used further --> best ROC and Sens, second best prediction accuracy


#------second regression: ridge/lasso for shrinking model---------

set.seed(1998)

myGrid <- expand.grid(alpha = 0:1,
                      lambda = seq(0.0001, 1, length = 100))

model2 <- train(Green2 ~ .,
                data=train_dfGreen2,
                method = "glmnet", 
                metric = "ROC", 
                na.action = na.omit,
                tuneGrid = myGrid,
                trControl=myControl2) 

print(model2)
summary(model2)
coef(model2$finalModel, model2$finalModel$lambdaOpt)


varImp(model2)

ImportanceAll2 <- varImp(model2)$importance
ImportanceAll2 <- arrange(ImportanceAll2, desc(Overall))
ImportanceAll2


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions2 <- predict(model2, newdata=test_dfGreen2)


# Create confusion matrix

confusionMatrix(as.factor(predictions2), as.factor(test_dfGreen2$Green2))


#------------third regression: specify ideal model--------------

set.seed(1999)

model3 <- train(Green2 ~ Pflanzlich_stark + Evangelisch_de + Buendnis_90_Die_Gruenen + Alex_Koch + Tiere_suchen_ein_Zuhause + Mady_Morrison + Fridays_for_Future + Parookaville + Riccardo_Simonetti + Alice_Weidel + Germanroamers + Guido_Maria_Kretschmer + Aldi_Sued + Christian_Lindner + Querdenken711 + Leonie_Hanne + Louisa_Dellert + Julien_Co + AfD + Backen_de + Berlin_Tag_und_Nacht + Michael_Bully_Herbig + Faktastisch + Germanys_next_Topmodel + heute_show + Laser_Luca + Love_Island + Mario_Barth + Made_My_Day + Palina_Rojinski + Sebastian_Fitzek + The_Voice_of_Germany + Aldi_Nord + Einfach_Tasty + McDonalds_Deutschland + Sallys_Welt + Apotheken_Umschau + BILD_Zeitung + Frankfurter_Allgemeine_Zeitung + Quarks_Co + RTL_Aktuell + NYX_Professional_Makeup + IKEA + Boehse_Onkelz + LionTTV + Michael_Wendler + Bundeswehr + CDU + Die_Linke + Die_Partei + FDP + adidas_Deutschland + Inscope21 + Manuel_Neuer + Oceans_Apart + Urlaubspiraten + Beyonce + Kim_Kardashian_West + National_Geographic,
                data = train_dfGreen2,
                method = "glm", 
                metric = "ROC",
                na.action = na.omit,
                trControl=myControl2) 

print(model3)
summary(model3)

varImp(model3)

ImportanceAll3 <- varImp(model3)$importance
ImportanceAll3 <- arrange(ImportanceAll3, desc(Overall))
ImportanceAll3


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

predictions3 <- as.factor(predict(model3, newdata=test_dfGreen2))


# Create confusion matrix

confusionMatrix(data=predictions3, as.factor(test_dfGreen2$Green2))


#----------save best regression model----------------------

bestregression_Green1 <- model3


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

myControl2 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "up", 
  search = "grid"
)

myControl3 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "down", 
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
RFGreen2_11 <- train(Green2 ~ ., 
                    data=train_dfGreen2,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')
set.seed(1997)
RFGreen2_12 <- train(Green2 ~ ., 
                     data=train_dfGreen2,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 500,
                     na.action = na.omit,
                     trControl = myControl2, 
                     importance = 'impurity')
set.seed(1997)
RFGreen2_13 <- train(Green2 ~ ., 
                     data=train_dfGreen2,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 500,
                     na.action = na.omit,
                     trControl = myControl3, 
                     importance = 'impurity')

# Print models to console

RFGreen2_11
summary(RFGreen2_11)
plot(RFGreen2_11)
#mtry = xx, extratrees, min.node.size = xx


RFGreen2_12
summary(RFGreen2_12)
plot(RFGreen2_12)
#mtry = xx, extratrees, min.node.size = xx


RFGreen2_13
summary(RFGreen2_13)
plot(RFGreen2_13)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen2_11, newdata=test_dfGreen2)
predictions <- predict(RFGreen2_12, newdata=test_dfGreen2)
predictions <- predict(RFGreen2_13, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGreen2$Green2))

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
                 predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFGreen2_11 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model2 auc
RFGreen2_12 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model3 auc
RFGreen2_13 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RFGreen2_11,
                   Model2 = RFGreen2_12,
                   Model3 = RFGreen2_13)

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

# Plot ROC curve for all 3 models

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

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(1997)
RFGreen2_21 <- train(Green2 ~ ., 
                     data=train_dfGreen2,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 1000,
                     na.action = na.omit,
                     trControl = myControl1, 
                     importance = 'impurity')
set.seed(1997)
RFGreen2_22 <- train(Green2 ~ ., 
                     data=train_dfGreen2,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 1000,
                     na.action = na.omit,
                     trControl = myControl2, 
                     importance = 'impurity')
set.seed(1997)
RFGreen2_23 <- train(Green2 ~ ., 
                     data=train_dfGreen2,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "ROC",
                     num.tree = 1000,
                     na.action = na.omit,
                     trControl = myControl3, 
                     importance = 'impurity')

# Print models to console

RFGreen2_21
summary(RFGreen2_21)
plot(RFGreen2_21)
  #mtry = xx, extratrees, min.node.size = xx

RFGreen2_22
summary(RFGreen2_22)
plot(RFGreen2_22)
  #mtry = xx, extratrees, min.node.size = xx


RFGreen2_23
summary(RFGreen2_23)
plot(RFGreen2_23)
  #mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFGreen2_21, newdata=test_dfGreen2)
predictions2 <- predict(RFGreen2_22, newdata=test_dfGreen2)
predictions3 <- predict(RFGreen2_23, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGreen2$Green2))

#save the best mtry 

bestmtry <- modelGeschlechtRF$bestTune$mtry

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFGreen2_21 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model2 auc
RFGreen2_22 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model3 auc
RFGreen2_23 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RFGreen2_21,
                   Model2 = RFGreen2_22,
                   Model3 = RFGreen2_23)

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
myGrid1 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
myGrid2 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)
myGrid3 <- expand.grid(mtry = 10, splitrule ="extratrees", min.node.size = 15)

set.seed(1997)
RFGreen2_fin1 <- train(Green2 ~ ., 
                      data=train_dfGreen2, 
                      method="ranger", metric= "ROC",
                      tuneGrid = myGrid1,
                      na.action = na.omit,
                      num.tree = 500,
                      trControl = myControl1, 
                      na.action = na.omit,
                      importance = 'impurity')
set.seed(1997)
RFGreen2_fin2 <- train(Green2 ~ ., 
                       data=train_dfGreen2, 
                       method="ranger", metric= "ROC",
                       tuneGrid = myGrid2,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl2, 
                       na.action = na.omit,
                       importance = 'impurity')
set.seed(1997)
RFGreen2_fin3 <- train(Green2 ~ ., 
                       data=train_dfGreen2, 
                       method="ranger", metric= "ROC",
                       tuneGrid = myGrid3,
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl3,
                       na.action = na.omit, 
                       importance = 'impurity')

# Print models
RFGreen2_fin1
summary(RFGreen2_fin1)
plot(RFGreen2_fin1)
#mtry = xx, extratrees, min.node.size = xx


RFGreen2_fin2
summary(RFGreen2_fin2)
plot(RFGreen2_fin2)
#mtry = xx, extratrees, min.node.size = xx


RFGreen2_fin3
summary(RFGreen2_fin3)
plot(RFGreen2_fin3)
#mtry = xx, extratrees, min.node.size = xx


#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFGreen2_fin1)
plot(varImp(RFGreen2_fin1), 20, main = "Green_Values")

varImp(RFGreen2_fin2)
plot(varImp(RFGreen2_fin2), 20, main = "Green_Values")

varImp(RFGreen2_fin3)
plot(varImp(RFGreen2_fin3), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFGreen2_fin1, newdata=test_dfGreen2)
predictions2 <- predict(RFGreen2_fin2, newdata=test_dfGreen2)
predictions3 <- predict(RFGreen2_fin3, newdata=test_dfGreen2)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model1 auc
RFGreen2_fin1 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model2 auc
RFGreen2_fin2 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#model3 auc
RFGreen2_fin3 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RFGreen2_fin1,
                   Model2 = RFGreen2_fin2,
                   Model3 = RFGreen2_fin3)

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

# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#best forest: model x (sampling = xx)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables
###anpassen: name vom dataset


imp <- importance(RFGreen2_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFGreen2_fin

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

besttree_Green2 <- RFGreen2_fin123
saveRDS(besttree_Green2, "./tree_Green2.rds")

#load the model

Tree_Green2 <- readRDS("./tree_Green2.rds")
print(Tree_Green2)



