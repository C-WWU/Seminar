
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

cols_names <- names(data)  
cols_names

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable

# c(313 --> das ist hier die column wo die Dv drin ist, in dem Fall weiblich_maennlich)
# c(27:255 --> das sind unsere IV's, sprich die Accounts)
data_Extraversion<- data[,c(295, 27:255)]

### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Extraversion <- names(data_Extraversion)
data_Extraversion$Extraversion <- as.numeric(data_Extraversion$Extraversion)

#Gibt es NAs in der DV?
sum(is.na(data_Extraversion$Extraversion)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Extraversion <- data_Extraversion%>% filter(Extraversion != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Extraversion$Extraversion, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

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
  #sampling = "smote", #wenn sampling, dann hier anpassen und für alle drei Varianten ausprobieren!! (up, down, smote)
  search = "grid"
)


####-------tree 1: mtry, splitrule and min.node.size tunen --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

set.seed(667)
modelExtraversionRF <- train(Extraversion ~ ., 
                      data=train_dfExtraversion,
                      tuneGrid = myGrid,
                      method="ranger",
                      metric= "RMSE", # numeric: RMSE; categorical: Kappa; binary: ROC
                      na.action = na.omit,
                      num.tree = 500,
                      trControl = myControl, 
                      importance = 'impurity')

# Print model to console

modelExtraversionRF
summary(modelExtraversionRF)
plot(modelExtraversionRF)


#best mtry = 10, splitrule = extratrees, min.node.size = 15

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelExtraversionRF, newdata=test_dfExtraversion)

#check performance measures --> für numerisch
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionRF

spearmanExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionRF


#save the best mtry 

bestmtry <- modelExtraversionRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!
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

# Print model to console

modelExtraversionRF1
summary(modelExtraversionRF1)
plot(modelExtraversionRF1)

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelExtraversionRF1, newdata=test_dfExtraversion)


#check performance measures --> für numerisch
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversion1

spearmanExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversion1



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(1997)
modelExtraversionfinal <- modelExtraversionRF1

# Print model
### hier den Model namen ändern
print(modelExtraversionfinal)

#output in terms of regression coefficients
summary(modelExtraversionfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelExtraversionfinal)
plot(varImp(modelExtraversionfinal), 20, main = "Extraversion")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelExtraversionfinal, newdata=test_dfExtraversion)


#check performance measures --> für numerisch
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionfinal

spearmanExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

###anpassen: name vom dataset

imp <- importance(modelExtraversionfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

###Model umbenennen

PartialPlots <- modelExtraversionfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Extraversion")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

