
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
data_Agreeableness<- data[,c(296, 27:255)]

### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Agreeableness <- names(data_Agreeableness)
data_Agreeableness$Agreeableness <- as.numeric(data_Agreeableness$Agreeableness)

#Gibt es NAs in der DV?
sum(is.na(data_Agreeableness$Agreeableness)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Agreeableness <- data_Agreeableness%>% filter(Agreeableness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Agreeableness$Agreeableness, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

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
modelAgreeablenessRF <- train(Agreeableness ~ ., 
                             data=train_dfAgreeableness,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "RMSE", # numeric: RMSE; categorical: Kappa; binary: ROC
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelAgreeablenessRF
summary(modelAgreeablenessRF)
plot(modelAgreeablenessRF)


#best mtry = 11, splitrule = extratrees, min.node.size = 15

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelAgreeablenessRF, newdata=test_dfAgreeableness)

#check performance measures --> für numerisch
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessRF

spearmanAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessRF


#save the best mtry 

bestmtry <- modelAgreeablenessRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!
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

# Print model to console

modelAgreeablenessRF1
summary(modelAgreeablenessRF1)
plot(modelAgreeablenessRF1)

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelAgreeablenessRF1, newdata=test_dfAgreeableness)


#check performance measures --> für numerisch
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeableness1

spearmanAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeableness1



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(1997)
modelAgreeablenessfinal <- modelAgreeablenessRF

# Print model
### hier den Model namen ändern
print(modelAgreeablenessfinal)

#output in terms of regression coefficients
summary(modelAgreeablenessfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelAgreeablenessfinal)
plot(varImp(modelAgreeablenessfinal), 20, main = "Agreeableness")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelAgreeablenessfinal, newdata=test_dfAgreeableness)


#check performance measures --> für numerisch
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessfinal

spearmanAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

###anpassen: name vom dataset

imp <- importance(modelAgreeablenessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

###Model umbenennen

PartialPlots <- modelAgreeablenessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Agreeableness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

