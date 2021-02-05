
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
data_Conscientiousness<- data[,c(297, 27:255)]

### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Conscientiousness <- names(data_Conscientiousness)
data_Conscientiousness$Conscientiousness <- as.numeric(data_Conscientiousness$Conscientiousness)

#Gibt es NAs in der DV?
sum(is.na(data_Conscientiousness$Conscientiousness)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Conscientiousness <- data_Conscientiousness%>% filter(Conscientiousness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Conscientiousness$Conscientiousness, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfConscientiousness <- data_Conscientiousness[index,]
test_dfConscientiousness <- data_Conscientiousness[-index,]


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
modelConscientiousnessRF <- train(Conscientiousness ~ ., 
                             data=train_dfConscientiousness,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "RMSE", # numeric: RMSE; categorical: Kappa; binary: ROC
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelConscientiousnessRF
summary(modelConscientiousnessRF)
plot(modelConscientiousnessRF)


#best mtry = 10, splitrule = extratrees, min.node.size = 15

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelConscientiousnessRF, newdata=test_dfConscientiousness)

#check performance measures --> für numerisch
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessRF

spearmanConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessRF


#save the best mtry 

bestmtry <- modelConscientiousnessRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!
set.seed(1997)
modelConscientiousnessRF1 <- train(Conscientiousness ~ ., 
                              data=train_dfConscientiousness,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelConscientiousnessRF1
summary(modelConscientiousnessRF1)
plot(modelConscientiousnessRF1)

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelConscientiousnessRF1, newdata=test_dfConscientiousness)


#check performance measures --> für numerisch
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousness1

spearmanConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousness1



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(1997)
modelConscientiousnessfinal <- modelConscientiousnessRF

# Print model
### hier den Model namen ändern
print(modelConscientiousnessfinal)

#output in terms of regression coefficients
summary(modelConscientiousnessfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelConscientiousnessfinal)
plot(varImp(modelConscientiousnessfinal), 20, main = "Conscientiousness")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelConscientiousnessfinal, newdata=test_dfConscientiousness)


#check performance measures --> für numerisch
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessfinal

spearmanConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

###anpassen: name vom dataset

imp <- importance(modelConscientiousnessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

###Model umbenennen

PartialPlots <- modelConscientiousnessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Conscientiousness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

