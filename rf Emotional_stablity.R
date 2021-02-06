
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
data_Emotional_stablity<- data[,c(298, 27:255)]

### es ist besonders wichtig die gewünschte DV in einen Faktor zu transformieren, da "caret" nicht mit 0/1 ausprägungen umgehen kann, wenn das model trainiert werden soll. 

cols_Emotional_stablity <- names(data_Emotional_stablity)
data_Emotional_stablity$Emotional_stablity <- as.numeric(data_Emotional_stablity$Emotional_stablity)

#Gibt es NAs in der DV?
sum(is.na(data_Emotional_stablity$Emotional_stablity)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Emotional_stablity <- data_Emotional_stablity%>% filter(Emotional_stablity != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------



### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

### hier einmal das vorhin definierte dataframe auswählen und nach dem $ die gewünschte DV eintragen. 
### p=0.8 heißt das data set wird nach der 80/20 regel in training und test data set geteilt. 
### Könnte  man auch anpassen in 70/30 oder 75/25 wie Kübler das in seinem Buch geschrieben hat. 

index <- createDataPartition(data_Emotional_stablity$Emotional_stablity, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfEmotional_stablity <- data_Emotional_stablity[index,]
test_dfEmotional_stablity <- data_Emotional_stablity[-index,]


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
modelEmotional_stablityRF <- train(Emotional_stablity ~ ., 
                             data=train_dfEmotional_stablity,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "RMSE", # numeric: RMSE; categorical: Kappa; binary: ROC
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelEmotional_stablityRF
summary(modelEmotional_stablityRF)
plot(modelEmotional_stablityRF)


#best mtry = 10, splitrule = extratrees, min.node.size = 10

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelEmotional_stablityRF, newdata=test_dfEmotional_stablity)

#check performance measures --> für numerisch
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityRF

spearmanEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityRF


#save the best mtry 

bestmtry <- modelEmotional_stablityRF$bestTune$mtry

####-------tree 2: num.tree prüfen --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
###mtry, splitrule und min.node.size zu dem anpassen, was tree 1 gefunden hat!
set.seed(1997)
modelEmotional_stablityRF1 <- train(Emotional_stablity ~ ., 
                              data=train_dfEmotional_stablity,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelEmotional_stablityRF1
summary(modelEmotional_stablityRF1)
plot(modelEmotional_stablityRF1)

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelEmotional_stablityRF1, newdata=test_dfEmotional_stablity)


#check performance measures --> für numerisch
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablity1

spearmanEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablity1



#fit model with num.trees = xx trees (better performance)

####-------tree 3: Final --------------------------------------------------

### hier das finale model mit bestmtry und node size einfügen , auch best num.tree anpassen

set.seed(1997)
modelEmotional_stablityfinal <- modelEmotional_stablityRF

# Print model
### hier den Model namen ändern
print(modelEmotional_stablityfinal)

#output in terms of regression coefficients
summary(modelEmotional_stablityfinal)

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.
### hier auch den model namen ändern

varImp(modelEmotional_stablityfinal)
plot(varImp(modelEmotional_stablityfinal), 20, main = "Emotional_stablity")

# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen
predictions <- predict(modelEmotional_stablityfinal, newdata=test_dfEmotional_stablity)


#check performance measures --> für numerisch
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

###numeric only:
#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityfinal

spearmanEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityfinal

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 10 most important variables

###anpassen: name vom dataset

imp <- importance(modelEmotional_stablityfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

###Model umbenennen

PartialPlots <- modelEmotional_stablityfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Emotional_stablity")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

