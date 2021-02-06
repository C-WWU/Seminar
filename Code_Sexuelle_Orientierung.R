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
install.packages("naniar")

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
library(naniar)

options(max.print = 100000)


#######################
#Sexuelle_Orientierung: Categorical (3 Gruppen: Bisexuell, Heterosexuell, Homosexuell)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Sexuelle_Orientierung <- data[,c(16, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Sexuelle_Orientierung$Sexuelle_Orientierung)) #122 NAs
#Sonstige auch als NA, um sie aus Analyse auszuschließen:
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% replace_with_na_all(condition = ~.x == "Sonstiges:")
#Datenset ohne NAs
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% subset(data_Sexuelle_Orientierung$Sexuelle_Orientierung != "NA")

#ist die Variable unbalanced?
table(data_Sexuelle_Orientierung$Sexuelle_Orientierung) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung)/sum(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung))) #no information rate 61%

#IV als Faktor:
data_Sexuelle_Orientierung$Sexuelle_Orientierung <- as.factor(data_Sexuelle_Orientierung$Sexuelle_Orientierung)

#Variablennamen anpassen für Analyse
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% mutate(Sexuelle_Orientierung = case_when(Sexuelle_Orientierung == "1" ~ 'Bisexuell',
                                                               Sexuelle_Orientierung == "2" ~ 'Heterosexuell',
                                                               Sexuelle_Orientierung == "3" ~ 'Homosexuell',
                                                               ))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Sexuelle_Orientierung$Sexuelle_Orientierung, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[index,]
test_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[-index,]



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
RFSexuelle_Orientierung <- train(Sexuelle_Orientierung ~ ., 
                    data=train_dfSexuelle_Orientierung,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFSexuelle_Orientierung
summary(RFSexuelle_Orientierung)
plot(RFSexuelle_Orientierung)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFSexuelle_Orientierung, newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model1 auc: 
RFSexuelle_Orientierung %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFSexuelle_Orientierung1 <- train(Sexuelle_Orientierung ~ ., 
                     data=train_dfSexuelle_Orientierung, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFSexuelle_Orientierung1
summary(RFSexuelle_Orientierung1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSexuelle_Orientierung1, newdata=test_dfSexuelle_Orientierung)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_Orientierung1 %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFSexuelle_OrientierungFinal <- RFSexuelle_Orientierung

# Print models
RFSexuelle_OrientierungFinal 
summary(RFSexuelle_OrientierungFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFSexuelle_OrientierungFinal )
plot(varImp(RFSexuelle_OrientierungFinal ), 20, main = "Sexuelle_Orientierung")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSexuelle_OrientierungFinal , newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_OrientierungFinal %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFSexuelle_OrientierungFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFSexuelle_OrientierungFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Bisexuell") %>%plotPartial (main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")


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
