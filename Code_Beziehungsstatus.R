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
#Beziehungsstatus: Categorical (5 Gruppen: Geschieden, In einer Beziehung, Single, Verheiratet, Verwitwet)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Beziehungsstatus <- data[,c(18, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Beziehungsstatus$Beziehungsstatus)) #122 NAs
data_Beziehungsstatus <- data_Beziehungsstatus %>% subset(data_Beziehungsstatus$Beziehungsstatus != "NA")


#ist die Variable unbalanced?
table(data_Beziehungsstatus$Beziehungsstatus) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Beziehungsstatus$Beziehungsstatus)/sum(table(data_Beziehungsstatus$Beziehungsstatus))) #no information rate 61%

#IV als Faktor:
data_Beziehungsstatus$Beziehungsstatus <- as.factor(data_Beziehungsstatus$Beziehungsstatus)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Beziehungsstatus$Beziehungsstatus, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBeziehungsstatus <- data_Beziehungsstatus[index,]
test_dfBeziehungsstatus <- data_Beziehungsstatus[-index,]



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
RFBeziehungsstatus <- train(Beziehungsstatus ~ ., 
                          data=train_dfBeziehungsstatus,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "Kappa",
                          num.tree = 500,
                          trControl = myControl1, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print models to console

RFBeziehungsstatus
summary(RFBeziehungsstatus)
plot(RFBeziehungsstatus)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBeziehungsstatus, newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model1 auc: 
RFBeziehungsstatus %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFBeziehungsstatus1 <- train(Beziehungsstatus ~ ., 
                           data=train_dfBeziehungsstatus, 
                           method="ranger", metric= "Kappa",
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl1, 
                           importance = 'impurity')

# Print models
RFBeziehungsstatus1
summary(RFBeziehungsstatus1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBeziehungsstatus1, newdata=test_dfBeziehungsstatus)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatus1 %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBeziehungsstatusFinal <- RFBeziehungsstatusXX

# Print models
RFBeziehungsstatusFinal 
summary(RFBeziehungsstatusFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFBeziehungsstatusFinal )
plot(varImp(RFBeziehungsstatusFinal ), 20, main = "Beziehungsstatus")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBeziehungsstatusFinal , newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatusFinal %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBeziehungsstatusFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFBeziehungsstatusFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Geschieden") %>%plotPartial (main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Geschieden") %>%plotPartial(main = "Geschieden")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "In.einer.Beziehung") %>%plotPartial(main = "In einer Beziehung")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Single") %>%plotPartial(main = "Single")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Verwitwet") %>%plotPartial(main = "Verwitwet")


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
