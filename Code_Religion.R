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
#Religion: Categorical (4 Gruppen: Christentum, Ich fühle mich keiner Religion zugehörig, Islam, Judentum)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


# load data 

load("data_for_analysis.RData")

#data <- full  #oder ändern zu data <- reduced_set

cols_names <- names(data)  
cols_names


#define data for analysis
data_Religion <- data[,c(12, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Religion$Religion)) #122 NAs
data_Religion <- data_Religion %>% subset(data_Religion$Religion != "NA")


#ist die Variable unbalanced?
table(data_Religion$Religion) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Religion$Religion)/sum(table(data_Religion$Religion))) #no information rate 61%

#IV als Faktor:
data_Religion$Religion <- as.factor(data_Religion$Religion)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Religion$Religion, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfReligion <- data_Religion[index,]
test_dfReligion <- data_Religion[-index,]



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
RFReligion <- train(Religion ~ ., 
                            data=train_dfReligion,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "Kappa",
                            num.tree = 500,
                            trControl = myControl1, 
                            na.action = na.omit,
                            importance = 'impurity')

# Print models to console

RFReligion
summary(RFReligion)
plot(RFReligion)
#mtry = 14, extratrees, min.node.size = 10


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFReligion, newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model1 auc: 
RFReligion %>%
  test_roc(data = test_dfReligion) %>%
  auc()



####-------tree 2: num.tree prüfen --------------------------------------------------

#getunte Werte setzen und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFReligion1 <- train(Religion ~ ., 
                             data=train_dfReligion, 
                             method="ranger", metric= "Kappa",
                             tuneGrid = myGrid,
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl1, 
                             importance = 'impurity')

# Print models
RFReligion1
summary(RFReligion1)
#mtry = xx, extratrees, min.node.size = xx


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFReligion1, newdata=test_dfReligion)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfReligion$Religion))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligion1 %>%
  test_roc(data = test_dfReligion) %>%
  auc()


#model1: 500 trees performs better


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFReligionFinal <- RFReligionXX

# Print models
RFReligionFinal 
summary(RFReligionFinal )

#evaluate variable importance 
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees.

varImp(RFReligionFinal )
plot(varImp(RFReligionFinal ), 20, main = "Religion")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFReligionFinal , newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligionFinal %>%
  test_roc(data = test_dfReligion) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFReligionFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

#Model umbenennen

PartialPlots <- RFReligionFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Christentum") %>%plotPartial (main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Christentum") %>%plotPartial(main = "Christentum")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ich_fühle_mich_keiner_Religion_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Islam") %>%plotPartial(main = "Islam")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Judentum") %>%plotPartial(main = "Judentum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Judentum") %>%plotPartial(main = "Judentum")

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
