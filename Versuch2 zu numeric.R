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


#------------------------Analysis Reduced Dataset------------------------------

# load data 

load("data_reduced.RData")

data <- reduced_set

cols_names <- names(data)  
cols_names



#define tuning grid
myGridnum = expand.grid(mtry = c(71:81),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))




#######################
#Green Values: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green1 <- data[,c(305, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_Green1$Green_Values)) #keine NAs

#ist die Variable unbalanced?
table(data_Green1$Green_Values)
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green1$Green_Values, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen1 <- data_Green1[index,]
test_dfGreen1 <- data_Green1[-index,]


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

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen1_1mtry <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGridnum,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 500,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_1mtry
summary(RFGreen1_1mtry)
plot(RFGreen1_1mtry)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_1mtry, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_1

spearmanGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_1

#save model to disk 

tree500_Green_mtry <- RFGreen1_1mtry
saveRDS(tree500_Green_mtry, "./tree500_Green_mtry.rds")



####-------tree 2: num.tree prüfen --------------------------------------------------

#1000 num.tree ausprobieren --> ist mehr besser?

set.seed(1997)

RFGreen1_2mtry <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGridnum,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 1000,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_2mtry
summary(RFGreen1_2mtry)
plot(RFGreen1_2mtry)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_2mtry, newdata=test_dfGreen1)


MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_2

spearmanGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_2

#num.trees 1000 performs slightly better

#save model to disk 

tree1000_Green_mtry <- RFGreen1_2mtry
saveRDS(tree1000_Green_mtry, "./tree1000_Green_mtry.rds")



####-------tree 3: Final --------------------------------------------------

#final getunte Werte einsetzen

set.seed(1997)

RFGreen1_fin_mtry <- RFGreen1_2mtry

# Print model
RFGreen1_fin_mtry
summary(RFGreen1_fin_mtry)

#evaluate variable importance 
varImp(RFGreen1_fin_mtry)
plot(varImp(RFGreen1_fin_mtry), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_fin_mtry, newdata=test_dfGreen1)

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

#checking direction of the 20 most important variables

imp <- importance(RFGreen1_fin_mtry$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen1_fin_mtry

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

mtryGreen1 <- RFGreen1_fin_mtry
saveRDS(mtryGreen1, "./mtryGreen1.rds")

#load the model

mtryGreen1 <- readRDS("./mtryGreen1.rds")
print(mtryGreen1)




