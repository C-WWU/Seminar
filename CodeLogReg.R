#install and load packages 

install.packages("readr")
library(readr)
install.packages("stepPlr")
library(stepPlr)
library(randomForest)
library(mlbench)
library(caret)
library(readxl)
library(DMwR)
library(ROSE)
library(ranger)
library(e1071)
library(tidyverse)
library(MASS)
library(dplyr)

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#Gibt es NAs in der DV?
sum(is.na(data_Geschlecht$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Geschlecht <- data_Geschlecht %>% filter(weiblich_maennlich != "NA")


###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable
data_Geschlecht <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$weiblich_maennlich <- as.factor(data_Geschlecht$weiblich_maennlich)

#Training und Test Dataset
set.seed(1997)

#-----------------------------------------------------------------------------------------------------------------

### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN


# Partitioning of the data: Create index matrix of selected values

# Create index matrix 
index <- createDataPartition(data_Geschlecht$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfGeschlecht <- data_Geschlecht[index,]
test_dfGeschlecht <- data_Geschlecht[-index,]

#re-label values of outcome (0 = weiblich, 1 = männlich)

### hier das dataset, DV und Ausprägungen anpassen

#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 1] = "0"
#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 2] = "1"

# Convert DV into type factor (wenn noch nicht geschehen)

train_dfGeschlecht$Geschlecht <- as.factor(train_dfGeschlecht$Geschlecht)
test_dfGeschlecht$Geschlecht <- as.factor(train_dfGeschlecht$Geschlecht)

#----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "repeatcv",
  repeats=3,
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, #Wenn das benutzt wird, auch ClassProbs = True setzen!; Nimmt in kombin 
  classProbs = TRUE,
  allowParallel=TRUE,
  #sampling = "smote",
  search = "random",
)


# set random seed again

set.seed(1997)

# apply stepwise logistic regression to find most importand IV's 

model <- glm(weiblich_maennlich~., data = train_dfGeschlecht, family = binomial) %>% 
  stepAIC(trace = FALSE)


# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN

# set random seed again

set.seed(1997)

model1 <- train(weiblich_maennlich ~ . 
               data=train_dfGeschlecht,
               method = "glm", family= binomial, 
               trControl=myControl) 

set.seed(1998)

model2 <- train(weiblich_maennlich ~ . 
                data=train_dfGeschlecht,
                method = "glm", family= binomial, 
                trControl=myControl) 

set.seed(1999)

model3 <- train(weiblich_maennlich ~ . 
                data=train_dfGeschlecht,
                method = "glm", family= binomial, 
                trControl=myControl) 

print(model)

#kappa rules of thumb for interpretation: 
# .81-1.00 Almost perfect
# .61-.80 Substantial
# .41-60 Moderate
# .21-.40 Fair
# .00-.20 Slight
# < .00 Poor 

# Output in terms of regression coefficients

summary(model)

# Odds Ratio 

exp(coef(model))

#variable Importance (predictor variables)

varImp(model)

# test for multicollinearity 

install.packages("car")
library(car)

vif(model)

#----------------------------------------FIND BEST THRESHOLD WITH ROC----------------------------------------

#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. 
#The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, legacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes
### renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden

Geschlecht <- traindf_Geschlecht$Geschlecht
model_Geschlecht_ROC <- roc(weiblich_maennlich ~ model$fitted.values, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...

roc.info_Geschlecht <- roc(weiblich_maennlich ~ model$fitted.values, legacy.axes=TRUE)
str(roc.info_Geschlecht)


# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model, newdata=test_dfGeschlecht)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlecht$Geschlecht)



# ------------------------------------------------MODEL COMPARISON------------------------------------------------



### model vergleich zu anderen models wie bspw. Random Forest

# roc(Geschlecht, model$traindf_Geschlecht, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# plot.roc(Geschlecht, model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

# Legend creates a legend on the ROC plot giving your models names and different colors 

# legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)



#--------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_model <- model
saveRDS(final_model, "./final_model.rds")

#load the model

super_model <- readRDS("./final_model.rds")
print(super_model)

