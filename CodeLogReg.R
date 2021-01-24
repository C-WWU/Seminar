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
library(pROC)

# load data 

load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich)) #keine NAs
###folgende Kommentierung und Code nur drin lassen und anpassen, wenn es NAs gibt --> bitte prüfen, dass der Code auch das richtige macht :)
#Respondents mit NAs für diese Variable löschen (NAs stehen nur, wenn Respondent "Keine Angabe" gemacht hat, daher bedeutet löschen keinen Informationsverlust)
data_Geschlecht <- data_GeschlechtMW %>% filter(weiblich_maennlich != "NA")


###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable
data_GeschlechtMW <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Training und Test Dataset
set.seed(1997)

#-----------------------------------------------------------------------------------------------------------------

### ACHTUNG DAS DATA SET NUR SPLITTEN WENN NOCH NICHT VORHER FÜR DIE DV GEMACHT. ANSONSTEN STEP ÜBERSPRINGEN


# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_GeschlechtMW$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

### name anpassen an DV

train_dfGeschlechtMW <- data_GeschlechtMW[index,]
test_dfGeschlechtMW <- data_GeschlechtMW[-index,]


#re-label values of outcome (0 = weiblich, 1 = männlich)

### hier das dataset, DV und Ausprägungen anpassen

#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 1] = "0"
#train_dfGeschlecht$Geschlecht[train_dfGeschlecht$Geschlecht == 2] = "1"

# Convert DV into type factor (wenn noch nicht geschehen)

train_dfGeschlechtMW$weiblich_maennlich <- as.factor(train_dfGeschlechtMW$weiblich_maennlich)
test_dfGeschlechtMW$weiblich_maennlich <- as.factor(test_dfGeschlechtMW$weiblich_maennlich)

#----------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> bei uns 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
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

model <- glm(weiblich_maennlich~., data = train_dfGeschlechtMW, family = binomial) %>% 
  stepAIC(trace = FALSE)


# Specify logistic regression model with most important IV's (maybe also these indicated by random forest and our own suggestions)

### HIER DIE "~ ." WEG UND DIE WICHTIGSTEN VARIABLEN MIT + EINFÜGEN. DIESEN SCHRITT MEHRMALS WIEDERHOLEN UM DAS BESTE MODEL ZU FINDEN

# set random seed again

set.seed(1997)

model1 <- train(weiblich_maennlich ~ RB_Leipzig + FC_Bayern_Muenchen + adidas_Deutschland + Westwing + dm, 
               data=train_dfGeschlechtMW,
               method = "glm", family= binomial, 
               na.action = na.omit,
               trControl=myControl)

set.seed(1998)

model2 = train(weiblich_maennlich ~ ., 
                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, 
                na.action = na.omit,
                trControl=myControl) 

set.seed(1999)

model3 <- train(weiblich_maennlich ~ . 
                data=train_dfGeschlechtMW,
                method = "glm", family= binomial, 
                na.action = na.omit,
                trControl=myControl) 

print(model2)


#kappa rules of thumb for interpretation: 
# .81-1.00 Almost perfect
# .61-.80 Substantial
# .41-60 Moderate
# .21-.40 Fair
# .00-.20 Slight
# < .00 Poor 

# Output in terms of regression coefficients

summary(model2)

# Odds Ratio 

exp(coef(model1)) # -> funktioniert glaube ich auch leider nicht mit Cross Validation :/ dafür kann man sich aber die variable importance mit der funktion unten ausgeben lassen. 

#variable Importance (predictor variables)

varImp(model2)


#----------------------------------------FIND BEST THRESHOLD WITH ROC !MAYBE NOT POSSIBLE WITH CROSS VALIDATION?----------------------------------------

#Drawing ROC and AUC using pROC and the final model--> the ROC Graph summarizes all of the confusion matrices that each threshold produced. 
#The AUC makes it easy to compare 1 ROC curve to another. This is a measure to compare performance of different models having a BINARY DV! 

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region

### Hier wieder DV und dataset austauschen, legacy.axes = TRUE heißt er beschreibt 1- Specificity auf der x-axes
### renamed the x and y axes added color and more width with col and lwd
### DV musste hier komischwerweise nochmal definiert werden, sonst wird sie in dem code darunter nicht gefunden

weiblich_maennlich <- train_dfGeschlechtMW$weiblich_maennlich
model_Geschlecht_ROC <- roc(weiblich_maennlich, model1$fitted.values, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

# If we want to find out the optimal threshold we can store the 
# data used to make the ROC graph in a variable...

roc.info_Geschlecht <- roc(weiblich_maennlich, model1$fitted.values, legacy.axes=TRUE)

str(roc.info_Geschlecht)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info_Geschlecht$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info_Geschlecht$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.infoGeschlecht$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%


# ----------------------------------------------MODEL EVALUATION-------------------------------------------------


# Apply model to test_df --> test_dfGeschlecht

# predict outcome using model from train_df applied to the test_df

### hier auch einmal nach dem testdf der DV umbenennen

predictions <- predict(model, newdata=test_dfGeschlecht)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfGeschlecht$Geschlecht)



# ------------------------------------------------MODEL COMPARISON------------------------------------------------



### model vergleich zu anderen models wie bspw. Random Forest

### erst die originale ROC curve vom model im oberen code plotten

# roc(Geschlecht, model$traindf_Geschlecht, plot=TRUE, ledacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)

#### und dann fügen wir die ROC Curve eines anderen Models hinzu

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

