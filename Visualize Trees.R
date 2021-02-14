#growing a single tree

install.packages("rpart")
library(rpart)
library(rpart.plot)



load("data_reduced.RData")

data <- reduced_set

#######################
#Alter: Age Ranges Categorical (hoch, mittel, niedrig)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_AgeRange <- data[,c(312, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_AgeRange$Age_Range))
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#ist die Variable unbalanced?
table(data_AgeRange$Age_Range) 
max(table(data_AgeRange$Age_Range)/sum(table(data_AgeRange$Age_Range)))

#IV als Faktor:
data_AgeRange$Age_Range <- as.factor(data_AgeRange$Age_Range)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AgeRange$Age_Range, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAgeRange <- data_AgeRange[index,]
test_dfAgeRange <- data_AgeRange[-index,]



#Alter

tree_agerange <- rpart(Age_Range ~ . ,
                       data = train_dfAgeRange,
                       method = "class")

rpart.plot(tree_agerange)

importances <- varImp(tree_agerange)
importances %>%
  arrange(desc(Overall))

predictions <- predict(tree_agerange, newdata = test_dfAgeRange, type = "class")
predictions

confusionMatrix(predictions, test_dfAgeRange$Age_Range)






#######################
#Alter: numerisch
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Gibt es NAs in der DV?
sum(is.na(data_Alter$Alter)) #keine NAs

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#---------------------------------------------------------------------------------
  
tree_Alter <- rpart(Alter ~ . ,
                       data = train_dfAlter,
                       method = "anova")

rpart.plot(tree_Alter)

importances <- varImp(tree_Alter)
importances %>%
  arrange(desc(Overall))

predictionsAlter <- predict(tree_Alter, newdata = test_dfAlter, type = "class")
predictionsAlter

confusionMatrix(predictionsAlter, test_dfAlter$Alter)
