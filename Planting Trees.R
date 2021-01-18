#####################################
##
## Now we are ready to build a random forest.
##
#####################################


load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names


#####
#Geschlecht

data_Geschlecht <- data[,c(25, 27:255)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$Geschlecht <- as.factor(data_Geschlecht$Geschlecht)


#Training und Test Dataset
set.seed(400)

## ID NAs: impute any missing values in the training set using proximities
#DATASET <- rfImpute(DATASET ~ ., data = data, iter=6)





## NOTE: iter = the number of iterations to run. Breiman says 4 to 6 iterations
## is usually good enough. With this dataset, when we set iter=6, OOB-error
## bounces around between 17% and 18%. When we set iter=20, 
# set.seed(42)
# data.imputed <- rfImpute(hd ~ ., data = data, iter=20)
## we get values a little better and a little worse, so doing more 
## iterations doesn't improve the situation.
##
## NOTE: If you really want to micromanage how rfImpute(),
## you can change the number of trees it makes (the default is 300) and the
## number of variables that it will consider at each step.


## Now we are ready to build a random forest.


## NOTE: If the thing we're trying to predict (in this case it is 
## whether or not someone has heart disease) is a continuous number 
## (i.e. "weight" or "height"), then by default, randomForest() will set 
## "mtry", the number of variables to consider at each step, 
## to the total number of variables divided by 3 (rounded down), or to 1 
## (if the division results in a value less than 1).
## If the thing we're trying to predict is a "factor" (i.e. either "yes/no"
## or "ranked"), then randomForest() will set mtry to 
## the square root of the number of variables (rounded down to the next
## integer value).


## In this example, "hd", the thing we are trying to predict, is a factor and
## there are 13 variables. So by default, randomForest() will set 
## mtry = sqrt(13) = 3.6 rounded down = 3
## Also, by default random forest generates 500 trees (NOTE: rfImpute() only
## generates 300 tress by default)
model <- randomForest(Geschlecht ~ ., data=data_Geschlecht, proximity=TRUE, mtry = sqrt(229))


## RandomForest returns all kinds of things
model # gives us an overview of the call, along with...
# 1) The OOB error rate for the forest with ntree trees. 
#    In this case ntree=500 by default
# 2) The confusion matrix for the forest with ntree trees.
#    The confusion matrix is laid out like this:
#          
#                Healthy                      Unhealthy
#          --------------------------------------------------------------
# Healthy  | Number of healthy people   | Number of healthy people      |
#          | correctly called "healthy" | incorectly called "unhealthy" |
#          | by the forest.             | by the forest                 |
#          --------------------------------------------------------------
# Unhealthy| Number of unhealthy people | Number of unhealthy peole     |
#          | incorrectly called         | correctly called "unhealthy"  |
#          | "healthy" by the forest    | by the forest                 |
#          --------------------------------------------------------------


## Now check to see if the random forest is actually big enough...
## Up to a point, the more trees in the forest, the better. You can tell when
## you've made enough when the OOB no longer improves.

#Anm. Miriam: anpassen: times = 1+Ausprägungen; Error = anpassen!!
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"], 
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
#save this plot
ggsave("oob_error_rate_500_trees_Geschlecht.pdf")

model_Geschlecht_500 <- model

## Blue line = The error rate specifically for calling "Unheathly" patients that
## are OOB.
##
## Green line = The overall OOB error rate.
##
## Red line = The error rate specifically for calling "Healthy" patients 
## that are OOB.


## NOTE: After building a random forest with 500 tress, the graph does not make 
## it clear that the OOB-error has settled on a value or, if we added more 
## trees, it would continue to decrease.
## So we do the whole thing again, but this time add more trees.

model <- randomForest(Geschlecht ~ ., data=data_Geschlecht, ntree = 1000, proximity=TRUE, mtry = sqrt(229))
model


oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"], 
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

ggsave("oob_error_rate_1000_trees_Geschlecht.pdf")

model_Geschlecht_1000 <- model

## After building a random forest with 1,000 trees, we get the same OOB-error
## 16.5% and we can see convergence in the graph. So we could have gotten
## away with only 500 trees, but we wouldn't have been sure that number
## was enough.




## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Geschlecht ~ ., data=data_Geschlecht, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
model <- randomForest(Geschlecht ~ ., 
                      data=data_Geschlecht,
                      ntree=1000, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
model

## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)


mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)


## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)


## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data_Geschlecht$Geschlecht)


ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

ggsave(file="random_forest_mds_plot_Geschlecht.pdf")


