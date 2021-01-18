#####################################
##
## Now we are ready to build a random forest.
##
#####################################

#install and load relevant packages
library(ggplot2)
install.packages("cowplot")
library(cowplot)
install.packages("randomForest")
library(randomForest)

#load data
load("data_for_analysis.RData")

cols_names <- names(data)  
cols_names


###Erklärung:
#eine Raute bedeutet, das hier als Kommentierung stehen lassen (dann sind wir einigermaßen gleich auch wenn alle verschieden Coden)
###3 Rauten bedeuten, dass hier etw. angepasst werden muss. Das danach gerne löschen
###auch gerne eigene Sachen kommentieren, was auch immer zu den Modellen auffällt!

#####
#Geschlecht 
###(Name der DV die wir untersuchen austauschen)

###hier: Zeilen anpassen, die wir auswählen, und Dateienname ändern zu jew. Variable
data_Geschlecht <- data[,c(25, 27:255)]

cols_Geschlecht <- names(data_Geschlecht)
data_Geschlecht$Geschlecht <- as.factor(data_Geschlecht$Geschlecht)


#Training und Test Dataset
set.seed(400)

###falls es NAs gibt (nur bei wenigen Daten) --> Zeile 41 Raute weg und Namen von Dataset anpassen
#DATASET <- rfImpute(DATASET ~ ., data = data, iter=6)

# Modell erstellen: 300 trees (default)

###mtry: wenn numerisch, dann default = sqrt(229); wenn continuous, dann default = 229/3
### generates 300 tress by default, können wir so lassen
###anpassen: IV, data = neues Dataset; mtry anpassen zu entweder sqrt(229) oder 229/3
model <- randomForest(Geschlecht ~ ., data=data_Geschlecht, proximity=TRUE, mtry = sqrt(229))

#Modell prüfen
model 


#grafische Darstellung des OOB samples:

###anpassen: Z. 58 times = 1+Ausprägungen; Z. 59 rep anpassen mit ausprägungen, z. 60ff Error = anpassen!!
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "1", "2", "3"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"1"],  
          model$err.rate[,"2"],
          model$err.rate[,"3"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#Plot und Modell speichern
###NAmen anpassen zu richtiger Variable!
ggsave("oob_error_rate_500_trees_Geschlecht.pdf")

###Model abspeichern: Variablenname anpassen
model_Geschlecht_500 <- model


#zweites Model mit 1000 Trees --> wird error weniger oder stagniert er? 
##Modelgleichung: DV anpassen, data = .. anpassen
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

## Kommentieren: wie hat sich der Error verändert? Größer, kleiner, oder gleich? Sprich, war es notwendig mit 1000 Trees zu arbeiten?

###ABWÄGEN: auch ausprobieren für andere Zahl von Trees notwendig, zb 500? (dann einfach Kopieren und ntree = 500 setzen) 




# Prüfen: was ist das ideale mtry? 
###Zeile 110: Modellgleichung anpassen wie davor; ntree wählen welches besser war (300 oder 1000 oder anderes)
oob.values <- vector(length=10)
for(i in 1:20) {
  temp.model <- randomForest(Geschlecht ~ ., data=data_Geschlecht, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# find  minimum error
min(oob.values)
# find  optimal value for mtry
which(oob.values == min(oob.values))

# create a model for proximities using the best value for mtry
###anpassen: DV, Data, ntree
model <- randomForest(Geschlecht ~ ., 
                      data=data_Geschlecht,
                      ntree=1000, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
model


#Creating an MDS-plot
#First: Converting  proximity matrix into distance matrix
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)


#calculate the percentage of variation that each MDS axis accounts for:
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)


#Create actual MDS plot:
###anpassen: Status --> datenset$DV
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

###anpassen: Name (DV)
ggsave(file="random_forest_mds_plot_Geschlecht.pdf")


