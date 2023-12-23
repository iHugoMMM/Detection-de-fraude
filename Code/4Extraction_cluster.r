#-------------------------#
#   EXTRACTION CLUSTERS   #
#-------------------------#
library(gridExtra)
library(cluster)
library(rpart)
library(rpart.plot)
library(C50)
library(tree)
library(ROCR)
library(caret)
library(ggplot2)

fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                      header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE) #StringsAsFactors pour les variables qualitatives
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
fraud_data$total_policy_claims <- factor(as.factor(fraud_data$total_policy_claims), ordered=TRUE)

#-----------------------#
#  Matrice de Distance  #
#-----------------------#
dmatrix <- daisy(fraud_data)

#---------------------------------#
# Clustering par partitionnement  #
#---------------------------------#
km4 <- kmeans(dmatrix, 5)
table(km4$cluster, fraud_data$fraudulent)
qplot(km4$cluster, data=fraud_data, fill=fraud_data$fraudulent)
"1 : age"
qplot(age, km4$cluster, data=fraud_data, color=fraud_data$fraudulent)  + geom_jitter(width = 0.2, height = 0.2)
"2 : days_to_incident"
qplot(days_to_incident, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)
"3 : claim_amount # Etant donné que les valeurs des abscisses vont de [2, 14991], on va les afficher par tranche de 1000"
qplot(claim_amount, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)
"4 : total_policy_claims"
qplot(total_policy_claims, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)

#-----------------------#
# CHARGEMENT DE DONNEES #
#-----------------------#
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                      header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE) #StringsAsFactors pour les variables qualitatives
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data <- resample(fraud_data)
fraud_data$total_policy_claims <- as.ordered(fraud_data$total_policy_claims)
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
dmatrix <- daisy(fraud_data)
summary(dmatrix)
kmi <- kmeans(dmatrix, 6)
fraud_data$cluster <- kmi$cluster
fraud_data_EA <- fraud_data[1:1128,]
fraud_data_ET <- fraud_data[1129:1692,]

sapply(fraud_data_EA, function(x) length(levels(x)))
fraud_data_EA <- subset(fraud_data_EA, select=-c(claim_area, gender, incident_cause))
tree1 <- rpart(fraudulent~., fraud_data_EA)
tree2 <- C5.0(fraudulent~., fraud_data_EA)
tree3 <- tree(fraudulent~., fraud_data_EA)
prp(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
plot(tree2, type="simple", main="Arbre C5.0")
plot(tree3, main="Arbre tree", col="blue", fill="blue")
text(tree3, pretty = 0)

#---------------#
#  Courbes ROC  #
#---------------#
ROC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, fraud_data_ET, type="prob")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, fraud_data_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, fraud_data_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], fraud_data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"tpr","fpr")
  return(roc_perf)
}
plot(ROC("rpart"), col = "green")
plot(ROC("C5.0"), col = "red", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart", "rpart", "C5.0", "C5.0", "tree", "ctree"), col=c("green", "green", "red", "red", "blue", "blue"), lty=1:3, cex=0.8)
title(main="Courbes ROC")

#-------#
#  AUC  #
#-------#
AUC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, fraud_data_ET, type="prob")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, fraud_data_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, fraud_data_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], fraud_data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"auc")
  return(paste("Aire de l'arbre", type, "est =", attr(roc_perf, "y.values")))
}
AUC("rpart")
AUC("C5.0")
AUC("tree")

#--------------#
#  EVALUATION  #
#--------------#
MC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, fraud_data_ET, type="class")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, fraud_data_ET, type="class")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, fraud_data_ET, type="class")
    }
  pred_reelle <- as.factor(fraud_data_ET$fraudulent)
  pred_tree <- as.factor(prob_tree)
  confusion_matrix_tree <- confusionMatrix(pred_tree, pred_reelle)
  return(confusion_matrix_tree)
}
MC("rpart")
MC("C5.0")
MC("tree")

rm(list = ls())
