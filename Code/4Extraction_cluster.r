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
# Pourqplot 
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
km4 <- kmeans(dmatrix, 7)
table(km4$cluster, fraud_data$fraudulent)
qplot(km4$cluster, data=fraud_data, fill=fraud_data$fraudulent)
# Nuages des points avec en abscisses:
# 1 : age
qplot(age, km4$cluster, data=fraud_data, color=fraud_data$fraudulent)  + geom_jitter(width = 0.2, height = 0.2)
# 2 : days_to_incident
qplot(days_to_incident, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)
# 3 : claim_amount # Etant donné que les valeurs des abscisses vont de [2, 14991], on va les afficher par tranche de 1000
qplot(claim_amount, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)
# 4 : total_policy_claims
qplot(total_policy_claims, km4$cluster, data=fraud_data, color=fraud_data$fraudulent) + geom_jitter(width = 0.2, height = 0.2)

# Prediction avec les clusters sans réequilibrage
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                      header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE) #StringsAsFactors pour les variables qualitatives
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
# Les variables ordinales devront être ordonnées, ici les variables ordinales sont : age, days_to_incident, claim_amount, total_policy_claims
fraud_data$total_policy_claims <- as.ordered(fraud_data$total_policy_claims)
dmatrix <- daisy(fraud_data)
summary(dmatrix)
kmi <- kmeans(dmatrix, 7)
fraud_data$cluster <- kmi$cluster
donnees_EA <- fraud_data[1:734,]
donnees_ET <- fraud_data[735:1100,]
sapply(donnees_EA, function(x) length(levels(x)))
# Enlèves les variables nos signif
donnees_EA <- subset(donnees_EA, select=-c(incident_cause, gender, claim_area, claim_type))
# Arbres
tree1 <- rpart(fraudulent~., donnees_EA)
tree2 <- C5.0(fraudulent~., donnees_EA)
tree3 <- tree(fraudulent~., donnees_EA)
prp(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
plot(tree2, type="simple", main="Arbre C5.0")
plot(tree3, main="Arbre tree", col="blue", fill="blue")
text(tree3, pretty = 0)
#---------------#
#  Courbes ROC  #
#---------------#
ROC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, donnees_ET, type="prob")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, donnees_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, donnees_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], donnees_ET$fraudulent)
  roc_perf <- performance(roc_pred,"tpr","fpr")
  return(roc_perf)
}
plot(ROC("rpart"), col = "green")
# plot(roc_perf1.1, col = "green", add = TRUE)
plot(ROC("C5.0"), col = "red", add = TRUE)
# plot(roc_perf2.1, col = "red", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
#plot(roc_perf4, col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart", "rpart", "C5.0", "C5.0", "tree", "ctree"), col=c("green", "green", "red", "red", "blue", "blue"), lty=1:3, cex=0.8)
title(main="Courbes ROC")
#-------#
#  AUC  #
#-------#
AUC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, donnees_ET, type="prob")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, donnees_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, donnees_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], donnees_ET$fraudulent)
  roc_perf <- performance(roc_pred,"auc")
  return(paste("Aire de l'arbre", type, "est =", attr(roc_perf, "y.values")))
}
AUC("rpart")
AUC("C5.0")
AUC("tree")
#Mesures d'évaluation
pred_reelle <- as.factor(donnees_ET$fraudulent)
pred_tree1 <- predict(tree1, donnees_ET, type="class")
pred_tree2 <- predict(tree2, donnees_ET, type="class")
pred_tree3 <- predict(tree3, donnees_ET, type="class")
confusionMatrix(pred_tree1, donnees_ET$fraudulent)
confusionMatrix(pred_tree2, donnees_ET$fraudulent)
confusionMatrix(pred_tree3, donnees_ET$fraudulent)

rm(list = ls())
