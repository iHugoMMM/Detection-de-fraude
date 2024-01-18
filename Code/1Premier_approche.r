#-------------------------#
#  Chargement de données  #
#-------------------------#
library(ggplot2)
library(rpart)
library(C50)
library(tree) 
library(party) 
library(rpart.plot) 
library(ROCR)
library(caret) 
library(CORElearn)
library(entropy)
library(ROSE) # Pour le reequilibrage des classes 
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) 
#Type du variable fraud_data
str(fraud_data)
View(fraud_data)
names(fraud_data)
summary(fraud_data)
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))

qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar")
qplot(gender, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(age, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(claim_area, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(claim_type, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(claim_amount, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(incident_cause, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(days_to_incident, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(days_to_incident, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(police_report, data = fraud_data, fill = fraudulent, geom = "bar")
qplot(total_policy_claims, data = fraud_data, fill = fraudulent, geom = "bar")

fraud_data_EA <- fraud_data[1:734,]
fraud_data_ET <- fraud_data[735:1100,]

#-------------------------#
#   Aprentissages arbres  #
#-------------------------#
tree1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "gini"))
tree1.1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "information"), control = rpart.control(maxdepth = 9))
tree2 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "gini"))
tree2.1 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "information"))
tree3 <- tree(fraudulent~., fraud_data_EA)

rpart.plot(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
rpart.plot(tree1.1, extra=8, box.col=c("tomato", "darkturquoise")[tree1.1$frame$yval], main="Arbre rpart") # Claim_amount comme plus significatif
plot(tree2, type="simple", main="Arbre C5.0")
plot(tree2.1, type="simple", main="Arbre C5.0")
plot(tree3, main="Arbre tree", col="blue")
text(tree3, pretty = 0)

#Sauvegarde dans le dossier Graphs en tant que png
plot_tree1 <- rpart.plot(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
plot_tree1.1 <- rpart.plot(tree1.1, extra=8, box.col=c("tomato", "darkturquoise")[tree1.1$frame$yval], main="Arbre rpart") # Claim_amount comme plus significatif
png("Graphs/arbre_rpart.png")
rpart.plot(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
dev.off()
png("Graphs/Arbre_rpart1.png")
plot_tree1.1
dev.off()
png("Graphs/Arbre_C5.0.png")
print(plot(tree2, type="simple", main="Arbre C5.0"))
dev.off()
png("Graphs/arbre_C5.0_information.png")
print(plot(tree2.1, type="simple", main="Arbre C5.0 - Information comme critère"))
dev.off()
png("Graphs/arbre_tree.png")
print(plot(tree3, main="Arbre tree", col="blue"))
text(tree3, pretty = 0)  # Ajout de la fonction text pour afficher les informations de l'arbre
dev.off()

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
plot(roc_perf1.1, col = "green", add = TRUE)
plot(ROC("C5.0"), col = "red", add = TRUE)
plot(roc_perf2.1, col = "red", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
plot(roc_perf4, col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart", "C5.0", "tree"), col=c("green", "red","blue",), lty=1:3, cex=0.8)
title(main="Courbes ROC")

#Sauvegarde
# Ouvrir le fichier PNG pour sauvegarder l'image
png("Graphs/graphROC.png")

# Courbe ROC pour rpart
roc_perf_rpart <- ROC("rpart")
plot(roc_perf_rpart, col = "green", main="Courbes ROC", lty=1)
legend("bottomright", legend="rpart", col="green", lty=1, cex=0.8)

# Ajouter la courbe ROC pour C5.0
roc_perf_C5.0 <- ROC("C5.0")
plot(roc_perf_C5.0, col = "red", add = TRUE)
legend("bottomright", legend="C5.0", col="red", lty=1, cex=0.8)

# Ajouter la courbe ROC pour tree
roc_perf_tree <- ROC("tree")
plot(roc_perf_tree, col = "blue", add = TRUE)
legend("bottomright", legend="tree", col="blue", lty=1, cex=0.8)

# Fermer le fichier PNG
dev.off()


#---------#
#   AUC   #
#---------#
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

#---------------------------#
#  MESURES D'EVALUATION/MC  #
#---------------------------#
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
"Pour rpart, on a une matrice de confusion :
    No Yes
No 263 63
Yes 16 24
Ainsi on a :
Vrais Positifs = 24
Vrais Négatifs = 263
Faux Positifs = 63
Faux Négatifs = 16
Ainsi on a :
Rappel (sensibilite) = VP / (VP + FN) = 24 / (24 + 16) = 0.6) VP correctement predits
Specificite = VN / (VN + FP) = 263 / (263 + 63) = 0.81  VN correctement predits
Precision = VP / (VP + FP) = 24 / (24 + 63) = 0.28 
Accuracy = (VP + VN) / (VP + VN + FP + FN) = (24 + 263) / (24 + 263 + 63 + 16) = 0.79"
MC("C5.0")
MC("tree")

rm(list=ls())
