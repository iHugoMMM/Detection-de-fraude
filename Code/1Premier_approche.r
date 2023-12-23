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
library(ROSE)
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) 
qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar") 
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
fraud_data_EA <- fraud_data[1:734,]
fraud_data_ET <- fraud_data[735:1100,]
# fraud_data_EA <- subset(fraud_data_EA, select=-c(customer_id, claim_id))

#-------------------------#
#   Aprentissages arbres  #
#-------------------------#
tree1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "gini"))
# tree1.1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "information"), control = rpart.control(maxdepth = 9))
tree2 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "gini"))
# tree2.1 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "information"))
tree3 <- tree(fraudulent~., fraud_data_EA)
# tree4 <- ctree(fraudulent~., fraud_data_EA)
prp(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
# prp(tree1.1, extra=8, box.col=c("tomato", "darkturquoise")[tree1.1$frame$yval], main="Arbre rpart") # Claim_amount comme plus significatif
plot(tree2, type="simple", main="Arbre C5.0")
# plot(tree2.1, type="simple", main="Arbre C5.0")
plot(tree3, main="Arbre tree", col="blue", fill="blue")
text(tree3, pretty = 0)
# plot(tree4, main="Arbre ctree", col="blue", fill="blue")

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
# plot(roc_perf1.1, col = "green", add = TRUE)
plot(ROC("C5.0"), col = "red", add = TRUE)
# plot(roc_perf2.1, col = "red", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
#plot(roc_perf4, col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart", "rpart", "C5.0", "C5.0", "tree", "ctree"), col=c("green", "green", "red", "red", "blue", "blue"), lty=1:3, cex=0.8)
title(main="Courbes ROC")

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
MC("C5.0")
MC("tree")

rm(list=ls())
