#-------------------------#
#  Chargement de données  #
#-------------------------#
# On va tester httpgd avec un histogramme de la loi normale
library(httpgd)
hgd() # Pour ouvrir une fenêtre httpgd locale
hgd_browse() # Pour initialiser 
hist(rnorm(1000)) # Plot test
hist(rnorm(50)) # Plot test
dev.off() # Pour arrêter le plot dans la fenêtre locale
# Pour arrêter d'utiliser httpgd, il faut utiliser la commande suivante
httpgd.stop()
library(ggplot2)
library(rpart)
library(C50)
library(tree) # Pour tree
library(party) # Pour ctree
library(rpart.plot) # Pour prp
library(ROCR)
library(caret) # Pour la matrice de confusion
fraud_data <- read.csv("S1/R/Traitement de donnees/Projet/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) 
qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar") 
fraud_data_EA <- fraud_data[1:734,]
fraud_data_ET <- fraud_data[735:1100,]
fraud_data_EA <- subset(fraud_data_EA, select=-c(customer_id, claim_id))

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
# On test avec rpart 
AUC("rpart")
AUC("C5.0")
AUC("tree")
# [1] "Aire de l'arbre rpart est = 0.630783174720883"
# > AUC("C5.0")
# [1] "Aire de l'arbre C5.0 est = 0.646150043257941"
# > AUC("tree")
# [1] "Aire de l'arbre tree est = 0.625406830634862"
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
# On test avec rpart
MC("rpart")
MC("C5.0")
MC("tree")
