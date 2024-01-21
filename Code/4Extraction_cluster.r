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
"    No Yes
  1 391  38
  2  91   0
  3 135  52
  4 124  39
  5 105 125"
"Global"
qplot(km4$cluster, data=fraud_data, fill=fraudulent, binwidth=0.5)
"1 : age"
qplot(age, km4$cluster, data=fraud_data, color=fraudulent)  + geom_jitter(width = 0.2, height = 0.2)
"2 : days_to_incident"
qplot(days_to_incident, km4$cluster, data=fraud_data, color=fraudulent) + geom_jitter(width = 0.2, height = 0.2)
"3 : claim_amount # Etant donné que les valeurs des abscisses vont de [2, 14991], on va les afficher par tranche de 1000"
qplot(claim_amount, km4$cluster, data=fraud_data, color=fraudulent) + geom_jitter(width = 0.2, height = 0.2)
"4 : total_policy_claims"
qplot(total_policy_claims, km4$cluster, data=fraud_data, color=fraudulent) + geom_jitter(width = 0.2, height = 0.2)


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
kmi <- kmeans(dmatrix, 6)
fraud_data$cluster <- kmi$cluster
fraud_data_EA <- fraud_data[1:1128,]
fraud_data_ET <- fraud_data[1129:1692,]
# sapply(fraud_data_EA, function(x) length(levels(x)))
# fraud_data_EA <- subset(fraud_data_EA, select=-c(claim_area, gender, incident_cause))
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
[1] "Aire de l'arbre rpart est = 0.941854907682246"
AUC("C5.0")
[1] "Aire de l'arbre C5.0 est = 0.965714141973135"
AUC("tree")
[1] "Aire de l'arbre tree est = 0.908732454595764"

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
  confusion_matrix_tree <- confusionMatrix(pred_tree, pred_reelle, positive = "Yes")
  return(confusion_matrix_tree)
}
MC("rpart")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  244  57
       Yes  43 220

               Accuracy : 0.8227          
                 95% CI : (0.7886, 0.8533)
    No Information Rate : 0.5089
    P-Value [Acc > NIR] : <2e-16

                  Kappa : 0.645

 Mcnemar's Test P-Value : 0.1936

            Sensitivity : 0.7942
            Specificity : 0.8502
         Pos Pred Value : 0.8365
         Neg Pred Value : 0.8106
             Prevalence : 0.4911
         Detection Rate : 0.3901
   Detection Prevalence : 0.4663
      Balanced Accuracy : 0.8222

       'Positive' Class : Yes"
MC("C5.0")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  257  28
       Yes  30 249

               Accuracy : 0.8972
                 95% CI : (0.8691, 0.921)
    No Information Rate : 0.5089
    P-Value [Acc > NIR] : <2e-16

                  Kappa : 0.7943

 Mcnemar's Test P-Value : 0.8955

            Sensitivity : 0.8989
            Specificity : 0.8955
         Pos Pred Value : 0.8925
         Neg Pred Value : 0.9018
             Prevalence : 0.4911
         Detection Rate : 0.4415
   Detection Prevalence : 0.4947
      Balanced Accuracy : 0.8972

       'Positive' Class : Yes"
MC("tree")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  242  55
       Yes  45 222

               Accuracy : 0.8227
                 95% CI : (0.7886, 0.8533)
    No Information Rate : 0.5089
    P-Value [Acc > NIR] : <2e-16

                  Kappa : 0.6451

 Mcnemar's Test P-Value : 0.3681

            Sensitivity : 0.8014
            Specificity : 0.8432
         Pos Pred Value : 0.8315
         Neg Pred Value : 0.8148
             Prevalence : 0.4911
         Detection Rate : 0.3936
   Detection Prevalence : 0.4734
      Balanced Accuracy : 0.8223

       'Positive' Class : Yes"









rm(list = ls())

#-----------------------#
# PREDICTION DE DONNEES #
#-----------------------#
fraud_pred <- read.csv("Donnees/Data_Projet_1_New.csv", 
                      header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
fraud_pred <- subset(fraud_pred, select=-c(customer_id, claim_id))
fraud_pred$total_policy_claims <- as.ordered(fraud_pred$total_policy_claims)
"On crée la colonne cluster"
dmatrix <- daisy(fraud_pred)
kmi <- kmeans(dmatrix, 6)
fraud_pred$cluster <- kmi$cluster
"On crée la colonne fraudulent à l'aide de l'arbre C5.0"
fraud_pred$fraudulent <- predict(tree2, fraud_pred, type="class")
"On crée la colonne proba_fraudulent à l'aide de l'arbre C5.0"
fraud_pred$proba_fraudulent <- predict(tree2, fraud_pred, type="prob")[,2]
"On rajoute la colonne customer_id"
fraud_pred$customer_id <- 1:nrow(fraud_pred)
"On crée le fichier de sortie, d'abord on va créer un data frame avec:
1.Le numéro d'identification du client.
2.La classe prédite pour ce client.
3.La probabilité associée à la prédiction de cette classe
Et ce au format csv, le fichier s'appellera Data_result"
Data_result <- data.frame(customer_id = fraud_pred$customer_id, fraudulent = fraud_pred$fraudulent, proba_fraudulent = fraud_pred$proba_fraudulent)
write.csv(Data_result, file = "Donnees/Data_result.csv", row.names = FALSE)


