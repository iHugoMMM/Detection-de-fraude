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

#---------------#
#  Courbes ROC  #
#---------------#
ROC <- function(type){
    if(type == "rpart"){
        prob_tree <- predict(tree1, fraud_data_ET, type="prob")
    }
    else if(type == "rpart1"){
        prob_tree <- predict(tree1.1, fraud_data_ET, type="prob")
    }
    else if(type == "C5.0"){
        prob_tree <- predict(tree2, fraud_data_ET, type="prob")
    }
    else if(type == "C5.01"){
        prob_tree <- predict(tree2.1, fraud_data_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, fraud_data_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], fraud_data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"tpr","fpr")
  return(roc_perf)
}
plot(ROC("rpart"), col = "green")
plot(ROC("rpart1"), col = "black", add = TRUE)
plot(ROC("C5.0"), col = "red", add = TRUE)
plot(ROC("C5.01"), col = "yellow", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart","rpart(info)", "C5.0","C5.0(info)", "tree"), 
col=c("green","black", "red","yellow","blue"), lty=1:3, cex=0.8)
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
    else if(type == "rpart1"){
        prob_tree <- predict(tree1.1, fraud_data_ET, type="prob")
    }
    else if(type == "C5.1"){
        prob_tree <- predict(tree2.1, fraud_data_ET, type="prob")
    }
    else if(type == "tree"){
        prob_tree <- predict(tree3, fraud_data_ET, type="vector")
    }
  roc_pred <- prediction(prob_tree[,2], fraud_data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"auc")
  return(paste("Aire de l'arbre", type, "est =", attr(roc_perf, "y.values")))
}
AUC("rpart") #AUC : 0.630783174720883
AUC("rpart1") #AUC : 0.608659827792197
AUC("C5.0") #AUC : 0.646150043257941
AUC("C5.1") # AUC : 0.646150043257941
AUC("tree") #AUC : 0.625406830634862

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
    else if(type == "rpart1"){
        prob_tree <- predict(tree1.1, fraud_data_ET, type="class")
    }
    else if(type == "C5.1"){
        prob_tree <- predict(tree2.1, fraud_data_ET, type="class")
    }
  pred_reelle <- as.factor(fraud_data_ET$fraudulent)
  pred_tree <- as.factor(prob_tree)
  # Avec la classe possitive = "Yes"
  confusion_matrix_tree <- confusionMatrix(pred_tree, pred_reelle, positive = "Yes")
  return(confusion_matrix_tree)
}
MC("rpart")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  263  63
       Yes  16  24

               Accuracy : 0.7842          
                 95% CI : (0.7384, 0.8252)
    No Information Rate : 0.7623
    P-Value [Acc > NIR] : 0.1789

                  Kappa : 0.2684

 Mcnemar's Test P-Value : 2.274e-07
                                          
            Sensitivity : 0.27586
            Specificity : 0.94265
         Pos Pred Value : 0.60000
         Neg Pred Value : 0.80675
             Prevalence : 0.23770
         Detection Rate : 0.06557
   Detection Prevalence : 0.10929
      Balanced Accuracy : 0.60926

       'Positive' Class : Yes"
MC("C5.0")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  262  62
       Yes  17  25

               Accuracy : 0.7842
                 95% CI : (0.7384, 0.8252)
    No Information Rate : 0.7623
    P-Value [Acc > NIR] : 0.1789

                  Kappa : 0.2754

 Mcnemar's Test P-Value : 7.407e-07

            Sensitivity : 0.28736
            Specificity : 0.93907
         Pos Pred Value : 0.59524
         Neg Pred Value : 0.80864
             Prevalence : 0.23770
         Detection Rate : 0.06831
   Detection Prevalence : 0.11475
      Balanced Accuracy : 0.61321

       'Positive' Class : Yes"
MC("tree")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  269  76
       Yes  10  11

               Accuracy : 0.765
                 95% CI : (0.7182, 0.8075)
    No Information Rate : 0.7623
    P-Value [Acc > NIR] : 0.4798
                                          
                  Kappa : 0.1226

 Mcnemar's Test P-Value : 2.398e-12

            Sensitivity : 0.12644
            Specificity : 0.96416
         Pos Pred Value : 0.52381
         Neg Pred Value : 0.77971
             Prevalence : 0.23770
         Detection Rate : 0.03005
   Detection Prevalence : 0.05738
      Balanced Accuracy : 0.54530         

       'Positive' Class : Yes"
MC("rpart1")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  264  65
       Yes  15  22

               Accuracy : 0.7814
                 95% CI : (0.7355, 0.8227)
    No Information Rate : 0.7623
    P-Value [Acc > NIR] : 0.2135
                                          
                  Kappa : 0.2482

 Mcnemar's Test P-Value : 4.293e-08

            Sensitivity : 0.25287
            Specificity : 0.94624
         Pos Pred Value : 0.59459
         Neg Pred Value : 0.80243
             Prevalence : 0.23770
         Detection Rate : 0.06011
   Detection Prevalence : 0.10109
      Balanced Accuracy : 0.59956

       'Positive' Class : Yes"
MC("C5.1")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  262  62
       Yes  17  25

               Accuracy : 0.7842
                 95% CI : (0.7384, 0.8252)
    No Information Rate : 0.7623
    P-Value [Acc > NIR] : 0.1789

                  Kappa : 0.2754

 Mcnemar's Test P-Value : 7.407e-07

            Sensitivity : 0.28736
            Specificity : 0.93907
         Pos Pred Value : 0.59524
         Neg Pred Value : 0.80864
             Prevalence : 0.23770
         Detection Rate : 0.06831
   Detection Prevalence : 0.11475
      Balanced Accuracy : 0.61321

       'Positive' Class : Yes"

# rm(list=ls())
