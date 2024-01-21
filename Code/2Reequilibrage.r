#-------------------------#
#  Chargement de données  #
#-------------------------#
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data <- resample(fraud_data)
qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar", main="Fraudulent", xlab="Fraudulent", ylab="Nombre de cas")
"Sauvegarde le qplot dans Graphs\chap2"
"ggsave("Graphs/chap2/qplot_fraudulent.png")"
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
"2/3 de 1692 pour fraud_data_EA et 1/3 pour fraud_data_ET"
fraud_data_EA <- fraud_data[1:1128,]
fraud_data_ET <- fraud_data[1129:1692,]
"Arbres"
tree1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "gini"))
tree2 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "gini"))
tree3 <- tree(fraudulent~., fraud_data_EA)
"Plot arbres"
"Sauvegarde les plots dans Graphs\chap2"
png('Graphs/chap2/plot_tree1.png')
rpart.plot(tree1, type=4, extra=7, box.col=c("tomato", "darkturquoise")[tree1$frame$yval], main="Arbre rpart")
dev.off()
png('Graphs/chap2/plot_tree2.png')
print(plot(tree2, type="simple", main="Arbre C5.0"))
dev.off()
png('Graphs/chap2/plot_tree3.png')
print(plot(tree3, main="Arbre tree", col="blue"))
text(tree3, pretty = 0)  # Ajout de la fonction text pour afficher les informations de l'arbre
dev.off()
"ROC"
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
legend(0.5, 0.5, legend=c("rpart", "C5.0", "tree"), col=c("green", "red", "blue"), lty=1:3, cex=0.8)
title(main="Courbes ROC")
"sauvegarde"
png('Graphs/chap2/plot_ROC.png')
plot(ROC("rpart"), col = "green")
plot(ROC("C5.0"), col = "red", add = TRUE)
plot(ROC("tree"), col = "blue", add = TRUE)
legend(0.5, 0.5, legend=c("rpart", "C5.0", "tree"), col=c("green", "red", "blue"), lty=1:3, cex=0.8)
title(main="Courbes ROC")
dev.off()


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
AUC("rpart") #AUC : 0.733895444756495
AUC("C5.0") #AUC : 0.829943453018148
AUC("tree") #AUC : 0.665100814830674

"En effet, l'arbre C5.0 est le plus performant avec un AUC de 0.829943453018148.
Si on le compare à l'AUC obtenu dans la section 1 (qui était de 0.646150043257941),
on peut remarquer une amélioration, en pourcentage, de 28.4%."
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
       No  195  90
       Yes  76 203

               Accuracy : 0.7057
                 95% CI : (0.6662, 0.743)
    No Information Rate : 0.5195
    P-Value [Acc > NIR] : <2e-16

                  Kappa : 0.4116

 Mcnemar's Test P-Value : 0.313

            Sensitivity : 0.6928
            Specificity : 0.7196
         Pos Pred Value : 0.7276
         Neg Pred Value : 0.6842
             Prevalence : 0.5195
         Detection Rate : 0.3599
   Detection Prevalence : 0.4947
      Balanced Accuracy : 0.7062

       'Positive' Class : Yes"
MC("C5.0")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  183  38
       Yes  88 255

               Accuracy : 0.7766
                 95% CI : (0.7399, 0.8103)
    No Information Rate : 0.5195
    P-Value [Acc > NIR] : < 2.2e-16       

                  Kappa : 0.5494

 Mcnemar's Test P-Value : 1.27e-05        

            Sensitivity : 0.8703
            Specificity : 0.6753
         Pos Pred Value : 0.7434
         Neg Pred Value : 0.8281
             Prevalence : 0.5195
         Detection Rate : 0.4521
   Detection Prevalence : 0.6082
      Balanced Accuracy : 0.7728

       'Positive' Class : Yes"
MC("tree")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  228 180
       Yes  43 113

               Accuracy : 0.6046
                 95% CI : (0.5629, 0.6452)
    No Information Rate : 0.5195
    P-Value [Acc > NIR] : 2.902e-05

                  Kappa : 0.2228

 Mcnemar's Test P-Value : < 2.2e-16

            Sensitivity : 0.3857
            Specificity : 0.8413
         Pos Pred Value : 0.7244
         Neg Pred Value : 0.5588
             Prevalence : 0.5195
         Detection Rate : 0.2004
   Detection Prevalence : 0.2766
      Balanced Accuracy : 0.6135

       'Positive' Class : Yes"

rm(list=ls())
