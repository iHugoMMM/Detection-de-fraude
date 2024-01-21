"Chap1"
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

"Chapitre 2"
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
fraud_data <- subset(fraud_data, select=-c(customer_id, claim_id))
fraud_data_EA <- fraud_data[1:1128,]
fraud_data_ET <- fraud_data[1129:1692,]
"Arbres"
tree1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "gini"))
tree2 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "gini"))
tree3 <- tree(fraudulent~., fraud_data_EA)
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

# rm(list=ls())

"Chapitre 3"
#----------------------------#
#  TESTS DE SIGNIFICATIVITE  #
#----------------------------#
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) #StringsAsFactors pour les variables qualitatives
fraud_data <- subset(fraud_data, select=-c(customer_id,claim_id))
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data <- resample(fraud_data)
fraud_data$total_policy_claims <- as.ordered(fraud_data$total_policy_claims)
#-------------------------------#
#  TEST CHI2 VAR CATEGORIELLES  #
#-------------------------------#
chi2 <- function(test){
  return (chisq.test(test, fraud_data$fraudulent)[3])
}
fisher <- function(test){
  return (fisher.test(test, fraud_data$fraudulent)[1])
}
pvchi2_table <- function() {
  variable_names <- c("gender", "incident_cause", "claim_area", "police_report", "claim_type")
  p_values <- sapply(variable_names, function(var) chi2(fraud_data[[var]]))

  # Convertir la liste en vecteur
  p_values <- unlist(p_values)

  # Créer un vecteur
  pvalue_vector <- numeric(length(variable_names))
  
  # Remplir le vecteur avec les valeurs p
  for (i in seq_along(variable_names)) {
    pvalue_vector[i] <- p_values[i]
  }

  # Ajouter les noms de variables au vecteur
  names(pvalue_vector) <- variable_names
  
  # Ordonner le vecteur par valeurs décroissantes
  pvalue_vector <- pvalue_vector[order(pvalue_vector, decreasing = FALSE)]
  
  return(pvalue_vector)
}
pvfish_table <- function() {
  variable_names <- c("gender", "incident_cause", "claim_area", "police_report", "claim_type")
  p_values <- sapply(variable_names, function(var) fisher.test(table(fraud_data[[var]], fraud_data$fraudulent))[1])

  # Convertir la liste en vecteur
  p_values <- unlist(p_values)

  # Créer un vecteur
  pvalue_vector <- numeric(length(variable_names))
  
  # Remplir le vecteur avec les valeurs p
  for (i in seq_along(variable_names)) {
    pvalue_vector[i] <- p_values[i]
  }

  # Ajouter les noms de variables au vecteur
  names(pvalue_vector) <- variable_names
  
  # Ordonner le vecteur par valeurs décroissantes
  pvalue_vector <- pvalue_vector[order(pvalue_vector, decreasing = FALSE)]
  
  return(pvalue_vector)
}
pvchi2_table()
pvfish_table()
"pvchi2_table()
 police_report     claim_type incident_cause         gender     claim_area 
  2.474865e-10   1.775015e-08   9.940190e-02   1.585209e-01   2.931374e-01
> pvfish_table()
 police_report     claim_type incident_cause         gender     claim_area 
  1.441902e-10   1.358592e-08   9.969912e-02   1.585007e-01   2.931189e-01"
  
#--------------------------#
#  FISCHER VARIABLES NUME  #
#--------------------------#
fraud_data_num <- ifelse(fraud_data$fraudulent == "Yes", 1, 0)
fraud_data_copie <- fraud_data
fraud_data_copie$fraudulent <- fraud_data_num
pearson <- function(variable){
  return (cor.test(fraud_data_copie[[variable]], fraud_data_copie$fraudulent, method = "pearson")[3])
}
spearman <- function(variable){
  return (cor.test(fraud_data_copie[[variable]], fraud_data_copie$fraudulent, method = "spearman")[3])
}
kendall <- function(variable){
  return (cor.test(fraud_data_copie[[variable]], fraud_data_copie$fraudulent, method = "kendall")[3])
}
psk <- function(variable){
  "return sous la forme : p-value de pearson de la variable, variable, est : ,pearson(variable), même chose pour pearman et kendall"
  print(paste("Variable testée : ", variable))
  print(paste("p-value Pearson = ", pearson(variable)))
  print(paste("p-value Spearman = ", spearman(variable))) 
  print(paste("p-value Kendall = ", kendall(variable))) 
}
psk("age")
'[1] "p-value Pearson =  4.61337284270551e-07"
[1] "p-value Spearman =  2.48938561558996e-07"
[1] "p-value Kendall =  2.73961583160717e-07"'
psk("days_to_incident")
'"[1] "p-value Pearson =  1.63154239370855e-06"
[1] "p-value Spearman =  4.29831904665197e-11"
[1] "p-value Kendall =  5.59674069164966e-11"'
psk("claim_amount")
'[1] "p-value Pearson =  0.455960214966064"
[1] "p-value Spearman =  0.0703230077916243"
[1] "p-value Kendall =  0.0703407348935491"'
psk("total_policy_claims")
'[1] "p-value Pearson =  0.00980647494827477"
[1] "p-value Spearman =  0.0372912761703214"
[1] "p-value Kendall =  0.0373492251937505"'
"Avec une p-value < 0.05, on rejette l'hypothèse nulle, il y a donc une corrélation entre la variable et la variable cible"

#-------------------------#
#  GRAPHIQUE EN MOZAIQUE  #
#-------------------------#
"Mosaic plot de représentation chi2-residuals de toutes les variables par rapport à la classe étudiée fraudulent"
"Avec gender"
mosaicplot(table(fraud_data$gender, fraud_data$fraudulent), shade = TRUE, main = "Gender", xlab = "Gender", ylab = "fraudulent", color = TRUE)
"Incident cause"
mosaicplot(table(fraud_data$incident_cause, fraud_data$fraudulent), shade = TRUE, main = "Incident Cause", xlab = "Incident Cause", ylab = "fraudulent", color = TRUE)
"Claim_area"
mosaicplot(table(fraud_data$claim_area, fraud_data$fraudulent), shade = TRUE, main = "Claim Area", xlab = "Claim Area", ylab = "fraudulent", color = TRUE)
"Police_report"
mosaicplot(table(fraud_data$police_report, fraud_data$fraudulent), shade = TRUE, main = "Police report", xlab = "Police report", ylab = "fraudulent", color = TRUE)
"Claim_type"
mosaicplot(table(fraud_data$claim_type, fraud_data$fraudulent), shade = TRUE, main = "Claim Type", xlab = "Claim Type", ylab = "fraudulent", color = TRUE)

"Discretisation supervisée à faire"
"Age"
fraud_data$age <- cut(fraud_data$age, breaks = 5)
mosaicplot(table(fraud_data$age, fraud_data$fraudulent), shade = TRUE, main = "Age", 
                 xlab = "age", ylab = "fraudulent", color = TRUE)
"Days_to_incident"
fraud_data$days_to_incident <- cut(fraud_data$days_to_incident, breaks = 8)
mosaicplot(table(fraud_data$days_to_incident, fraud_data$fraudulent), shade = TRUE, main = "Days to Incident", 
                 xlab = "days_to_incident", ylab = "fraudulent", color = TRUE)
"Claim_amount"
fraud_data$claim_amount <- cut(fraud_data$claim_amount, breaks = 10)
mosaicplot(table(fraud_data$claim_amount, fraud_data$fraudulent), shade = TRUE, main = "Claim Amount", 
                 xlab = "claim_amount", ylab = "fraudulent", color = TRUE)
"Total_policy_claims"
fraud_data$total_policy_claims <- cut(fraud_data$total_policy_claims, breaks = 5)
mosaicplot(table(fraud_data$total_policy_claims, fraud_data$fraudulent), shade = TRUE, main = "Total Policy Claims", 
                 xlab = "total_policy_claims", ylab = "fraudulent", color = TRUE)

Eval <- function(estimator){
  attr <- attrEval(fraudulent~., fraud_data, estimator = estimator)
  attr <- sort(attr, decreasing = TRUE)
  return(attr)
}
Eval("GainRatio")
"total_policy_claims       police_report    days_to_incident                 age 
        0.104655533         0.019078534         0.009190180         0.008606188 
         claim_type          claim_area      incident_cause              gender
        0.007791333         0.003102022         0.001044547         0.000363893 
       claim_amount
       -1.000000000"
Eval("Gini")
"claim_amount       police_report                 age    days_to_incident 
       0.0303624371        0.0171140163        0.0136374392        0.0120551733
         claim_type total_policy_claims      incident_cause          claim_area
       0.0070925484        0.0055454957        0.0016318665        0.0009763576
             gender 
       0.0002522038"
Eval("Accuracy")
"claim_amount                 age       police_report    days_to_incident 
         0.07210402          0.06560284          0.05910165          0.05614657
         claim_type total_policy_claims      incident_cause          claim_area 
         0.04314421          0.03546099          0.02127660          0.01300236 
             gender
         0.01122931"
Eval("Relief")
"      age    days_to_incident      incident_cause       police_report 
         0.12529551          0.10283688          0.10224586          0.09929078
total_policy_claims        claim_amount          claim_type              gender 
         0.09574468          0.07978723          0.03250591          0.02836879
         claim_area
         0.01536643"
Eval("MDL")
"       claim_amount       police_report                 age    days_to_incident 
       0.0382797154        0.0211697834        0.0113500490        0.0067407198
total_policy_claims          claim_type          claim_area              gender 
       0.0060765507        0.0059883394       -0.0005329839       -0.0020233076
     incident_cause
      -0.0060326374"


#-------------------------#
#  Chargement de données  #
#-------------------------#
"Réequilibrage et suppression des variables non significatives"
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) 
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data_sample <- resample(fraud_data)
"2/3 de 1692 pour fraud_data_EA et 1/3 pour fraud_data_ET"
fraud_data_EA <- fraud_data_sample[1:1128,]
fraud_data_ET <- fraud_data_sample[1129:1692,]
"On enlève les variables non significatives"
fraud_data_EA <- subset(fraud_data_EA, select=-c(customer_id, claim_id, claim_area, gender, incident_cause))
"Arbres"
tree1 <- rpart(fraudulent~., fraud_data_EA, parms = list(split = "gini"))
tree2 <- C5.0(fraudulent~., fraud_data_EA, param = list(split = "gini"))
tree3 <- tree(fraudulent~., fraud_data_EA)

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
AUC("rpart") #AUC : 0.746698943661972
AUC("C5.0") #AUC : 0.818228118712274
AUC("tree") #AUC : 0.679564889336016
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
       No  209 104
       Yes  71 180

               Accuracy : 0.6897
                 95% CI : (0.6497, 0.7277)
    No Information Rate : 0.5035
    P-Value [Acc > NIR] : < 2e-16

                  Kappa : 0.3799

 Mcnemar's Test P-Value : 0.01556

            Sensitivity : 0.6338
            Specificity : 0.7464
         Pos Pred Value : 0.7171
         Neg Pred Value : 0.6677
             Prevalence : 0.5035
         Detection Rate : 0.3191
   Detection Prevalence : 0.4450
      Balanced Accuracy : 0.6901

       'Positive' Class : Yes"
MC("C5.0")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  223  95
       Yes  57 189

               Accuracy : 0.7305
                 95% CI : (0.6918, 0.7667)
    No Information Rate : 0.5035
    P-Value [Acc > NIR] : < 2e-16

                  Kappa : 0.4615

 Mcnemar's Test P-Value : 0.00269

            Sensitivity : 0.6655
            Specificity : 0.7964
         Pos Pred Value : 0.7683
         Neg Pred Value : 0.7013
             Prevalence : 0.5035
         Detection Rate : 0.3351
   Detection Prevalence : 0.4362
      Balanced Accuracy : 0.7310

       'Positive' Class : Yes"
MC("tree")
"Confusion Matrix and Statistics

          Reference
Prediction  No Yes
       No  235 167
       Yes  45 117

               Accuracy : 0.6241
                 95% CI : (0.5827, 0.6642)
    No Information Rate : 0.5035
    P-Value [Acc > NIR] : 5.493e-09

                  Kappa : 0.2505

 Mcnemar's Test P-Value : < 2.2e-16

            Sensitivity : 0.4120          
            Specificity : 0.8393
         Pos Pred Value : 0.7222
         Neg Pred Value : 0.5846
             Prevalence : 0.5035
         Detection Rate : 0.2074
   Detection Prevalence : 0.2872
      Balanced Accuracy : 0.6256

       'Positive' Class : Yes"

# rm(list=ls())

"Chapitre 5"
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