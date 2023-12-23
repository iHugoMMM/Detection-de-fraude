#----------------------------#
#  TESTS DE SIGNIFICATIVITE  #
#----------------------------#
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) #StringsAsFactors pour les variables qualitatives

fraud_data <- subset(fraud_data, select=-c(customer_id,claim_id))
View(fraud_data)
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data <- resample(fraud_data)
qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar", main="Fraudulent", xlab="Fraudulent", ylab="Nombre de cas")

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
 police_report     claim_type     claim_area         gender incident_cause (pas l'air très utile mais on compare)
  0.0000337742   0.0052221436   0.4750560347   0.5123374414   0.8852211944 
> pvfish_table()
 police_report     claim_type         gender     claim_area incident_cause 
  8.821351e-06   3.680532e-03   4.750903e-01   4.829364e-01   8.839457e-01"
  
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
'[1] "p-value Pearson =  0.000183065573093701"
[1] "p-value Spearman =  0.000177558752583205"
[1] "p-value Kendall =  0.00018434538924881"'
psk("days_to_incident")
'[1] "p-value Pearson =  3.9899884800508e-05"
[1] "p-value Spearman =  1.81355946546409e-07"
[1] "p-value Kendall =  2.11595814151737e-07"'
psk("claim_amount")
'[1] "p-value Pearson =  0.455960214966064"
[1] "p-value Spearman =  0.0703230077916243"
[1] "p-value Kendall =  0.0703407348935491"'
psk("total_policy_claims")
'[1] "p-value Pearson =  0.00980647494827477"
[1] "p-value Spearman =  0.0372912761703214"
[1] "p-value Kendall =  0.0373492251937505"'

#-------------------------#
#  GRAPHIQUE EN MOZAIQUE  #
#-------------------------#
"Mosaic plot de représentation chi2-residuals de toutes les variables par rapport à la classe étudiée fraudulent"
"Avec gender"
mosaicplot(table(fraud_data$gender, fraud_data$fraudulent), shade = TRUE, main = "age", xlab = "age", ylab = "fraudulent", color = TRUE)
"Incident cause"
mosaicplot(table(fraud_data$incident_cause, fraud_data$fraudulent), shade = TRUE, main = "age", xlab = "age", ylab = "fraudulent", color = TRUE)
"Claim_area"
mosaicplot(table(fraud_data$claim_area, fraud_data$fraudulent), shade = TRUE, main = "age", xlab = "age", ylab = "fraudulent", color = TRUE)
"Police_report"
mosaicplot(table(fraud_data$police_report, fraud_data$fraudulent), shade = TRUE, main = "age", xlab = "age", ylab = "fraudulent", color = TRUE)
"Claim_type"
mosaicplot(table(fraud_data$claim_type, fraud_data$fraudulent), shade = TRUE, main = "age", xlab = "age", ylab = "fraudulent", color = TRUE)

"Discretisation supervisée à faire"
"Age"
fraud_data$age <- cut(fraud_data$age, breaks = 5)
mosaicplot(table(fraud_data$age, fraud_data$fraudulent), shade = TRUE, main = "age", 
                 xlab = "age", ylab = "fraudulent", color = TRUE)
"Days_to_incident"
fraud_data$days_to_incident <- cut(fraud_data$days_to_incident, breaks = 8)
mosaicplot(table(fraud_data$days_to_incident, fraud_data$fraudulent), shade = TRUE, main = "days_to_incident", 
                 xlab = "days_to_incident", ylab = "fraudulent", color = TRUE)
"Claim_amount"
fraud_data$claim_amount <- cut(fraud_data$claim_amount, breaks = 10)
mosaicplot(table(fraud_data$claim_amount, fraud_data$fraudulent), shade = TRUE, main = "claim_amount", 
                 xlab = "claim_amount", ylab = "fraudulent", color = TRUE)
"Total_policy_claims"
fraud_data$total_policy_claims <- cut(fraud_data$total_policy_claims, breaks = 5)
mosaicplot(table(fraud_data$total_policy_claims, fraud_data$fraudulent), shade = TRUE, main = "total_policy_claims", 
                 xlab = "total_policy_claims", ylab = "fraudulent", color = TRUE)

"A quel point il va être difficile de crée un arbre de décision : proche de 1 plus compliqué, proche de 0 plus simple"
entropy(table(fraud_data$fraudulent))
"attrEval sert pour savoir quelles sont les variables les plus significatives"
Eval <- function(estimator){
  attr <- attrEval(fraudulent~., fraud_data, estimator = estimator)
  attr <- sort(attr, decreasing = TRUE)
  return(attr)
}
"GainRatio"
Eval("GainRatio")
"Gini"
Eval("Gini")
"Accuracy"
Eval("Accuracy")
"Relief"
Eval("Relief")
"MDL"
Eval("MDL")

#-------------------------#
#  Chargement de données  #
#-------------------------#
"Réequilibrage et suppression des variables non significatives"
fraud_data <- read.csv("Donnees/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) 
qplot(fraudulent, data=fraud_data, fill=fraudulent, geom="bar", main="Fraudulent", xlab="Fraudulent", ylab="Nombre de cas") 
resample <- function(fraud_data){
  resample <- ovun.sample(fraudulent ~ ., data = fraud_data, method = "over", N = 1.539 * length(fraud_data$fraudulent))
  indices <- sample(nrow(resample$data))
  resample <- resample$data[indices,]
  return(resample)
}
fraud_data_sample <- resample(fraud_data)
qplot(fraudulent, data=fraud_data_sample, fill=fraudulent, geom="bar", main="Fraudulent", xlab="Fraudulent", ylab="Nombre de cas")
"Nombre de lignes dans fraud_data_sample"
nrow(fraud_data_sample)
"2/3 de 1692 pour fraud_data_EA et 1/3 pour fraud_data_ET"
fraud_data_EA <- fraud_data_sample[1:1128,]
fraud_data_ET <- fraud_data_sample[1129:1692,]
"On enlève les variables non significatives"
fraud_data_EA <- subset(fraud_data_EA, select=-c(customer_id, claim_id, claim_area, gender, incident_cause))
View(fraud_data_EA)
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
AUC("rpart")
AUC("C5.0")
AUC("tree")
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

"On vide le global environment"
rm(list=ls())
