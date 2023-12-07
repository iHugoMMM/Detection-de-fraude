#----------------------------#
#  TESTS DE SIGNIFICATIVITE  #
#----------------------------#
fraud_data <- read.csv("S1/R/Traitement de donnees/Projet/Data_Projet_1.csv", 
                    header = TRUE, sep = ",", dec = ".", stringsAsFactors = T) #StringsAsFactors pour les variables qualitatives

fraud_data <- subset(fraud_data, select=-c(customer_id,claim_id))
View(fraud_data)
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
  variable_names <- c("gender", "incident_cause", "claim_area", "police_report", "claim_type", "fraudulent")
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
  variable_names <- c("gender", "incident_cause", "claim_area", "police_report", "claim_type", "fraudulent")
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
chisq.test(fraud_data$gender, fraud_data$fraudulent)
chi2(fraud_data$gender)
pvchi2_table()
pvfish_table()

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
  print(paste("p-value de pearson de la variable", variable, "est :", pearson(variable)))
  print(paste("p-value de spearman de la variable", variable, "est :", spearman(variable)))
  print(paste("p-value de kendall de la variable", variable, "est :", kendall(variable)))
}
psk("age")
psk("days_to_incident")
psk("claim_amount")
psk("total_policy_claims")

#DERNIER TD
View(fraud_data)
Entropy(table(fraud_data$fraudulent))
attr <- attrEval(fraudulent~., fraud_data, estimator = "GainRatio")
attr <- sort(attr, decreasing = TRUE)
attr
attr2 <- attrEval(fraudulent ~ ., fraud_data, estimator = "Gini")
attr2 <- sort(attr, decreasing = TRUE)
attr2
attr3 <- attrEval(fraudulent ~ ., fraud_data, estimator = "Accuracy")
attr3 <- sort(attr, decreasing = TRUE)
attr3

"Conclusion :
Variable                Importance
------------------------------------
claim_amount            0.2961
total_policy_claims     0.0622
days_to_incident        0.0452
age                     0.0413
police_report           0.0112
claim_type              0.0055
claim_area              0.0010
gender                  0.0003
incident_cause          0.0003
"
