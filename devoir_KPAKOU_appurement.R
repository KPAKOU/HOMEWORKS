#*******************EXERCICE1*****************************

#importation des données
library(haven)
data<-read_dta("E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\EXERCICES\\cereales.dta")
View(data)
#renomons les variables

old_name <- colnames(data)[1:14]
old_name #affichage des anciens noms
new_name <- c(old_name[1],old_name[2],old_name[3],"autre_cereales", "quanite_cons", 
              "unites_cons","taille_cons","provenance_auto","provenance_other",
              "freq_achat","quatite_achat", 
              "unite_achat","taille_achat","value_lastachat")
new_name #affichage de nouveaux noms
colnames(data) <- new_name
#description des variables

summary(data)
#imputation

base<-read.csv2("E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\BASES\\Table de conversion phase 2.csv")
library(dplyr)
base<-select(base,-c(X,X.1,X.2,X.3,X.3,X.4,X.5,X.6,X.7,X.8,X.9,X.10,X.11,X.12,X.13,X.14,X.15,X.16,X.17))
View(base)

#description de la variable poids

summary(base$poids)
class(base$poids)
#-création d'une clé pour les deux bases

prod<-as.character(base$produitID) # transformons les variables en caractères
taille<-as.character(base$tailleID)
unite<-as.character(base$uniteID)
base$ID<-paste(prod, taille, unite,sep = "- ")
key<-base$ID
View(key)
##8-fusionnons les deux bases


produit1 <- subset(base, select = c("produitID","produitNom"))

#verification de la presence des doublons dans produit1
duplicated(produit1)

#suppression des doublons dans la variable produit1

Produit<-distinct(produit1)
View(Produit)
## creer une nouvelle variable dans la base cereales data contenant les produit
# liste des produits 

colnames(data)[3]<-"produitID"
data$produit <-factor(data$produitID,
                           levels = Produit$produitID, 
                           labels = Produit$produitNom) 
glimpse(data)
## on fait  de  mme chose pour les unités et les tailles

uniteCons <- subset(base, select = c("uniteID", "uniteNom"))
tailleCons <- subset(base, select = c("tailleID", "tailleNom"))
## supression de doublons

uniteCons_df <- distinct(uniteCons)
tailleCons_df <- distinct(tailleCons)
data$unites <- factor(data$unites_cons, 
                           levels = uniteCons_df$uniteID, 
                           labels = uniteCons_df$uniteNom)
data$tailles <- factor(data$taille_cons,
                            levels = tailleCons_df$tailleID,
                            labels = tailleCons_df$tailleNom)
labels(data$unites,"Unités de consommation",data = data)
labels(data$tailles, "tailles des unités de consommation",data = data)
labels(data$produit, "Produit de consommations",data = data)

glimpse(data)

#on peut maintenant fusionner les deux bases

Fusion_data<-merge(data,base,by="produitID")
View(Fusion_data)

#9-conversions des quantités consommées en unités standart
#variable pour calculer le poids

class(Fusion_data$poids)
class(Fusion_data$quanite_cons)
# transformons poids en numerique

Fusion_data<-na.omit(Fusion_data)
Fusion_data$Poids<-as.numeric(Fusion_data$poids)

attach(Fusion_data)

Fusion_data$Qte_cons<-((Fusion_data$quanite_cons*Fusion_data$poids/1000)/7)*360
View(Fusion_data)
# 10-Generationde taille_men

set.seed(123)
dim(Fusion_data)
# Générer 68755 nombres aléatoires selon une distribution normale

alea <- rnorm(68755, mean = 0, sd = 1)
# Ajuster les valeurs pour qu'elles correspondent à l'intervalle [1,30] et la moyenne de 9
Fusion_data$taille_men<- pmax(pmin(round(alea * 6 + 9), 30), 1)



 #********************EXERCICE 2**********************
 #1-Fonction d'importation des données
import<-function(chemin){
  #verifier si le chemin existe
  if (!file.exists(chemin)) {
    stop("Le fichier n'existe pas.")
  }
  
  # Importer les données en utilisant le type de fichier approprié
  extension <- tools::file_ext(chemin)
  if (extension == "csv") {
    data <- read.csv(chemin)
  } else if (extension == "xlsx" || extension == "xls") {
    data <- readxl::read_excel(chemin)
  } else if (extension == "dta"){
    data<-read_dta(chemin)
  }else {
    stop("Format de fichier non supporté.")
  }
  return(data)
  View(data)
}
#application
my_data<-import("E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\EXERCICES\\cereales.dta")

#2-Fonction de renommage des variables :
#old_names et new_names ,des listes contenant les anciens noms et nouveaux noms
rename <- function(data, old_names,new_names) {
  # Vérifier que les noms existent
  if (!all(old_names %in% names(data))) {
    stop("Noms de variables inexistants dans les données.")
  }
  
  # Vérifier que les noms de remplacement n'existennt pas déjà
  if (any(new_names %in% names(data))) {
    stop("Noms de variables de remplacement déjà existants dans les données.")
  }
  
  # Renommer les variables
  names(data)[match(old_names, names(data))] <- new_names
  
  # Retourner les données avec les noms de variables renommés
  return(data)
}

#3- Fusion de deux base de données

merge_data <- function(data1, data2, by_vars) {
  # Vérifier que les variables de fusion existent dans les deux bases de données
  if (!all(by_vars %in% names(data1)) || !all(by_vars %in% names(data2))) {
    stop("Variables de fusion inexistants dans les données.")
  }
  
  # Fusionner les données
  merged_data <- merge(data1, data2, by = by_vars, all = TRUE)
  
  # Retourner la base de fusion
  return(merged_data)
}

#4-Detection des valeurs manquantes

missing_value <- function(data) {
  # Vérifier si des valeurs manquantes sont présentes
  missing_values <- is.na(data)
  # Retourner le nombre de valeurs manquantes par variable
  return(missing_values)
}
#5_Fonction qui impute les valeurs manquantes
# on va imputer les valeurs manquantes selonla methode choisie
imputation <- function(data, methode){
  # verification de l'existance des valeurs manquantes
  missing_values <- is.na(data)
  # Imputer les valeurs manquantes avec la méthode choisie
  if (method == "mean") {
    imput_data <- replace(data, missing_values, mean(data, na.rm = TRUE))
  } else if (method == "median") {
    imput_data <- replace(data, missing_values, median(data, na.rm = TRUE))
  }  else {
    stop("Méthode d'imputation non connu.")
  }
}

#6-Fonction qui détecte les valeurs aberrantes 

outliers <- function(x, a= 1.5) {
  #calcul des quartiles
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  #calcul de l'intervalle interquartile
  interval_interquartile <- qnt[2] - qnt[1]
  borne_inf <- qnt[1] - a * interval_interquartile #borne inf du boxplot
  borne_sup <- qnt[2] + a * interval_interquartile #borne sup du boxplot
  outliers <- x < inf | x > sup
  return(outliers)
}

#7- Correction des valeur abberantes
#supposons que nous corrigeons les valeurs aberantes par la moyenne
#utilisions la fonction outliers precedente
correction_outliers <- function(data, a= 1.5) {
  data_correct <- data
  
  # Parcourir les colonnes du data.frame
  for (col in 1:ncol(data)) {
    col_name <- colnames(data)[col]
    outliers <- outliers(data[[col]], a)
    
# Si des valeurs aberrantes sont détectées dans la colonne alors on les remplace par la moyenne
    
    if (sum(outliers) > 0) {
      mediane<- median(data[[col]], na.rm = TRUE)
      data_correct[[col]][outliers] <- mediane
      cat("Valeurs aberrantes corrigées dans la colonne", col_name, "\n")
    }
  }
  
  return(data_correct)
}







  
