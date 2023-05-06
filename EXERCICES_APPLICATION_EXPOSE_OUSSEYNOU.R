
# **************EXERCICES EXPOSES(OUSSEYNOU)*******
library(haven)
data<-read_dta("E:\\KPAM ISEP 2\\Semestre 4\\Programmation R\\Cours R 2023\\EXERCICES\\cereales.dta")
View(data)
#Application   la base  céréales des fonctions suivantes :sapply ; copy (data.table);impute(Hmisc); with; if_else ;
#na.locf(zoo); lapply(); mice(mice); knn(KNN); missForest(missForest)

#copie de la base cerales

library(data.table)
data1<-copy(data)
#imputation des valeurs manquantes avec la bibliothèque Hmisc dans la 4e variable par exemple


#imputation par la mediane
install.packages("Hmisc")
library(Hmisc)
data1$s07Bq03a_cereales <- with(data1, impute(s07Bq03a_cereales, median))
# imputation par methode de regression linéaire

model <- lm(s07Bq03a_cereales ~ ., data = data1)
data1$s07Bq03a_cereales <- ifelse(is.na(data1$s07Bq03a_cereales), predict(model, newdata = data1), data1$s07Bq03a_cereales)
##Imputation par la méthode de LOCF(Last Observation Carried Forward) 

data1$s07Bq03a_cereales <- na.locf(data1$s07Bq03a_cereales ) 
##  Imputation par la méthode de Monte carlo

library(mice)
data1 <- data.frame(lapply(data, factor))

# Convertir les variables qualitatives en variables facteur
m <- 1 # nombre d'itérations
imp <- mice(data, m = m, method = "norm.predict")
#Imputation par les K plus proche voisins


library(VIM)

dat.kNN=kNN(data1, k=5, imp_var=FALSE)

#Imputation par la Foret aleatoire


library(missForest)
mifa.imp <- missForest(data1)
mifa.complete <- mifa.imp$ximp
View(mifa.complete)


  
