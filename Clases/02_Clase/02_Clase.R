# Preparación datos
# Instalamos las librerias que se van utilizar:
library(ggplot2)
library(dplyr)

# Luego de cargar la libreria especificar cada algoritmo
install.packages("caret")
library(caret)

# Cargamos los datos. Seguiremos con los datos de "iris"
data(iris)
summary(iris)
head(iris,5)
# Cuantos registros son
len_muestra<-nrow(iris)
len_muestra

# Definimos los sets de entrenamiento (En este caso será 80% - 20%)

set.seed(2023)
sample_muestra<-sample(len_muestra, len_muestra)
print(paste("Muestra aleatoria:", len_muestra))
sample_muestra

len_train<-len_muestra*0.8
print(paste("Longitud muestra de entrenamiento", len_train))
sample_train<-sample(sample_muestra, len_train) # Muestra de entrenamiento
sample_train

sample_test<-sample_muestra[!sample_muestra %in% sample_train] # Muestra test
len_test<-length(sample_test) # Largo del dataset test
print(paste("Longitud muestra test:", len_test))
sample_test

# Asignamos los valores de la muestra al dataset "iris" y obtenemos las particiones del dataset
iris_train <- iris[sample_train, ]
iris_test <- iris[sample_test, ]

# Separamos la columna "Species" que utilizaremos como etiquetas

# Entrenamiento
iris_train_dataset<-iris_train[,-5]
iris_train_output<-iris_train[,5]

# Testeo
iris_test_dataset<-iris_test[,-5]
iris_test_output_real<-iris_test[,5]

# Se podría visualizar el resultado de los sets de train y test:
iris_train_dataset
iris_train_output
iris_test_dataset
iris_test_output_real

