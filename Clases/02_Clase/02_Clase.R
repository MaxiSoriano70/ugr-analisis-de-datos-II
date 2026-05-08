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

# ALGORITMO KNN
# Carga de paquetes
# Existen distintas librerias para utilizar este algoritmo en este caso utilizaremos "class"
library(class)
# help(knn)

# Creamos el modelo

iris_test_output_knn <- knn (
  train = iris_train_dataset,
  cl = iris_train_output,
  test = iris_test_dataset,
  k = 50
)

iris_test_output_knn

# Elaboramos la matriz de confusión
mc_knn <- table(iris_test_output_knn, iris_test_output_real)
print(paste("Matriz de confusión:"))
mc_knn

# Calculamos el accuracy del modelo: aciertos/predicciones
ac_knn <- mean(iris_test_output_knn == iris_test_output_real)
print(paste("Accuracy: ", ac_knn))

# En este tipo de algoritmos, es comun querer encontrar un valor de k vecinos óptimo
set.seed(2023)
k <- 1:100
knn_kvalues <- data.frame(k, accuracy= 0)

for(n in k){
  iris_test_output_knn <- knn (
    train = iris_train_dataset,
    cl = iris_train_output,
    test = iris_test_dataset,
    k = n
  )
  knn_kvalues$accuracy[n] <- mean(iris_test_output_knn == iris_test_output_real)
}

# Visualización de resultados
ggplot(knn_kvalues)+
aes(k, accuracy)+ 
geom_line(colour="blue", lwd = 1, alpha = 0.5) +
geom_point(colour="black")+
geom_smooth(colour="red", lwd = 0.5)+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Para consultar el data frame
knn_kvalues

