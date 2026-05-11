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

# ALGORITMO REGRESÓN LINEAL
# Creamos el modelo de regresión lineal para clasificación con el set train
iris_rLineal <- lm(Species ~ Petal.Length + Petal.Width, data = iris_train)

# Predicción con el set 
predicciones <- predict(iris_rLineal, newdata = iris_test)

# Definir los limites para la clasificación
limites <- c(0, quantile(predicciones, probs = c(0.33, 0.67, 1)))

# Convertir las predicciones en clases
iris_test_output_rLineal <- cut(predicciones, breaks = limites, labels = c("setosa", "versicolor", "virginica"))

# Recordar que ya estaba creado el vector de clases reales
iris_test_output_real

# Matriz de confusión
mc_rLineal <- table(iris_test_output_real, iris_test_output_rLineal)
print("Matriz de confusión: ")
print(mc_rLineal)

# Accuracy
ac_rLineal <- mean(iris_test_output_rLineal == iris_test_output_real)
print(paste("Accuracy: ", ac_rLineal))

# Gráfico
library(ggplot2)

ggplot(data = iris_train,
       aes(x = Petal.Length,
           y = Petal.Width,
           color = Species)) +
  geom_point() +
  geom_abline(
    intercept = coef(iris_rLineal)[1],
    slope = coef(iris_rLineal)[2],
    color = "red",
    lwd = 0.5
  ) +
  labs(
    title = "Regresión Lineal para Clasificación",
    x = "Petal Length",
    y = "Petal Width"
  )

# ALGORITMO ÁRBOL

# Carga de paquetes
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# Crear árbol de decisión
iris_ad <- rpart(
  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris,
  method = "class"
)

# Visualización del árbol
rpart.plot(iris_ad, type = 2, extra = 1)

# -----------
# Para realizar un árbol de decisión para la clasificación a partir de un conjunto de entrenaiento
# Crear un árbol de decisión con rpart.plot
iris_ad <- rpart(
  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris_train,
  method = "class"
)

# Visualizar el árbol de decisión con rpart.plot
rpart.plot(iris_ad, type = 2, extra = 1)

# Realizar las predicciones con el árbol de decisión en el conjunto de prueba
iris_test_output_ad <- predict(
  iris_ad,
  iris_test,
  type = "class"
)

# Recordar que ya estaba creado elvectro de clases reales
iris_test_output_real

# Matriz de confusión
mc_ad <- table(iris_test_output_real, iris_test_output_ad)
print("Matriz de Confusión:")
print(mc_ad)

# Calcular la exactitud en el conjunto de prueba
ac_ad <- mean(iris_test_output_ad == iris_test_output_real)

# /nrow(iris_test)
print(paste("Accuracy: ", ac_ad))

# ALGORITMO RANDOM FOREST
# Instalar
install.packages("randomForest")
library(randomForest)
# Ajustamos un modelo de Random forest
iris_rf <- randomForest(iris_train_output ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                        data = iris_train,
                        ntree = 1000)

# Visualizar el Random Forest
plot(iris_rf)

# Realizar la predicción cin el árbol de decisión en el conjunto de prueba
iris_test_output_rf <- predict(iris_rf, iris_test)
iris_test_output_rf

# Recordar que ya estaba creado el vector de clases reales
iris_test_output_real

# Matriz de confusión
mc_rf <- table(iris_test_output_real, iris_test_output_rf)
print("Matriz de Confusión:")
print(mc_rf)

# Calcular la exactitud en el conjunto de prueba
ac_rf <- mean(iris_test_output_rf == iris_test_output_real)

# /nrow(iris_test)
print(paste("Accuracy: ", ac_rf))

# CALCULAR LA IMPORTANCIA DE KAS VARIABLES
importancia_variables <- importance(iris_rf)

# Mostrar la importancia de las variables
print("Importancia de las Variables")
print(importancia_variables)

# Visualizar la importancia de las variables
varImpPlot(
  iris_rf,
  main = "Importancia de Variable en el Modelo de Random Forest",
  pch = 19, #Tipo de punto
  col = "royalblue",
  cex.axis = 0.8, #Tamaño de la fuente de ejes
  cex.lab = 1.2
)

# Agregar una leyenda 
legend("topright", #Posición de la leyenda
       legend = c("Importancia"), #Etiqueta de la leyenda
       fill = c("blue"), #Color de la leyenda
       bty = "n") #Sincaja alrededor de la leyenda

# tambien podemos visualizar sin personalizarlo
varImpPlot(iris_rf)

# ALGORITMO NAIVE BAYES
install.packages("ggplot2")
library(ggplot2)

ggplot()
aes()
geom_point()
theme()
element_text()


install.packages("e1071")
library(e1071)

# Crea un modelo de Naive Bayes
iris_nb <- naiveBayes(Species ~ ., data = iris_train)

# Realiza predicciones en el conjunto de prueba
iris_test_output_nb <- predict(iris_nb, iris_test)

# Matriz de confusión
mc_nb <- table(iris_test_output_real, iris_test_output_nb)
print("Matriz de Confusión:")
print(mc_nb)

# Calcular la exactitud en el conjunto de prueba
ac_nb <- mean(iris_test_output_nb == iris_test_output_real)

# /nrow(iris_test)
print(paste("Accuracy: ", ac_nb))

# Crear un nuevo dataframe con las predicciones y las caracteristicas originales
iris_test_output_nb_results <- data.frame(iris_test, Prediccion = iris_test_output_nb)

# Grafico
plot_nb <- ggplot(iris_test_output_nb_results,
                  aes(x = Sepal.Length,
                      y = Sepal.Width,
                      color = Species,
                      shape = Prediccion)) +
  geom_point(size = 3) + #Aumenta el tamaño de los puntos
  ggtitle("Clasificación con Naive Bayes en Conjunto de los puntos") +
  labs(
    color = "Real",
    shape = "Predicción",
    x = "Largo del Sépalo",#Ajusta el nombre del eje X
    y = "Ancho del Sépalo"#Ajusta el nombre del eje Y
  ) +
  theme(
    axis.text = element_text(size = 12), #Ajusta el tamaño del texto de los ejes
    axis.title = element_text(size = 14) #Ajusta el tamaño del titulo de los ejes
  )

plot_nb
