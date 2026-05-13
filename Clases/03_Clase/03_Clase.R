# INICIO
# Librerias
library(tidyverse)
library(ggplot2)
library(readr)

#PREPARACIÓN DE DATOS 

# Vamos a buscar definir cual sera nuestra variable a predecir y tomar las variables que explicaran ese comportamiento.
# Luego , definiremos los datos de train y test

# Nuevamente convocamos la base de datos a trabajar (seguimos con petroleo)
# Llamamos a la base de datos
base <- read.csv("C:/Users/MAXY 70/Documents/Ciencia_de_datos_UGR/Clases/03_Clase/produccion-de-pozos-de-gas-y-petroleo-no-convencional.csv", 
                 header = TRUE, 
                 sep = ",", 
                 encoding = "UTF-8")

# Repasando nuestro analisis exploratorio definimos que variables vamos a elegir
analisis1 <- base %>% select(prod_pet, profundidad, cuenca)

str(analisis1)

# OUTLIERS
# Profundidad es q
q <- quantile(analisis1$profundidad, probs = c(.25, .75), na.rm = TRUE)
iqr <- IQR(analisis1$profundidad, na.rm = TRUE)
limite_inferior <- q[1] - 1.5*iqr
limite_superior <- q[2] + 1.5*iqr

# FILTRO outliers de profundidad
analisis1 <- analisis1 %>% 
  filter(profundidad >= limite_inferior & profundidad <= limite_superior)

# DIMENSIONES
len_analisis1 <- nrow(analisis1)

# PARTICIONO
set.seed(2023)

# Definimos los sets train-test (replicamos la realcion 80% 20%)
sample_analisis1 <- sample(len_analisis1, len_analisis1) # Definimos los numeros que vamos a

print(paste("Muestra aleatoria:", len_analisis1))
sample_analisis1

# Definimos la longitud de la muestra de entrenamiento (train). En este caso comenzamos tomando el 80%
len_train <- len_analisis1*0.8 # Aca tomo el 80%
print(paste("Longitud de la muestra train:", len_train))
sample_train <- sample(sample_analisis1, len_train)
sample_train

sample_test <- sample(sample_analisis1[!sample_analisis1%in%sample_train]) # Aqui pido que tome los datos para el test
# Con este 20% restante sacamos los valores que seran parte del test
len_test <- length(sample_test)
print(paste("Longitud muestra test:", len_test))
sample_test

# Asignamos los vlaores de las muestras en funcion alas particiones que le hicimos al dataset
analisis1_train <- analisis1[sample_train,]
analisis1_test <- analisis1[sample_test,]

# Como la variable a predecir sera la producción de petroleo separamos esa columna

# Train
analisis_train_dataset<-analisis1_train[,-1]
analisis_train_output<-analisis1_train[,1]

# Test
analisis_test_dataset<-analisis1_test[,-1]
analisis_test_output_real<-analisis1_test[,1]

# Visualizamos esos resultados
analisis_train_dataset
analisis_train_output
analisis_test_dataset
analisis_test_output_real


# METODO 2
set.seed(2023)

# Creamos las mascara de indices para la particion 80% 20%
sample_muestra_train <- sample(1:nrow(analisis1),0.8*nrow(analisis1))

# Creo los sets de train y test
analisis1_train2 <- analisis1[sample_muestra_train,]
analisis1_test2 <- analisis1[-sample_muestra_train,]

nrow(analisis1_train2)
nrow(analisis1_test2)

# Train
analisis_train_dataset2<-analisis1_train2[,-1]
analisis_train_output2<-analisis1_train2[,1]

# Test
analisis_test_dataset2<-analisis1_test2[,-1]
analisis_test_output_real2<-analisis1_test2[,1]


# APRENDIZAJE SUPERVISADO
# REGRESION LINEAL
# Convierto a variable categorica "Tipo de cuenca"
library(lattice)
library(caret)

# Uso dummyVars() para crear las variables binarias
dny <- dummyVars("~ .", data = analisis_train_dataset)

# Aplica la transformación al data frame de entrenamiento
analisis_train_dataset_codificado <- data.frame(predict(dny, newdata = analisis_train_dataset))

# Como tenia mi df antes
head(analisis_train_dataset)

# Como lo tengo ahora codificado
head(analisis_train_dataset_codificado)

# Modelo de regresión lineal
analisis1_rLineal <- lm(analisis_train_output ~ ., data = analisis_train_dataset_codificado)

# Aplica la transformación al data frame de testeo
analisis_test_dataset_codificado <- data.frame(predict(dny, newdata = analisis_test_dataset))

# Predicción con el set de test
predicciones <- predict(analisis1_rLineal, newdata = analisis_test_dataset_codificado)
df_resultados <- data.frame(
  Real = analisis_test_output_real,
  Predicho = predicciones
)

# Grafico Real vs Predicho
ggplot(
  df_resultados,
  aes(x = Real, y = Predicho)
) +
  geom_point(alpha = 0.3, color = "steelblue") +   # puntos semitransparentes
  geom_abline(slope = 1, intercept = 0,             # linea perfecta Real = Predicho
              color = "red", linetype = "dashed") +
  labs(title = "Regresión Lineal: Real vs Predicho",
       x = "Valor Real",
       y = "Valor Predicho") +
  theme_minimal()

summary(base)

# APRENDIZAJE SUPERVISADO KNN
library(class)

# Ahora correr KNN
analisis_test_output_knn <- knn(
  train = analisis_train_dataset,
  cl    = analisis_train_output,
  test  = analisis_test_dataset,
  k     = 50
)

analisis_test_output_knn

# Matriz de confusión
mc_knn <- table(analisis_test_output_knn, analisis_test_output_real)
print(paste("Matriz de confusión: "))
mc_knn

# Calculamos el accuracy del modelo aciertos / predicciones
ac_knn <- mean(analisis_test_output_knn == analisis_test_output_real)
print(paste("Accuracy: ", ac_knn))

# Buscamos el mejor K
set.seed(2023)
k <- 1:100
knn_kvalues <- data.frame(k, accuracy = 0)

for(i in k){
  knn_temp <- knn(
    train = analisis_train_dataset,
    cl    = analisis_train_output,
    test  = analisis_test_dataset,
    k     = i
  )
  knn_kvalues$accuracy[i] <- mean(knn_temp == analisis_test_output_real)
}

# Graficamos accuracy por valor de K
ggplot(knn_kvalues, aes(x = k, y = accuracy)) +
  geom_line(color = "steelblue") +
  geom_point(size = 1) +
  labs(title = "Accuracy por valor de K",
       x = "K",
       y = "Accuracy") +
  theme_minimal()

# Mejor K
mejor_k <- knn_kvalues$k[which.max(knn_kvalues$accuracy)]
print(paste("Mejor K:", mejor_k))

