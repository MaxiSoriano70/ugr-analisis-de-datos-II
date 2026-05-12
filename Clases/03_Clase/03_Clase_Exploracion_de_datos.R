# INICIO
# Librerias
library(tidyverse)
library(ggplot2)

# install.packages("readr")
library(readr)

# Llamo al archivo con el que voy a trabajar
base <- read.csv("produccion-de-pozos-de-gas-y-petroleo-no-convencional.csv", 
                 header = TRUE, 
                 sep = ",", 
                 encoding = "UTF-8")

# Podemos ver parte de nuestra base co elcomando view()
View(base)

# Podemos ver ascpectos basicos de cada una de las variables en modo de resumen
summary(base)

# Graficamos con GGPLOT
# Probamos la grafica
ggplot(base,
       aes(anio, prod_pet)
       ) + geom_point()

# Agregamos linea de tendencia
ggplot(base,
       aes(anio, prod_pet)) + 
  geom_point(alpha = 0.3) +        # puntos semitransparentes
  geom_smooth(method = "lm") +
  labs(title = "Producción de petróleo por año",
       x = "Año",
       y = "Producción de petróleo")

# Ahora vamos colocando colores oar aidentificar que tipo de extracción predomino por año
ggplot(base,
       aes(anio, prod_pet, color = tipoextraccion)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Producción de petróleo por año",
       x = "Año",
       y = "Producción de petróleo",
       color = "Tipo de extracción")

# Graficamos las otras variables para ver como se comporta el resto del corpus
# vemos la relacion entre las provincias y la cantidad de gas extraido

bp1 <- ggplot(base, aes(cuenca, prod_gas))+geom_boxplot() #Almacenamos el grafico en una variable
bp1

# Resulta que tenemos outliler altos en Neuquen, lo que hace que la distribución sea muy asimetrica
count(base)
