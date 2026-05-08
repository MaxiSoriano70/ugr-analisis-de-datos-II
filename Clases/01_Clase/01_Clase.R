library(tidyverse)
library(ggplot2)

install.packages("dplyr")
# Inicio del ejemplo: DataSet Flores Iris
data("iris")
attach(iris)
iris
dim(iris)
help(iris)
head(iris)
tail(iris)
# View permite ver la tabla
View(iris)
# Resumen matematico minimo maximo cuartiles etc
summary(iris)

# Flores iris con ggplot2
# Este grafico contiene los ejes que especificamos pero no contiene datos
ggplot(
  iris,
  aes(Sepal.Length, Petal.Length))

# Con puntos
ggplot(
  iris,
  aes(Sepal.Length, Petal.Length)) + geom_point()

# Con línea de tendencia
ggplot(
  iris,
  aes(Sepal.Length, Petal.Length)) + geom_point() +
  geom_smooth(method = "lm")

# Agregamos color por especie
ggplot(
  iris,
  aes(Sepal.Length, Petal.Length, color = Species)) + 
  geom_point() +
  geom_smooth(method = "lm")

# aes
ggplot(
  iris, aes(Sepal.Length, Petal.Length, color = Species)
)

# Agregamos linea de tendencia. Notar la diferencia segun la posicion del des()
ggplot(iris, aes(Sepal.Length, Petal.Length))+
  geom_point(aes(color = Species))+
  geom_smooth(method = "lm")

# Gráfico de úntos puntos
p1 <- ggplot(iris, aes(Species, Sepal.Length))+
  geom_point()
p1

# Grafico de puntos con ruido en el eje horizontal
p2 <-ggplot(iris, aes(Species, Sepal.Length))+
  geom_jitter(width = 0.1)
p2

# Boxplot
bp1 <- ggplot(iris, aes(Species, Sepal.Length))+
  geom_boxplot(color="lightblue", fill="white",
               lwd=0.6, width=0.15)
bp1

# Violin
v1 <- ggplot(iris, aes(Species, Sepal.Length))+
  geom_violin()
v1

# Combinado boxplot y violin
comb1 <- ggplot(iris, aes(Species, Sepal.Length))+
  geom_violin(fill="blue", alpha=0.45)+
  geom_boxplot(color="lightblue", fill="white",
               lwd=0.6, width=0.15)
comb1

# Grafico de barras con medias
b1 <- ggplot(iris, aes(Species, Sepal.Length))+
  stat_summary(fun.y = mean, geom = "bar",
               width=0.2)

# Facet
ggplot(iris, aes(Species, Sepal.Length))+
  geom_point()+
  facet_wrap(-Species)

ggplot(iris, aes(Species, Sepal.Length))+
  geom_point(aes(color=Petal.Width))+
  facet_wrap(~Species)

# Complot
# Si es la primera vez
install.packages("cowplot")

# Graficar dos graficos en el mismo panel
cowplot::plot_grid(p1,p2,bp1,comb1,b1, labels="AUTO")

