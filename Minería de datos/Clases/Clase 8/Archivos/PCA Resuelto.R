rm(list = ls())

library(corrplot)

setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Clases\\Clase 8\\Archivos"
))

# Datos a utilizar
datos <- read.csv(file = "Fidelizacion.csv", sep = ";", header = TRUE)

# Correlaciones de las variables originales
# La columna 11 es la variable a modelar
corrplot(cor(datos), method = "circle")

# PCA para las 28 variables explicativas
PCA <- prcomp(datos[,1:10], scale = TRUE)

# Varianza explicada
summary(PCA)

# Matriz de carga
PCA
# Todas las componentes no tiene correlación entre sí
corrplot(cor(PCA$x), method="circle")

# Construmos un nuevo data frame con los 4 mejores componentes
datos2 <- as.data.frame(
  cbind(PCA$x[,1:4], Satisfaccion = datos$Satisfaccion)
)

# Correlaciones de las nuevas variables
# La columna 5 es la variable a modelar
corrplot(cor(datos2), method="circle")


# Regresion Lineal
# Con las 10 variables originales
modelo1 <- lm(
  formula = Satisfaccion ~ .,
  data = datos
)
summary(modelo1)

# Con 4 componentes
modelo2 <- lm(
  formula = Satisfaccion ~ .,
  data = datos2
)
summary(modelo2)
