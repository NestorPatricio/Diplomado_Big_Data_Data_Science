rm(list = ls())

setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Clases\\Clase 8\\Archivos"
))

library(jpeg)
library(cluster)

# Cargamos la imagen
imagen <- readJPEG("doges.jpg")

# Dimensiones de la imagen
dimension <- dim(imagen)

# Creamos un data frame con 5 variabes
# 2 variables de ubicacion
# 3 variables de color
Foto <- data.frame(
  x = rep(1:dimension[2], each = dimension[1]),
  y = rep(dimension[1]:1, dimension[2]),
  valorR = as.vector(imagen[,,1]),
  valorG = as.vector(imagen[,,2]),
  valorB = as.vector(imagen[,,3])
)

summary(Foto)

# Graficamos la imagen
plot(
  y ~ x,
  data = Foto,
  col = rgb(Foto[c("valorR", "valorG", "valorB")]),
  asp = 1, pch = "."
)


# Clustering basado en RGB
RGB_Foto = Foto[, c("valorR", "valorG", "valorB")]

# Algoritmo K Means con K=3
clusters <- kmeans(RGB_Foto, centers = 3)

# Cada clúster se representa por su color promedio
clusters$centers
colores <- rgb(clusters$centers[clusters$cluster, ])

# Graficamos los clusters
plot(
  y ~ x,
  data = Foto,
  col = colores, 
  asp = 1, pch = "."
)


# Clustering basado en RGB y ubicación

# Los datos de posición están en otra escala, si que nos normalizamos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

Foto_norm <- as.data.frame(lapply(Foto, normalize))
summary(Foto_norm)

# Algoritmo K Means con K=3
clusters <- kmeans(Foto_norm, centers = 3)

# Cada cluster se representa por su color promedio
clusters$centers
colores <- rgb(
  red = clusters$centers[clusters$cluster,3],
  green = clusters$centers[clusters$cluster,4],
  blue = clusters$centers[clusters$cluster,5]
)

# Graficamos los clusters
plot(
  y ~ x,
  data = Foto,
  col = colores, 
  asp = 1, pch = "."
)
