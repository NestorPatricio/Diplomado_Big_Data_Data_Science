# Instalamos los paquetes necesarios, en caso que no los tengamos instalados
install.packages("cluster")
install.packages("factoextra")
install.packages("tidyverse")

# Cargamos las librerias que utilizaremos
library(cluster)
library(factoextra)
library(tidyverse)

# Indicamos el directorio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Clases/Clase 6'
))

# Cargamos la base de datos
datos <- read.csv('faithful.csv', sep=";",header=TRUE)

# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datos, normalize))

# K Means con K = 3
k3 <- kmeans(
  x = datos_norm,
  centers = 3,
  nstart = 25
)

# Analicemos los resultados
# Tamaño de cada clúster
k3$size
# clúster al que pertenece cada observación
k3$cluster
# Centros de cada clúster
k3$centers
# Suma cuadrática inicial de distancias
k3$totss
# Sumas cuadráticas de distancias dentro de cada clúster
k3$withinss
k3$tot.withinss


# Apliquemos el codo
clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(
    datos_norm,
    centers = k,
    nstart = 25
  )$tot.withinss
}
plot(clusters, codo, type = "l")


# Calculemos los valores de silueta para nuestros clústers
silueta_k3 <- silhouette(k3$cluster, dist(datos_norm))
summary(silueta_k3)

# La primera columna es el clúster de cada observación
silueta_k3[,1]
# La segunda columna es el clúster vecino de cada observación
silueta_k3[,2]
# La tercera columna es el valor de silueta de cada observación
silueta_k3[,3]

# Grafiquemos la silueta
silueta <- c(0,0,0,0,0,0,0,0,0)
for (k in 2:10) { # La función silhouette no acepta k=1, porque no tiene vecinos
  modelo_aux <- kmeans(
    datos_norm,
    centers = k,
    nstart = 25
  )
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm))
  silueta[k] <- mean(silueta_aux[, 3])
}
plot(clusters, silueta, type = "l")

# Distribuciones de valores de silueta
fviz_silhouette(silueta_k3)


# Interpretemos que es cada clúster

# Agreguemos el clúster de cada observacion a los datos
datos$cluster3 <- k3$cluster

# Podemos analizar visualmente los clústeres
plot(datos$eruptions, datos$waiting, col = datos$cluster3)

# Podemos analizar las variables promedio de cada clúster
View(
  datos %>%
    group_by(cluster3) %>%
    summarise_all(mean)
)



## Juguemos un poco con los datos de la evaluacion 3a ##
datosT <- read.csv(
  file = '../../Evaluaciones/Evaluación 2/SouthGermanCredit.csv',
  sep =  ';',
  header = TRUE
)

# En este ejemplo sólo utilizaremos siete variables explicativas, por simplicidad
# Usted deberá usar toda la información

# Elegiremos como variables:
# amount: variable continua
# savings: variable ordinal numérica
# age: variable continua
# housing: variable categórica con 3 niveles
# credits: variable ordinal numérica
# persons: variable binaria
# foreign: variable binaria

datosT_small <- as.data.frame(cbind(
  amount = datosT$amount,
  savings = datosT$savings,
  age = datosT$age,
  housing_free = (datosT$housing == 1),
  housing_rent = (datosT$housing == 2),
  credits = datosT$credits,
  persons = datosT$persons,
  foreign = datosT$foreign
))


# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datosT_norm <- as.data.frame(lapply(datosT_small, normalize))

# Considerando la cantidad de niveles en la variable categórica
# Antes de cambiar el peso se debe normalizar.
datosT_norm$housing_free <- datosT_norm$housing_free / 3
datosT_norm$housing_rent <- datosT_norm$housing_rent / 3
  
# Codo
clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(
    datosT_norm,
    centers = k,
    nstart = 25
  )$tot.withinss
}
plot(clusters, codo, type = "l")

# Silueta
silueta <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux <- kmeans(
    datosT_norm,
    centers = k,
    nstart = 25
  )
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datosT_norm))
  silueta[k] <- mean(silueta_aux[, 3])
}
plot(clusters, silueta, type = "l")

# Usando K = 3
modeloT3 <- kmeans(
  datosT_norm,
  centers = 3,
  nstart = 25
)
modeloT3$size


# Usando K = 5
modeloT5 <- kmeans(
  datosT_norm,
  centers = 5,
  nstart = 25
)
modeloT5$size

# Agregamos los clústers a cada observación
datosT_small$cluster3 <- modeloT3$cluster
datosT_small$cluster5 <- modeloT5$cluster


# Valores promedios de variables en cada clúster con K = 3
View(
  datosT_small
    %>% group_by(cluster3)
    %>% summarise_all(mean)
)

# Valores promedios de variables en cada clúster con K = 5
View(
  datosT_small
    %>% group_by(cluster5)
    %>% summarise_all(mean)
)
