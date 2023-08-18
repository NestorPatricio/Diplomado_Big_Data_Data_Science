# Se borran todas las variables
rm(list = ls())

setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Clases\\Clase 8\\Archivos"
))

# Cargamos la base de datos
datos <- read.csv(file = "DatosPaises.csv", sep= ";", header = TRUE)

# Nos quedamos con los datos numéricos
# (para fectos del ejemplo, usaremos las 29 variables)
datosN <- subset(datos, select = -PAIS )

# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datosN, normalize))

# Obtenemos la matriz de distancias
distancias <- dist(datos_norm, method = 'euclidean')

# Aplicamos clustering jer?rquico
hclust <- hclust(distancias, method = 'average')

# Dendograma resultante
plot(hclust, labels = datos$PAIS, cex = 0.4)


# Para efectos visuales, filtremos los datos
sudamerica <- datos[
  which(datos$PAIS %in% c(
    "Argentina", "Bolivia", "Brazil",
    "Chile", "Colombia", "Ecuador",
    "Guyana", "Paraguay", "Peru",
    "Suriname", "Uruguay", "Venezuela"
  )),
  names(datos)
]

# Nos quedamos con los datos num?ricos
sudamericaN <- subset(sudamerica, select = -PAIS )

# Normalizamos los datos
sudamerica_norm <- as.data.frame(lapply(sudamericaN, normalize))

# Obtenemos la matriz de distancias
distanciasSud <- dist(sudamerica_norm, method = 'euclidean')

# Aplicamos clustering jer?rquico
hclust_sud <- hclust(distanciasSud, method = 'average')

# Dendograma resultante
plot(hclust_sud,labels = sudamerica$PAIS)

# Supongamos que queremos 4 clusters
cutree(hclust_sud, k = 4)
sudamerica$PAIS

