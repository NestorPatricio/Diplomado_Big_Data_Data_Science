rm(list = ls())

setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Clases\\Clase 8\\Archivos"
))

library(tm)
library(rpart)
library(rpart.plot)

# Datos de comentarios de películas
comentarios <- read.csv(file = "Sentiment.csv", header = TRUE, sep = ";")
View(comentarios)

# Creamos una colección de documentos
coleccion <- Corpus(VectorSource(comentarios$Phrase))

# Pasamos todo a minúsculas
coleccion <- tm_map(coleccion, tolower)

# Eliminamos la puntuación
coleccion <- tm_map(coleccion, removePunctuation)

# Eliminamos conectores, artículos, preposiciones, etc.
coleccion <- tm_map(coleccion, removeWords, stopwords("english"))

# Estas son las palabras consideradas "vacías" en inglés
stopwords("english")

# Creamos una matriz de términos
matriz <- DocumentTermMatrix(coleccion)
matriz
inspect(matriz[1:10, 1:10])

# Términos frecuentes
# se eliminan todos los términos que aparezcan menos de 100 veces
frecuentes <- findFreqTerms(matriz, lowfreq = 100)
frecuentes

# Nos quedamos solo con los términos que aparecen en 0,5% de los comentarios
matriz_frec <- removeSparseTerms(matriz, 0.995)
matriz_frec

# Construimos el data frame
datos <- as.data.frame(as.matrix(matriz_frec))

datos$Sentiment <- comentarios$Sentiment

table(datos$Sentiment)


# Arbol de decision
modelo <- rpart(
  Sentiment ~ .,
  data = datos,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(
    minsplit = 100,
    minbucket = 40,
    maxdepth = 10,
    cp = 0
  )
)

rpart.plot(
  modelo,
  extra = 0,
  type = 0,
  cex = 0.7
)
