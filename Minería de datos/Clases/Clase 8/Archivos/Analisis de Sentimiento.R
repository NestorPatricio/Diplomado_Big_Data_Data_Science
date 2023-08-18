rm(list = ls())

library(tm)
library(rpart)
library(rpart.plot)

setwd("C:/Users/Sebastian/Documents/R")

# Datos de comentarios de peliculas
comentarios <- read.csv("Sentiment.csv", header = TRUE, sep = ";")

# Creamos una coleccion de documentos
coleccion <- Corpus(VectorSource(comentarios$Phrase))

# Pasamos todo a minusculas
coleccion <- tm_map(coleccion, tolower)

# Eliminamos la puntuacion
coleccion <- tm_map(coleccion, removePunctuation)

# Eliminamos conectores, articulos, preposiciones, etc.
coleccion <- tm_map(coleccion, removeWords, stopwords("english"))

stopwords("english")

# Creamos una matriz de terminos
matriz <- DocumentTermMatrix(coleccion)
matriz
inspect(matriz[1:10, 1:10])

# Terminos frecuentes
frecuentes <- findFreqTerms(matriz, lowfreq = 100)
frecuentes

# Nos quedamos solo con los terminos que aparecen en 0,5% de los comentarios
matriz_frec <- removeSparseTerms(matriz, 0.995)
matriz_frec

# Construimos el data frame
datos <- as.data.frame(as.matrix(matriz_frec))

datos$Sentiment <- comentarios$Sentiment

table(datos$Sentiment)


# Arbol de decision
modelo <- rpart(Sentiment ~ .,
                    data = datos,
                    method = "class",
                    parms = list(split = "information"),
                    control = rpart.control(minsplit = 100, minbucket = 40, maxdepth = 10, cp=0))

rpart.plot(modelo,
           extra = 0,
           type = 0,
           cex = 0.7)
