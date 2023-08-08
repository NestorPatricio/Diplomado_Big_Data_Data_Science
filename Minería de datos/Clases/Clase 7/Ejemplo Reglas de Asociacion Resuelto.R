# Paquetes que utilizaremos
# install.packages("arules")
# install.packages("arulesViz")

library(arules)
library(arulesViz)

# La lista de transacciones del ejemplo
lista_trans <- list(
  c("A","B","F"),
  c("B","D"),
  c("B","C"),
  c("A","B","D"),
  c("A","C","F"),
  c("B","C","E"),
  c("A","C"),
  c("A","B","D","F"),
  c("A","B","C")
)

# Forzamos la lista a la clase "transactions"
trans <- as(object = lista_trans, Clas = "transactions")

# Analizamos las transacciones
summary(trans)
image(x = trans, ylab = "Transacciones")

# Algoritmo Apriori
resultados <- apriori(
  data = trans,
  parameter = list(
    supp = 2/9, # El soporte mínimo (0.1 por defecto)
    conf = 2/3, # La confianza mínima (0.8 por defecto)
    minlen = 2, # Largo mínimo de los itemsets (sin valor por defecto)
    maxlen = 5, # Largo máximo de los itemsets (10 por defecto)
    maxtime = 5 # Tiempo máximo para buscar subconjuntos (5 por defecto)
  )
)
# El tamaño del itemset incluye "antecedentes" + "consecuentes".

# Parámetros del algoritmo:
# "supp" es el soporte mínimo
# "conf" es la confianza mínima
# "minlen" es el tamano mínimo de itemsets
# "maxlen" es el tamano máximo de itemsets
# "maxtime" tiempo limite (en segundos) para buscar subconjuntos


# Analizamos los resultados
summary(resultados)
inspect(x = resultados)

# Algunas representaciones gráficas
plot(resultados, method = "paracoord")
plot(resultados, method = "graph")


## Presentación de la evaluación ##

# Datos de listas de reproducción musical
setwd(paste0(
"/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos/",
"Clases/Clase 7/"
))

datos <- read.csv(
  file = "../../Evaluaciones/Evaluación 3b/lastfm.csv",
  sep = ";",
  header = TRUE
)

# Generamos la lista de transacciones a utilizar
write.table(
  x = datos,
  file = tmp <- file(),
  row.names = FALSE
)

listas <- read.transactions(
  file = tmp,
  format = "single",
  header = TRUE,
  cols = c("user", "artist")
)
close(tmp)
summary(listas)

# Recomendación: no utilizar el comando image(listas) pues son demasiadas
resultados2 <- apriori(
  data = listas,
  parameter = list(
    supp = 0.01,
    conf = 0.5,
    minlen = 2,
    maxlen = 10,
    maxtime = 5
  )
)

# Analizamos los resultados
summary(resultados2)
inspect(x = resultados2)

# Algunas representaciones gráficas
plot(resultados2, method = "paracoord")
plot(resultados2, method = "graph")


# Analisis ordenado por criterios
reglas_conf <- sort(
  x = resultados2,
  by = "confidence",
  decreasing = TRUE
)
inspect(x = head(reglas_conf))

reglas_lift <- sort(
  x = resultados2,
  by = "lift",
  decreasing=TRUE
)
inspect(x = head(reglas_lift))
