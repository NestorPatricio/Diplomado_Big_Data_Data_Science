# Paquetes que utilizaremos
install.packages("arules")
install.packages("arulesViz")

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

# Forzamos la lsita a la clase "transactions"
trans <- as(lista_trans, "transactions")

# Analizamos las transacciones
summary(trans)
image(trans)

# Algoritmo Apriori
resultados <- apriori(trans,
                   parameter = list(supp = 2/9,
                                    conf = 2/3,
                                    minlen = 1,
                                    maxlen = 5,
                                    maxtime = 5))

# Parametros del algoritmo:
# "supp" es el soporte mínimo
# "conf" es la confianza mínima
# "minlen" es el tamano mínimo de itemsets
# "maxlen" es el tamano maximo de itemsets
# "maxtime" tiempo limite (en segundos) para buscar subconjuntos


# Analizamos los resultados
summary(resultados)
inspect(resultados)

# Algunas representaciones graficas
plot(resultados, method = "paracoord")
plot(resultados, method = "graph")



## Presentacion de la evaluacion ##

# Datos de listas de reproduccion musical

setwd("C:/Users/Sebastian/Documents/R")

datos <- read.csv("lastfm.csv", sep=";",header=TRUE)


# Generamos la lista de transacciones a utilizar
write.table(datos, file = tmp <- file(), row.names = FALSE)
listas <- read.transactions(tmp,
                            format = "single",
                            header = TRUE,
                            cols = c("user", "artist"))
close(tmp)
summary(listas)

# Recomendacion: no utilizar el comando image(listas) pues son demasiadas

resultados2 <- apriori(listas,
                       parameter = list(supp = 0.01,
                                        conf = 0.5,
                                        minlen = 2,
                                        maxlen = 10,
                                        maxtime = 5))


# Analizamos los resultados
summary(resultados2)
inspect(resultados2)


# Algunas representaciones graficas
plot(resultados2, method = "paracoord")
plot(resultados2, method = "graph")


# Analisis ordenado por criterios
reglas_conf <- sort(resultados2,
                    by = "confidence",
                    decreasing = TRUE)
inspect(head(reglas_conf))


reglas_lift <- sort (resultados2,
                     by = "lift",
                     decreasing=TRUE)
inspect(head(reglas_lift))
