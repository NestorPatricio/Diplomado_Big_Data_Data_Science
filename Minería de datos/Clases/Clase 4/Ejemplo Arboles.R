# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
install.packages("rpart")
install.packages("rpart.plot")


# Cargamos las librerias que utilizaremos
library(rpart)
library(rpart.plot)

# Indicamos el directorio de trabajo
setwd("C:/Users/Sebastian/Documents/R")


# Cargamos la base de datos de entrenamiento
datos <- read.csv("Datos Tenis.csv", sep=";",header=TRUE)


# Árbol de clasificación
modelo1 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

# Especificamos la variable endogena (Jugar) como función de las
# las variables exogenas (Clima, Temperatura, Humedad y Viento)

# Metodos:
# "class" para arboles de clasificacion
# "anova" para arboles de regresión

# Parámetro de division para arboles de clasificacion:
# "information" para usar ganancia de informacion (entropia)
# "gini" para usar el indice de impuridad de Gini

# Parametros opcionales de control:
# minsplit determina la cantidad minima de observaciones para intentar dividir un nodo
# minbucket determina la cantidad minima de observaciones a tener en un nodo terminal
# maxdepth determina la profundidad maxima del arbol (el nodo raiz cuenta como 0)
# cp es una version escalada del parametro lambda asociado al costo de complejidad


# Arbol resultante
print(modelo1)

# Graficamos el arbol
rpart.plot(modelo1)


# Probemos otras combinaciones de parametros de control:
modelo2 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))
rpart.plot(modelo2)


# Tabla de cp muestra la mejora en costo de complejidad en cada nodo
printcp(modelo1)


# Cambiemos el criterio de division:
modelo3 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

rpart.plot(modelo3)



# Opciones adicionales para la representacion visual del arbol:
# 0 - Sin información adicional
# 1 - Mostrar cantidad de observaciones por nodo
# 2 - Tasa de clasificación del nodo (correctos/total)
# 3 - Tasa de error de clasificación del nodo (incorrectos/tota)
# 4 - Probabilidad de clasificación del nodo, condicionada al nodo
# 5 - Igual que 4, pero sin mostrar la clase ajustada
# 6 - Probabilidad de la segunda clase únicamente
# 7 - Igual que 6, pero sin mostrar la clase ajustada
# 8 - Probabilidad de la clase ajustada
# 9 - Probabilidad de clasificación del nodo, relativa a todas las observaciones
# 10 - Igual que 9, pero con la probababilidad de la segunda clase
# 11 - Igual que 10, pero sin mostrar la clase ajustada
# +100 - Muestra el porcentaje de observaviones del nodo

rpart.plot(modelo3, extra = 0)



# Probemos arboles de regresion
modelo4 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))
rpart.plot(modelo4)


modelo5 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.03))
rpart.plot(modelo5)


modelo6 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.18))
rpart.plot(modelo6)


# Usemos los modelos para predecir
# No tenemos una base de datos de validacion distinta a la de entrenamiento
# Asi que usaremos los mismos datos para entrenar y validar (no recomendado en la practica)

# Modelo discreto (informacion)
datos$prediccionB <- predict(modelo1, datos)


# "Empeoremos" un poco el modelo
modelo7 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 2,
                                         cp = 0))

rpart.plot(modelo7, extra=102)

datos$prediccionB <- predict(modelo7, datos)

datos$prediccionB <- predict(modelo7, datos, type = "class")

# Matriz de confusion
table(datos$Jugar,datos$prediccionB)



# Modelo continuo (anova)
datos$prediccionC <- predict(modelo6, datos)

plot(datos$Jugadores,datos$prediccionC)
cor(datos$Jugadores,datos$prediccionC)




## Presentacion de la evaluacion ##

datosBanco <- read.csv("SouthGermanCredit.csv", sep = ";", header = TRUE)


# Dividimos los datos en entrenamiento (75%) y validacion (25%)
set.seed(123)
subset <- sample(1:nrow(datosBanco), size = 0.75*nrow(datosBanco), replace = FALSE)
datos_train <- datosBanco[subset,]
datos_test <- datosBanco[-subset,]

# Arbol por defecto
modeloB <- rpart(credit ~ .,
                data = datos_train,
                method = "class",
                parms = list(split = "information"))

rpart.plot(modeloB, extra = 0, cex=0.6)

# CP
printcp(modeloB)


# Prediccion dentro de la base de entrenamiento
predictB_in <- predict(modeloB,
                    type = "class")

# Matriz de Confusion dentro de la base de entrenamiento
table(datos_train$credit, predictB_in)

# Accuracy dentro de la base de entrenamiento
sum(datos_train$credit == predictB_in)/nrow(datos_train)


# Prediccion en la base de validacion
predictB_out <- predict(modeloB,
                        newdata = datos_test,
                        type = "class")

# Matriz de Confusion en la base de validacion
table(datos_test$credit, predictB_out)

# Accuracy en la base de validacion
sum(datos_test$credit == predictB_out)/nrow(datos_test)



# Arbol con parametros de control
modeloB2 <- rpart(credit ~ .,
                 data = datos_train,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 30,
                                         minbucket = 10,
                                         maxdepth = 4,
                                         cp = 0))

rpart.plot(modeloB2, extra = 0, cex=0.6)


# Prediccion dentro de la base de entrenamiento
predictB2_in <- predict(modeloB2,
                       type = "class")

# Matriz de Confusion dentro de la base de entrenamiento
table(datos_train$credit, predictB2_in)

# Accuracy dentro de la base de entrenamiento
sum(datos_train$credit == predictB2_in)/nrow(datos_train)


# Prediccion en la base de validacion
predictB2_out <- predict(modeloB2,
                        newdata = datos_test,
                        type = "class")

# Matriz de Confusion en la base de validacion
table(datos_test$credit, predictB2_out)

# Accuracy en la base de validacion
sum(datos_test$credit == predictB2_out)/nrow(datos_test)
