# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
#install.packages("rpart")
#install.packages("rpart.plot")


# Cargamos las librerías que utilizaremos
library(rpart)
library(rpart.plot)

# Indicamos el directorio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Minería de datos/Clases/Clase 4'
))

# Cargamos la base de datos de entrenamiento
datos <- read.csv("Datos Tenis.csv", sep=";",header=TRUE)


# Árbol de clasificación
modelo1 <- rpart(
  Jugar ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    maxdepth = 5,
    cp = 0
  )
)

# Especificamos la variable endogena (Jugar) como función de las
# las variables exogenas (Clima, Temperatura, Humedad y Viento)

# Metodos:
# "class" para arboles de clasificacion
# "anova" para arboles de regresión

# Parámetro de división para árboles de clasificación (split):
# "information" para usar ganancia de información (entropía)
# "gini" para usar el índice de impuridad de Gini

# Parámetros opcionales de control:
# minsplit determina la cantidad mínima de observaciones para intentar dividir un nodo
# minbucket determina la cantidad mínima de observaciones a tener en un nodo terminal
# maxdepth determina la profundidad máxima del árbol (el nodo raíz cuenta como 0)
# cp es una versión escalada del parámetro lambda asociado al costo de complejidad

# Árbol resultante
print(modelo1)

# Graficamos el árbol
rpart.plot(modelo1)


# Probemos otras combinaciones de parametros de control:
modelo2 <- rpart(
  Jugar ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    maxdepth = 3,
    cp = 0.1334
  )
)
rpart.plot(modelo2)


# Tabla de cp muestra la mejora en costo de complejidad en cada nodo
printcp(modelo1)


# Cambiemos el criterio de división a Gini:
modelo3 <- rpart(
  Jugar ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    maxdepth = 5,
    cp = 0
  )
)
rpart.plot(modelo3)

# Opciones adicionales para la representacion visual del arbol (rpart.plot):
# 0 - Sin información adicional
# 1 - Mostrar cantidad de observaciones para cada uno de los valores de la variable a evaluar por nodo
# 2 - Tasa de clasificación del nodo (correctos/total)
# 3 - Tasa de error de clasificación del nodo (incorrectos/total)
# 4 - Probabilidad de clasificación del nodo, condicionada al nodo
# 5 - Igual que 4, pero sin mostrar la clase ajustada
# 6 - Probabilidad de la segunda clase únicamente
# 7 - Igual que 6, pero sin mostrar la clase ajustada
# 8 - Probabilidad de la clase ajustada
# 9 - Probabilidad de clasificación del nodo, relativa a todas las observaciones
# 10 - Igual que 9, pero con la probababilidad de la segunda clase
# 11 - Igual que 10, pero sin mostrar la clase ajustada
# +100 - Muestra el porcentaje de observaviones del nodo
# Por defecto muestra extra = 106


rpart.plot(modelo3, extra = 6)


# Probemos arboles de regresion
modelo4 <- rpart(
  Jugadores ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "anova",
  control = rpart.control(
    minsplit = 1,
    minbucket = 1,
    maxdepth = 5,
    cp = 0
  )
)
rpart.plot(modelo4)
printcp(modelo4)

modelo5 <- rpart(
  Jugadores ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "anova",
  control = rpart.control(
    minsplit = 1,
    minbucket = 1,
    maxdepth = 5,
    cp = 0.03
  )
)
rpart.plot(modelo5)


modelo6 <- rpart(
  Jugadores ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "anova",
  control = rpart.control(
    minsplit = 1,
    minbucket = 1,
    maxdepth = 5,
    cp = 0.18
  )
)
rpart.plot(modelo6)


# Usemos los modelos para predecir
# No tenemos una base de datos de validacion distinta a la de entrenamiento
# Así que usaremos los mismos datos para entrenar y validar (no recomendado en la practica)

# Modelo discreto (informacion)
datos$prediccionB <- predict(modelo1, datos)


# "Empeoremos" un poco el modelo
modelo7 <- rpart(
  Jugar ~ Clima + Temperatura + Humedad + Viento,
  data = datos,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(
    minsplit = 1,
    minbucket = 1,
    maxdepth = 2,
    cp = 0
  )
)
rpart.plot(modelo7, extra=102)
# Entrega la proporción de cada categoría
datos$prediccionB <- predict(modelo7, datos)
# Entrega la clase categórica
datos$prediccionB <- predict(modelo7, datos, type = "class")

# Matriz de confusión
table(datos$Jugar, datos$prediccionB)

# Modelo continuo (anova)
datos$prediccionC <- predict(modelo6, datos)

plot(datos$Jugadores, datos$prediccionC)
cor(datos$Jugadores, datos$prediccionC)


## Presentación de la evaluación ##

datosBanco <- read.csv(
  paste0(
    '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
    'Minería de datos/Evaluaciones/Evaluación 2/SouthGermanCredit.csv'
  ),
  sep = ';',
  header = TRUE
)

# Dividimos los datos en entrenamiento (75%) y validacion (25%)
set.seed(123)
subset <- sample(1:nrow(datosBanco), size = 0.75 * nrow(datosBanco), replace = FALSE)
datos_train <- datosBanco[subset,]
datos_test <- datosBanco[-subset,]

# Arbol por defecto
modeloB <- rpart(
  credit ~ .,
  data = datos_train,
  method = "class",
  parms = list(split = "information")
)

rpart.plot(modeloB, extra = 0, cex = 0.6)

# CP
printcp(modeloB)


# Predicción dentro de la base de entrenamiento
predictB_in <- predict(modeloB, type = "class")

# Matriz de Confusión dentro de la base de entrenamiento
table(datos_train$credit, predictB_in)

# Accuracy dentro de la base de entrenamiento
sum(datos_train$credit == predictB_in)/nrow(datos_train)


# Predicción en la base de validación
predictB_out <- predict(
  modeloB,
  newdata = datos_test,
  type = "class"
)

# Matriz de Confusión en la base de validación
table(datos_test$credit, predictB_out)

# Accuracy en la base de validación
sum(datos_test$credit == predictB_out)/nrow(datos_test)



# Árbol con parametros de control
modeloB2 <- rpart(
  credit ~ .,
  data = datos_train,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(
    minsplit = 30,
    minbucket = 10,
    maxdepth = 4,
    cp = 0
  )
)

rpart.plot(modeloB2, extra = 0, cex=0.6)


# Predicción dentro de la base de entrenamiento
predictB2_in <- predict(modeloB2, type = "class")

# Matriz de Confusión dentro de la base de entrenamiento
table(datos_train$credit, predictB2_in)

# Accuracy dentro de la base de entrenamiento
sum(datos_train$credit == predictB2_in)/nrow(datos_train)


# Predicción en la base de validación
predictB2_out <- predict(
  modeloB2,
  newdata = datos_test,
  type = "class"
)

# Matriz de Confusión en la base de validación
table(datos_test$credit, predictB2_out)

# Accuracy en la base de validación
sum(datos_test$credit == predictB2_out)/nrow(datos_test)
