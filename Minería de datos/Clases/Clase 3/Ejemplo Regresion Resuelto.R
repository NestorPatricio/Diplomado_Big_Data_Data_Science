setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Clases/Clase 3'
))
datos <- read.csv('DatosRegresion.csv', sep = ';', header = TRUE)
summary(datos)

# Análisis de correlaciones
cor(datos)

# Análisis visual de dispersión
pairs(datos)

############### Regresión Lineal ###############

# Modelo lineal con ambas variables
modelo1 <- lm(
  Y1 ~ X1 + X2,
  data = datos
)
summary(modelo1)

# Modelo lineal sólo con X1
modelo2 <- lm(
  Y1 ~ X1,
  data = datos
)
summary(modelo2)

# Modelo lineal sólo con X2
modelo3 <- lm(
  Y1 ~ X2,
  data = datos
)
summary(modelo3)

# Predicción
predict(modelo2)
plot(predict(modelo2), datos$Y1)

cor(predict(modelo2), datos$Y1)

# Errores de predicción
residuals(modelo2)


############### Regresion Logistica ###############

# Modelo logístico con ambas variables
modelo4 <- glm(
  formula = Y2 ~ X1 + X2,
  data = datos,
  family = binomial()
)
summary(modelo4)


# Predicción
predict(modelo4) # Entrega la función de respuesta, Ri
predict(modelo4, type = "response") # Entrega la probabilidad

# Matriz de confusión
pred <- as.numeric(predict(modelo4, type = "response") > 0.5)
table(as.factor(pred), as.factor(datos$Y2)) # Primero filas, luego columnas

# Accuracy
sum(as.factor(pred)==as.factor(datos$Y2))/nrow(datos)


############### Presentación de la evaluación ###############

datosP <- read.csv(
  '../../Evaluaciones/Evaluación 1/DatosPaises.csv',
  sep = ';',
  header = TRUE
)

head(datosP)
summary(datosP)

# Análisis de correlaciones
cor(datosP) # No funciona, porque tenemos una columna con nombres de paises
cor(datosP[c(2:30)]) # Matriz de 29x29
cor(datosP$PIB,datosP[c(3:30)]) # Vector con 28 correlaciones


# Analicemos algunas variables
# Los datos vienen ordenados por PIB per capita
plot(datosP$PIB)

plot(datosP$RENOVABLE)
plot(datosP$RENOVABLE, datosP$PIB)
plot(datosP$RENOVABLE, log(datosP$PIB))

plot(datosP$ESCOLARIDAD)
plot(datosP$ESCOLARIDAD, datosP$PIB)
plot(datosP$ESCOLARIDAD, log(datosP$PIB))

plot(datosP$POB)
plot(datosP$POB, datosP$PIB)
plot(datosP$POB, datosP$PIB, xlim=c(0,250))
plot(datosP$POB, log(datosP$PIB), xlim=c(0,250))

plot(log(datosP$PIB))


# Probamos un modelo con algunas pocas variables
# Elegí 8 variables explicativas al azar
modeloP1 <- lm(
  PIB ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO + TURISMO + PRISION +
    PARLAMENTO,
  data = datosP
)
summary(modeloP1)

plot(datosP$PIB , predict(modeloP1))
cor(datosP$PIB , predict(modeloP1))

# ¿Qué sucede si usamos el logaritmo del PIB?
modeloPl1 <- lm(
  log(PIB) ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO + TURISMO + PRISION +
    PARLAMENTO,
  data = datosP
)
summary(modeloPl1)

plot(datosP$PIB, exp(predict(modeloPl1))) # Con exp se quita el efecto de log
cor(datosP$PIB, exp(predict(modeloPl1)))


# Solo a modo de ejemplo, usemos regresiones logísticas

# Regresión binomial
summary(datosP$PIB)

datosP$PIB_binomial <- as.numeric(datosP$PIB > 11)

modeloP2 <- glm(
  PIB_binomial ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO + TURISMO + 
    PRISION + PARLAMENTO,
  data = datosP,
  family = binomial()
)
summary(modeloP2)

pred2 <- as.numeric(predict(modeloP2, type = "response") > 0.5)
confusionMatrix(as.factor(pred2), as.factor(datosP$PIB_binomial))

# Regresión multinomial
install.packages('caret')
library(nnet)
library(caret)
summary(datosP$PIB)

datosP$PIB_multinomial <- 4 * ( datosP$PIB > 23 ) + 
  3 * ( datosP$PIB > 11 ) * ( datosP$PIB <= 23 ) +
  2 * ( datosP$PIB > 3.5 ) * ( datosP$PIB <= 11 ) +
  1 * ( datosP$PIB <= 3.5 )

modeloP3 <- multinom(
  PIB_multinomial ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO + TURISMO +
    PRISION + PARLAMENTO,
  data = datosP
)
summary(modeloP3)

summary(modeloP3)$coefficients / summary(modeloP3)$standard.errors

predict(modeloP3)

confusionMatrix(predict(modeloP3), as.factor(datosP$PIB_multinomial))
