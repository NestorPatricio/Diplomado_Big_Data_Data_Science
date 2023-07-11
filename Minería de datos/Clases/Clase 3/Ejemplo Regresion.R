setwd("C:/Users/Sebastian/Documents/R")

datos <- read.csv("DatosRegresion.csv", sep = ";", header = TRUE)

# Análisis de correlaciones
cor(datos)

# Análisis visual de dispersión
pairs(datos)

## Regresion Lineal ##

# Modelo lineal con ambas variables
modelo1 <- lm(Y1 ~ X1 + X2,
              data = datos)
summary(modelo1)

# Modelo lineal solo con X1
modelo2 <- lm(Y1 ~ X1,
              data = datos)
summary(modelo2)

# Modelo lineal solo con X2
modelo3 <- lm(Y1 ~ X2,
              data = datos)
summary(modelo3)

# Prediccion
predict(modelo2)
plot(predict(modelo2),datos$Y1)

cor(predict(modelo2),datos$Y1)

# Errores de prediccion
residuals(modelo2)


## Regresion Logistica ##

# Modelo logistico con ambas variables
modelo4 <- glm(formula = Y2 ~ X1 + X2,
              data = datos,
              family = binomial())
summary(modelo4)


# Prediccion
predict(modelo4)
predict(modelo4, type = "response")

# Matriz de confusion
pred <- as.numeric(predict(modelo4, type = "response") > 0.5)

table(as.factor(pred), as.factor(datos$Y2))

# Accuracy
sum(as.factor(pred)==as.factor(datos$Y2))/nrow(datos)



## Presentacion de la evaluacion ##

datosP <- read.csv("DatosPaises.csv", sep = ";", header = TRUE)

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
plot(datosP$RENOVABLE,datosP$PIB)
plot(datosP$RENOVABLE,log(datosP$PIB))

plot(datosP$ESCOLARIDAD)
plot(datosP$ESCOLARIDAD,datosP$PIB)
plot(datosP$ESCOLARIDAD,log(datosP$PIB))

plot(datosP$POB)
plot(datosP$POB,datosP$PIB)
plot(datosP$POB,datosP$PIB,xlim=c(0,250))
plot(datosP$POB,log(datosP$PIB),xlim=c(0,250))

plot(log(datosP$PIB))


# Probamos un modelo con algunas pocas variables
# Elegi 8 variables explicativas al azar
modeloP1 <- lm(PIB ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO
              + TURISMO + PRISION + PARLAMENTO,
             data = datosP)
summary(modeloP1)

plot(datosP$PIB , predict(modeloP1))
cor(datosP$PIB , predict(modeloP1))


# Solo a modo de ejemplo, usemos regresiones logisticas

# Regresion binomial
summary(datosP$PIB)

datosP$PIB_binomial <- as.numeric(datosP$PIB > 11)

modeloP2 <- glm(PIB_binomial ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO
               + TURISMO + PRISION + PARLAMENTO,
               data = datosP,
               family = binomial())
summary(modeloP2)

pred2 <- as.numeric(predict(modeloP2, type = "response") > 0.5)

confusionMatrix(as.factor(pred2), as.factor(datosP$PIB_binomial))


# Regresion multinomial
library(nnet)

summary(datosP$PIB)

datosP$PIB_multinomial <- 4 * ( datosP$PIB > 23 ) + 
                          3 * ( datosP$PIB > 11 ) * ( datosP$PIB <= 23 ) +
                          2 * ( datosP$PIB > 3.5 ) * ( datosP$PIB <= 11 ) +
                          1 * ( datosP$PIB <= 3.5 )

modeloP3 <- multinom(PIB_multinomial ~ POB + IDH + GENERO + SUICIDIOMAS + HOMICIDIO
                     + TURISMO + PRISION + PARLAMENTO,
                     data = datosP)
summary(modeloP3)

summary(modeloP3)$coefficients / summary(modeloP3)$standard.errors

predict(modeloP3)

confusionMatrix(predict(modeloP3), as.factor(datosP$PIB_multinomial))