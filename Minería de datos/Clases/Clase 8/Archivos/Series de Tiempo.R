rm(list = ls())

library(forecast)

# Datos a utilizar
datos <- read.csv("Brocoli.csv", sep = ";", header = TRUE)
datos <- ts(datos$Precio,frequency=12,start=c(1994,3))

# Veamos los datos
plot(datos)

# Descompogamos la serie
plot(decompose(datos))

# Ajustamos automaticamente un modelo ARIMA
modelo <- auto.arima(datos)
modelo

# Residuos del modelo ARIMA
summary(residuals(modelo))

# Veamos como ajusta el modelo
plot(datos)
lines(modelo$fitted, col = 2)

# Usemos el modelo para predecir 24 meses a futuro
prediccion <- forecast(modelo, h = 24)
prediccion

plot(prediccion)
