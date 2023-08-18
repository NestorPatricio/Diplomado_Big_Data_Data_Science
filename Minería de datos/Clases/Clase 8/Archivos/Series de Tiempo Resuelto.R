rm(list = ls())

setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Clases\\Clase 8\\Archivos"
))

library(forecast)

# Datos a utilizar
datos <- read.csv(file = "Brocoli.csv", sep = ";", header = TRUE)
View(datos)
# Se transforma a serie de tiempo
datos <- ts(data = datos$Precio, frequency = 12, start = c(1994, 3))

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
