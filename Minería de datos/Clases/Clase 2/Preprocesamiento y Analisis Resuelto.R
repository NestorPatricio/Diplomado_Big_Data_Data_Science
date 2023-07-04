# Indicar el directorio de trabajo: Dirección donde se encuentra la carpeta
setwd(
  paste0(
    '/Users/nrojas/Documents/Diplomado_Big_Data_Data_Science/Minería de datos',
    '/Clases/Clase 2/'
  )
)

# Cargar la base de datos: Indicar el nombre del archivo, el tipo de separación
#de columnas
datos <- read.csv("DatosTrafico.csv", sep=";",header=TRUE)

# El dataframe da información del tráfico y las ventas que se generan en una
#página web, junto con el día de la semana y un valor id.

# Head para visualizar las primeras 6 filas de la base de datos
head(datos)

# Summary para generar un resumen estadístico de la base de datos
summary(datos)

# Promedio de una variable 
mean(datos$Trafico)

# Mediana de una variable 
median(datos$Trafico)

# Desviación estándar y varianza de una variable 
sd(datos$Trafico)
var(datos$Trafico)

# Histogramas para los datos de una variable de la base
hist(datos$Trafico)
hist(datos$Trafico, xlim=c(0,20))

# Promedio de tráfico en cada día de la semana
tapply(datos$Trafico, datos$Dia, mean)

# Correlación entre dos variables 
cor(datos$Trafico, datos$Ventas)

# Gráfico de dispersión entre dos variables
plot(datos$Trafico, datos$Ventas)

# Correlación con variable categórica (no funciona, da error)
cor(datos$Trafico, datos$Dia)

# Se crea una nueva columna de variable binaria para identificar
#si es un día de fin de semana
datos$FinDeSemana <- (datos$Dia == "Sabado") + (datos$Dia == "Domingo")

# Se crea una nueva columna de variable binaria para identificar si es un dia
#laboral
datos$Laboral <- 1 - datos$FinDeSemana

# Correlación entre el trafico y si es un día laboral
cor(datos$Trafico, datos$Laboral)

# Promedio de trafico en dia laboral (1) y en no laboral (0)
tapply(datos$Trafico, datos$Laboral, mean)


# Aplicacion del Valor-Z Robusto #

# Nueva columna que contiene la diferencia absoluta entre el tráfico y
#la mediana de tráfico en la base de datos
datos$Error <- abs(datos$Trafico - median(datos$Trafico))

# Nueva columna que registra el Valor-Z como la razón entre el error y
#el promedio del error en la base de datos
datos$ValorZ <- datos$Error/median(datos$Error)

# Observaciones que serán candidatas a ser outiliers, considerando
#un Valor-Z de 4.5 como umbral
datos[which(datos$ValorZ >= 4.5),names(datos)]

# Se crea una nueva base de datos a partir de las observaciones que
#tienen un Valor-Z menor a 5.2 y se mantienen los nombres de las columnas
datos2 <- datos[which(datos$ValorZ < 5.2), names(datos)]

# Resumen estadístico de la nueva base de datos
summary(datos2)

# Correlación
cor(datos2$Trafico,datos2$Ventas)

# Gráfico de dispersión entre dos variables
plot(datos2$Trafico,datos2$Ventas)

# Histograma de una variable
hist(datos2$Trafico)


## Presentación de la evaluación ##

datosP <- read.csv("DatosPaises.csv", sep = ";", header = TRUE)

head(datosP)
summary(datosP)

# An?lisis de correlaciones
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