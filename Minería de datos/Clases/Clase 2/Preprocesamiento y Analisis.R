#Indicar el directorio de trabajo: Direccion donde se encuentra la carpeta
setwd(
  paste0(
    '/Users/nrojas/Documents/Diplomado_Big_Data_Data_Science/Minería de datos',
    '/Clases/Clase 2/'
  )
)

#Cargar la base de datos: Indicar el nombre del archivo, el tipo de separacion de columnas
datos <- read.csv("DatosTrafico.csv", sep=";",header=TRUE)

#Head para visualizar las primeras 6 filas de la base de datos
head(datos)

#Summary para generar un resumen estadistico de la base de datos
summary(datos)

#Promedio de una variable 
mean(datos$Trafico)

#Mediana de una variable 
median(datos$Trafico)

#Desviacion estandar y varianza de una variable 
sd(datos$Trafico)
var(datos$Trafico)

#Histogramas para los datos de una variable de la base
hist(datos$Trafico)
hist(datos$Trafico,xlim=c(0,20))

#Promedio de trafico en cada dia de la semana
tapply(datos$Trafico, datos$Dia, mean)

#Correlacion entre dos variables 
cor(datos$Trafico,datos$Ventas)

#Grafico de dispersion entre dos variables
plot(datos$Trafico,datos$Ventas)

#Correlacion con variablee categorica (no funciona)
cor(datos$Trafico,datos$Dia)

#Se crea una nueva columna de variable binaria para identificar
#si es un dia de fin de semana
datos$FinDeSemana <- (datos$Dia == "Sabado") + (datos$Dia == "Domingo")

#Se crea una nueva columna de variable binaria para identificar si es un dia laboral
datos$Laboral <- 1 - datos$FinDeSemana

#Correlacion entre el trafico y si es un dia laboral
cor(datos$Trafico,datos$Laboral)

#Promedio de trafico en dia laboral (1) y en no laboral (0)
tapply(datos$Trafico, datos$Laboral, mean)


# Aplicacion del Valor-Z Robusto #

#Nueva columna que contiene la diferencia absoluta entre el trafico y
#la mediana de trafico en la base de datos
datos$Error <- abs(datos$Trafico-median(datos$Trafico))

#Nueva columna que registra el Valor-Z como la razon entre el error y
#el promedio del error en la base de datos
datos$ValorZ <- datos$Error/median(datos$Error)

#Observaciones que ser?an candidatas a ser outiliers, considerando
#un Valor-Z de 4.5 como umbral
datos[which(datos$ValorZ >= 4.5),names(datos)]

#Se crea una nueva base de datos a partir de las observaciones que
#tienen un Valor-Z menor a 5.2 y se mantienen los nombres de las columnas
datos2 <- datos[which(datos$ValorZ < 5.2),names(datos)]

#Resumen estadistico de la nueva base de datos
summary(datos2)

#Correlacion
cor(datos2$Trafico,datos2$Ventas)

#Grafico de dispersion entre dos variables
plot(datos2$Trafico,datos2$Ventas)

#Histograma de una variable
hist(datos2$Trafico)


## Presentacion de la evaluacion ##

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