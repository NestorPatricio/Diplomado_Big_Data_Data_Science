# Control 2

#Nombre1: Néstor Patricio Rojas Ríos

# SECCIÓN 1
# Carga de archivos:
cafeteria <- read.csv('cafeteria.csv')

# Importación de librerías:
library(gapminder)
library(arules)
library(tidyverse)
#library(dplyr)
#library(ggplot2)

# SECCIÓN 1
# P1) Identifique las observaciones con alto GDP e indique claramente a qué
#país(es) y año(s) corresponden (aquellas encerradas en el recuadro rojo).
respuesta11a <- n_distinct(general$id_restaurant)
print(paste(
  '1.1a) Basándose en la tabla general hay',
  respuesta11a, # 9590
  'restaurantes distintos.'
))

# P2) Mediante un gráfico de puntos, visualice una comparativa entre la relación
#de ingresos y expectativa de vida, para los años 1952 y 2007. Para ello usted
#deberá replicar el siguiente gráfico, donde el color representa a un continente
#distinto y el tamaño está dado por el total de población.

# P3) Determine el nivel de correlación de Spearman, entre las variables
#gdpPercap y lifeExp para cada uno de los años registrados. ¿En qué año se
#observa el mayor nivel de correlación entre ambas variables?

# P4) Para el año obtenido en la pregunta anterior, realice una breve descrición
#de la distribución de la expectativa de vida lifeExp.
# P4.a) ¿Cuál fue la esperanza de vida promedio considerando todos los países
#registrados?

# P4.b) ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?

# P5.a) Considerando todos los años de observación, determine mediante el test
#de shapiro, indique los dos continentes que presentan un comportamiento normal
#en la distribución del logaritmo de lifeExp.

# P5.b) Para los continentes encontrados en a), grafique los histogramas para el
#logaritmo de lifeExp. Considere añadir estimaciones de las densidades, dadas
#por geom_density así como una densidad normal con parámetros de media y
#desviación estandar igual a la media y desviación estandar muestral. ¿Qué opina
#sobre el histograma de Oceanía? Comente sobre posibles causas de su aspecto.

# P5.c) Complemente lo anterior, visualizando los qqplots para el logaritmo de
#lifeExp. Considere la utilización de las funciones qqnorm() y qqline() para el
#contraste contra una distribución normal.

# P6) onsiderando el continente de África, y asumiendo normalidad en el
#logaritmo de lifeExp. Independiente del año ¿cuál es la probabilidad de que la
#expectativa de vida (lifeExp) sea superior a 54 años?

print('-----------------------------------------------------------------------')

# SECCIÓN 2
# P1) Genere tres nuevas columnas, que contengan la hora, minutos y segundos de
#la transacción registrada.

# P2) Genere una tabla resumen que contenga la siguiente información:
# hora: hora donde se registraron las transacciones. Por ejemplo, el valor 09
#indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
# total_trx: total de transacciones distintas generadas en el bloque horario
#respectivo.
# total_items: total de items vendidos en el bloque horario respectivo.
# total_items_unicos: total de items únicos venidos en el bloque horario
#respectivo.

# P3) Con base en la tabla anterior, diremos que una hora pertenece al horario
#punta si la cantidad de transacciones distintas generadas en dicho bloque
#supera las 1000 transacciones.
# P3.a) ¿Qué horas comprende el horario punta?

# P3.b) En promedio ¿cuántas transacciones distintas por hora se dieron en
#horario punta? ¿Y en horario no punta?

# P4) Se sabe que el total de personal disponible es capaz de atender como
#máximo, 1300 transacciones por hora, de modo que no se "sature" el sistema y
#que los tiempos de espera de los clientes sean razonables. Asumiendo que la
#cantidad de transacciones por hora tiene una distribución Poisson con parámetro 
#igual al estimado en la pregunta 3.b (hora punta):
# P4.a) ¿Cuál es la probabilidad de que en horario punta se den más de 1300
#transacciones en una hora? ¿Cómo interpretaría este valor? Comente.

# P4.b) Con el objetivo de reducir costos, se propone limitar el personal
#disponible a modo de poder atender como máximo 1.250 transacciones por hora
#¿que tan probable es que se supere este máximo de transacciones por hora?
#¿Recomendaría usted esta medida?

# P4.c) Usted sugiere modificar la cantidad de personal pero teniendo en cuenta
#de que se garantice la atención de al menos un 95% de las transacciones por
#hora ¿cuántas transacciones por hora se deberían poder gestionar en este
#escenario?

# P5) ¿Cuáles son los 5 items más vendidos? Ilustre mediante un gráfico de
#barras o una tabla.

# P6) ¿Cambian estos 5 ítems según el horario de atención? Para ello muestre los
#5 items más vendidos en los siguientes horarios:
# 07:00-11:59
# 12:00-16:59
# 17:00-23:59

# P7) Considerando un support mínimo de 0.02, un confidence mínimo de 0.1 y
#teniendo en cuenta que no se deben considerar reglas de asociación cuyo
#antecedente o consecuente sean vacíos:
# P7.a) ¿Cuál es la regla de asociación más frecuente en cada uno de los
#horarios indicados en P6)?

# P7.b) ¿Cuál es la regla de asociación con mayor confidence en cada uno de los
#horarios indicados en P6)?

# P7.c) ¿Cuál es la regla de asociación con mayor lift en cada uno de los
#horarios indicados en P6)?

# P8) Se quiere potenciar un segundo producto por la compra de un café en los
#tres horarios definidos previamente en P6:

# P8.a) Genere tres listados (uno por cada rango horario) con todas las reglas
#que contengan el producto Coffee en el antecedente.

# P8.b) ¿Qué promoción recomendaría en cada horario por la compra de un café?
#Justifique su respuesta basándose en los indicadores support, confidence y
#lift.
