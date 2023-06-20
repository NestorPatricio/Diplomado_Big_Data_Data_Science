## INSTALACIÓN DE PAQUETE GAPMINDER

install.packages("gapminder")

library(gapminder)

data("gapminder")

head(gapminder)

# LIBRERIAS A UTILIZAR

library(dplyr)

library(ggplot2)

library(arules)

install.packages("tidyverse")

library(tidyverse)

#**************************** SECCIÓN 1 ****************************

# Pregunta 1. 
# En el gráfico se pueden apreciar observaciones con alto GDP (aquellas encerradas 
# en el recuadro rojo). Identifíque dichas observaciones e indique claramente a qué país(es) 
# y año(s) corresponden.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, color = year)) +
  geom_point() +
  scale_size_area() +
  xlab("gdpPercap") +
  ylab("lifeExp") +
  ggtitle("Relación entre GDP per capita y Life Exp") +
  theme_minimal()

RP1 <- gapminder[gapminder$gdpPercap > 50000, ]

# Respuesta: Las observaciones con alto GPD identificadas en el recuadro rojo
# se detallan en la variable RP1 y corresponden a Kuwai para los siguientes años:

# 1952 (GPD = 108382.35), 
# 1957 (GPD = 113523.13),
# 1962 (GPD = 95458.11)
# 1967 (GPD = 80894.88)
# 1972 (GPD = 109347.87)
# 1977 (GPD = 59265.48)





# Pregunta 2.
# Mediante un gráfico de puntos, visualice una comparativa entre la relación de ingresos 
# y expectativa de vida, para los ños 1952 y 2007. Para ello usted deberá replicar el 
# siguiente gráfico, donde el color representa a un continente distinto y el tamaño
# está dado por el total de población.

datos_P2 <- gapminder[gapminder$year %in% c(1952, 2007), ]

plot <- ggplot(datos_P2, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  xlab("gdpPercap") +
  ylab("lifeExp") +
  ggtitle("Comparativa GDP per capita y Life Exp") +
  facet_wrap(~ year, nrow = 1)

print(plot)



# Pregunta 3.

# Determine el nivel de correlación de spearman, entre las variables gdpPercap y lifeExp 
# para cada uno de los años registrados
# ¿En qué año se observa el mayor nivel de correlación entre ambas variables?

correlacion <- by(gapminder, gapminder$year, function(df) {
  cor(df$gdpPercap, df$lifeExp, method = "spearman")})

mayor_nivel_correlacion <- names(correlacion)[which.max(correlacion)]

cat("El año con el mayor nivel de correlación entre gdpPercap y lifeExp es:", mayor_nivel_correlacion, "\n")

# Respuesta: El año de mayor nivel de correlación entre ambas variables es 1992



# Pregunta 4.
# Para el año obtenido en la pregunta anterior, realice una breve descripción de la distribución 
# de la expectativa de vida lifeExp. E indique lo siguiente:
# ¿Cuál fue la esperanza de vida promedio considerando todos los países registrados?
# ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?


year_data <- gapminder[gapminder$year == mayor_nivel_correlacion, ]

lifeExp_prom <- mean(year_data$lifeExp)

print(lifeExp_prom)

# La esperanza de vida promedio es 64.16

mayor_lifeExp_country <- year_data[which.max(year_data$lifeExp), "country"]

print(mayor_lifeExp_country)

# El pais con mayor esperanza de vida promedio es Japón 



# Pregunta 5a.
# Mediante el test de shapiro, indique los dos continentes que presentan un 
# comportamiento normal en la distribución del logaritmo de lifeExp.


gapminder <- gapminder %>%
  mutate(log_lifeExp = log(lifeExp))

gapminder %>%
  group_by(continent) %>%
  summarise(shapiro_pvalue = shapiro.test(log_lifeExp)$p.value) %>%
  arrange(shapiro_pvalue)

# los valores más cercanos a 1 indican una mayor evidencia de que los datos siguen 
# una distribución normal. En este caso, los valores de los continentes más cercanos a 1 
# son Oceania y Africa, con p-values de 9.12e-2 y 1.17e-1 respectivamente.



# Pregunta 5b.
# Para los continentes encontrados en a), grafique los histogramas para el logaritmo de lifeExp.
# Considere añadir estimaciones de las densidades, dadas por geom_density así como una
# densidad normal con parámetros de media y desviación estandar igual a la media
# y desviación estandar muestral. ¿Qué opina sobre el histograma de Oceanía? 
# Comente sobre posibles causas de su aspecto.

datos_continentes <- gapminder[gapminder$continent %in% c("Oceania", "Africa"), ]

ggplot(datos_continentes, aes(x = log_lifeExp, fill = continent)) +
  geom_histogram(binwidth = 0.2, alpha = 0.7, position = "identity") +
  geom_density(aes(color = continent), linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(datos_continentes$log_lifeExp), sd = sd(datos_continentes$log_lifeExp)), linetype = "dashed", linewidth = 1) +
  facet_wrap(~ continent) +
  labs(x = "Logaritmo de lifeExp", y = "Frecuencia", fill = "Continente", color = "Continente") +
  ggtitle("Histogramas y densidades del Logaritmo de lifeExp por Continente")

# Entre las posibles causas del aspecto del histograma de Oceanía, se encuentran
# la mayor variabilidad de los datos respecto al resto de los continentes, y
# la presencia de valores atípicos (outliers) en comparación con otros continentes.

# Pregunta 5c.
# Complemente lo anterior, visualizando los qqplots para el logaritmo de lifeExp.
# Considere la utilización de las funciones qqnorm() y qqline() para el contraste 
# contra una distribución normal.

qqnorm(gapminder$log_lifeExp)
qqline(gapminder$log_lifeExp)

# Pregunta 6.
# Considerando el continente de África, y asumiendo normalidad en el logaritmo de lifeExp. 
# Independiente del año, ¿cuál es la probabilidad de que la expectativa de vida (lifeExp) 
# sea superior a 54 años?


datos_africa <- gapminder[gapminder$continent == "Africa", ]

media <- mean(datos_africa$log_lifeExp)
desviacion <- sd(datos_africa$log_lifeExp)

probabilidad_54 <- 1 - pnorm(log(54), mean = media, sd = desviacion)

print(probabilidad_54)

# La probabilidad es del 26,34%


#**************************** SECCIÓN 2 ****************************

# Pregunta 1

# Genere tres nuevas columnas, que contengan la hora, minutos y segundos 
# de la transacción registrada.

install.packages("tidyr")
library(tidyr)

cafeteria <- Cafeteria

cafeteria <- cafeteria %>%
  separate(Time, c("hora", "min", "seg"), sep = ':')



# Pregunta 2.
# Genere una tabla resumen que contenga la siguiente informació:

# hora: Hora donde se registraron las transacciones. por ejemplo, el valor 09 indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
# total_trx : total de transacciones distintas generadas en el bloque horario respectivo.
# total_items: total de items vendidos en el bloque horario respectivo.
# total_items_unicos: total de items únicos venidos en el bloque horario respectivo.

## Nota: Se declara no considerar los items vacios o NONE

Tabla_resumen <- cafeteria %>% 
  filter(Item != 'NONE') %>% group_by(hora) %>% 
  summarise(total_trx = n_distinct(Transaction), total_items = n(),
    total_items_unicos = n_distinct(Item))

View(Tabla_resumen)

# Pregunta 3.
# Con base en la tabla anterior, diremos que una hora pertenece al horario punta 
# si la cantidad de transacciones distintas generadas en dicho bloque supera las 1000 transacciones.

# 3a. ¿Qué horas comprende el horario punta?

Tabla_resumen %>% 
  filter(total_trx > 1000)

# Desde las 09 hasta las 14 horas comprende el horario punta

# 3b. En promedio, ¿cuántas transacciones distintas por hora se dieron en horario punta?
# ¿y en horario no punta?

Tabla_resumen %>%
  summarise(mean(total_trx))

# En promedio se dieron 526 transacciones distintas en horario punta

Tabla_resumen %>% 
  filter(total_trx <= 1000) %>% 
  summarise(mean(total_trx))

# En promedio se dieron 180 transacciones en horario no punta.


# Pregunta 4.
# Se sabe que el total de personal disponible es capaz de atender 
# como máximo, 1300 transacciones por hora, de modo que no se "sature" el sistema y que
# los tiempos de espera de los clientes sean razonables. 
# Asumiendo que la cantidad de transacciones por hora tiene una distribución Poisson con parámetro 
# igual al estimado en la pregunta 3.b (hora punta) responda lo siguiente:

# 4a. ¿Cuál es la probabilidad de que en horario punta se den más de 1300 transacciones en una hora? 
# ¿Cómo interpretaría este valor? Comente.


ppois(1300,lambda = cafeteria[[1, 1]],lower.tail = FALSE)

#  La probabilidad de que sea mayor o igual a 1300 es 1, es decir, la probabilidad es del 100%.

# 4b. Con el objetivo de reducir costos, se propone limitar el personal disponible 
# a modo de poder atender como máximo 1250 transacciones por hora. 
# ¿Que tan probable es que se supere este máximo de transacciones por hora ? 
# ¿Recomendaría usted esta medida?

ppois(1250,lambda = round(cafeteria[[1, 1]]), lower.tail = FALSE

# La probabilidad de que se supere 1250 transacciones por hora es de 16,12%
# No recomendaría esta medida debido a que se corre el riesgo de afectar la calidad en
# el tiempo de atención de clientes


# 4c. Usted sugiere modificar la cantidad de personal pero teniendo en cuenta de que 
# se garantice la atención de al menos un 95% de las transacciones por hora. 
# ¿Cuántas transacciones por hora se deberían poder gestionar en este escenario?

qpois(0.95,lambda = round(cafeteria[[1, 1]]))

# se deberían poder gestionar 1274 transacciones por hora en ese escenario.

# Pregunta 5
# ¿Cuáles son los 5 items más vendidos? Ilustre mediante un gráfico de barras o una tabla.


Top_5 <- cafeteria %>% 
  group_by(Item) %>% 
  summarise(item_amount = n()) %>% 
  arrange(desc(item_amount)) %>% 
  head(5)

Top_5 %>%
  ggplot(aes(x = reorder(Item, item_amount), y = item_amount, fill = Item)) +
  labs(x = "Item", y = "Cantidad") +
  ggtitle("Los 5 items más vendidos") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_label(aes(label = item_amount), color = "white", position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Los 5 items son Coffee, Bread, Tea, Cake, Pastry.

# Pregunta 6.
# ¿Cambian estos 5 ítems según el horario de atención? Para ello muestre los 5 items
# más vendidos en los siguientes horarios:

# 7:00-11:59
# 12:00-16:59
# 17:00-23:59


Top_5_nuevo_horario <- cafeteria %>%
  filter(Item != 'NONE') %>%
  mutate(hour_band = case_match(
    hora,
    c('07', '08', '09', '10', '11') ~ '07:00 - 11:59',
    c('12', '13', '14', '15', '16') ~ '12:00 - 16:59',
    c('17', '18', '19', '20', '21', '22', '23') ~ '17:00 - 23:59',
  )) %>% 
  filter(!is.na(hour_band))

resultados <- Top_5_nuevo_horario %>% 
  group_by(hour_band, Item) %>% 
  summarise(selled_items = n()) %>% 
  arrange(hour_band, desc(selled_items)) %>% 
  group_by(hour_band) %>% 
  top_n(5)

ggplot(resultados, aes(x = hour_band, y = selled_items, fill = Item)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rango horario", y = "Cantidad de items vendidos") +
  ggtitle("Top 5 de items vendidos por rango horario") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")) +
  theme_minimal()


# Primer turno 7:00-11:59: Coffee, Bread, Pastry, Tea, medialuna
# Segundo turno 12:00-16:59: Coffee, Bread, Tea, Cake, Sandwich
# Tercer turno 17:00-23:59: Coffee, Bread, Tea, Cake, Hot Chocolate

# Si cambian los 5 productos más vendidos según el horario o turno de atención.



# Pregunta 7.

#Considerando un support mínimo de 0.02 , un confidence mínimo de 0.1 y teniendo en cuenta
# que no se deben considerar reglas de asociación cuyo antecedente o consecuente sean vacíos.

# 7a. ¿Cuál es la regla de asociación más frecuente en cada uno de los horarios indicados en P6)?

## Paso 1: Se crean las reglas de asociación para cada turno u horario

reglas_asociacion_primer_turno <- Top_5_nuevo_horario %>% 
  filter(hour_band == '07:00 - 11:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

reglas_asociacion_segundo_turno <- Top_5_nuevo_horario %>% 
  filter(hour_band == '12:00 - 16:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

reglas_asociacion_tercer_turno <- Top_5_nuevo_horario %>% 
  filter(hour_band == '17:00 - 23:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

# Paso 2: Se determinan las reglas de asociación más frecuentes por turno u horario

reglas_asociacion_primer_turno %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el primer turno la reglas de asociación mas frecuente es {Bread} => {Coffee}

reglas_asociacion_segundo_turno %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el segundo turno la reglas de asociación mas frecuente es {Bread} => {Coffee}

reglas_asociacion_tercer_turno %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el segundo turno la reglas de asociación mas frecuente es {Cake} => {Coffee}

# 7b. ¿Cuál es la regla de asociación con mayor confidence en cada uno 
# de los horarios indicados en P6)?

reglas_asociacion_primer_turno %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el primer turno la regla de asociación con mayor confidence es {Toast} => {Coffee}

reglas_asociacion_segundo_turno %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el segundo turno la regla de asociación con mayor confidence es {Pastry} => {Coffee}

reglas_asociacion_tercer_turno %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el tercer turno la regla de asociación con mayor confidence es {Postcard} => {Tshirt}

# 7c. ¿Cuál es la regla de asociación con mayor lift en cada uno de los horarios indicados en P6)?

reglas_asociacion_primer_turno %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el primer turno la regla de asociación con mayor lift es {Toast} => {Coffee}

reglas_asociacion_segundo_turno %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el segundo turno la regla de asociación con mayor lift es {Cake} => {Tea}

reglas_asociacion_tercer_turno %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

# Para el tercer turno la regla de asociación con mayor lift es {Tshirt} => {Postcard}

# Pregunta 8.
# Se quiere potenciar un segundo producto por la compra de un café en los tres horarios 
# definidos previamente en P6.


# 8a. Genere tres listados (uno por cada rango horario) con todas las reglas 
# que contengan el producto Coffee en el antecedente.

listado_1 <- Top_5_nuevo_horario %>% 
  filter(hour_band == '07:00 - 11:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee'))

View(inspect(listado_1))


listado_2 <- Top_5_nuevo_horario %>% 
  filter(hour_band == '12:00 - 16:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee'))

View(inspect(listado_2))

listado_3 <- Top_5_nuevo_horario  %>% 
  filter(hour_band == '17:00 - 23:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee'))

View(inspect(listado_3))

# 8b ¿Qué promoción recomendaría en cada horario por la compra de un café?. 
# Justifique su respuesta basándose en los indicadores support, confidence y lift.

# Promoción recomendada horario de 7 a 11:59 : Por la compra de un café, se puede ofrecer 
# un descuento en la Medialuna.

# Promoción recomendada horario de 12 a 16:59 : Por la compra de un café, se puede ofrecer 
# un descuento en el Sandwich.

  
# Promoción recomendada horario de 17 a 23:59 : Por la compra de un café, se puede ofrecer 
# un descuento en el Alfajores.

# Estas promociones recomendadas se basan en los resultados con un alto support y confidence, 
# lo que indica que tienen una alta probabilidad de ocurrir cuando se compra un café. 
# Además, el lift también se tiene en cuenta para evaluar la fuerza de la asociación 
# entre el antecedente (compra de café) y el consecuente (elemento promocionado). 
# En general, las promociones recomendadas se basan en los consecuentes más frecuentes 
# y con mayor probabilidad condicional, lo que sugiere una fuerte asociación con la compra de café.

