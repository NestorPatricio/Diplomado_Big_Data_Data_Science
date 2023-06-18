# Control 2

#Nombre1: Néstor Patricio Rojas Ríos

# Instalación e importación de librerías:
librerias <- c('tidyverse', 'arules', 'gapminder')
for (libreria in librerias) {
  if(!is.element(libreria, .packages())) {
    install.packages(libreria)
  }
  library(libreria, character.only = TRUE)
}

# Configuración de la carpeta de trabajo, sólo para trabajo en mi local:
carpeta_de_trabajo_linux <- paste0(
  '~/Documentos/Diplomado_Big_Data_Data_Science/Programación en R para Ciencia',
  ' de Datos/Material de GitHub/Evaluaciones/C3'
)
carpeta_de_trabajo_windows <- paste0(
  'C:/Users/nproj/Documents/Diplomado_Big_Data_Data_Science/Programación en R',
  ' para Ciencia de Datos/Material de GitHub/Evaluaciones/C3'
)

setwd(carpeta_de_trabajo_linux)
setwd(carpeta_de_trabajo_windows)

# Carga de datasets:
data(gapminder)
cafeteria <- read.csv('cafeteria.csv')

# SECCIÓN 1
# P1) Identifique las observaciones con alto GDP e indique claramente a qué
#país(es) y año(s) corresponden (aquellas encerradas en el recuadro rojo).
respuesta11 <- gapminder %>% 
  filter(gdpPercap > 50000) %>% 
  select(country, year) %>% 
  mutate(country = as.character(country))

print(paste0(
  '1.1) Los registros que presentan alto GDP corresponden a ',
  respuesta11[1, 'country'], # Kuwait
  ' durante los años ',
  respuesta11[1, 'year'], # 1952
  ', ',
  respuesta11[2, 'year'], # 1957
  ', ',
  respuesta11[3, 'year'], # 1962
  ', ',
  respuesta11[4, 'year'], # 1967
  ', ',
  respuesta11[5, 'year'], # 1972
  ' y ',
  respuesta11[6, 'year'], # 1977
  '.'
))

# P2) Mediante un gráfico de puntos, visualice una comparativa entre la relación
#de ingresos y expectativa de vida, para los años 1952 y 2007. Para ello usted
#deberá replicar el siguiente gráfico, donde el color representa a un continente
#distinto y el tamaño está dado por el total de población.
respuesta12 <- gapminder %>% 
  filter(year %in% c(1952, 2007)) %>% 
  ggplot() +
  aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent) +
  facet_wrap(vars(year)) +
  geom_point()

respuesta12

# P3) Determine el nivel de correlación de Spearman, entre las variables
#gdpPercap y lifeExp para cada uno de los años registrados. ¿En qué año se
#observa el mayor nivel de correlación entre ambas variables?
respuesta13 <- gapminder %>% 
  group_by(year) %>% 
  summarise(sp_cor = cor(gdpPercap, lifeExp, method = 'spearman')) %>%
  arrange(desc(sp_cor))

print(paste0(
  '1.3) El año en que se observa mayor nivel de correlación fue ',
  respuesta13[1, 'year'], # 1992
  ' con un coeficiente de correlación de Spearman de ',
  round(respuesta13[1, 'sp_cor'], 3), # 0.897
  '.'
))

# P4) Para el año obtenido en la pregunta anterior, realice una breve descrición
#de la distribución de la expectativa de vida lifeExp.
respuesta14 <- gapminder %>% 
  filter(year == respuesta13[[1, 'year']])

# P4.a) ¿Cuál fue la esperanza de vida promedio considerando todos los países
#registrados?
respuesta14a <- respuesta14 %>% 
  summarise(mean(lifeExp))

print(paste0(
  '1.4.a) La esperanza de vida promedio para el año ',
  respuesta13[1, 'year'],
  ' fue de ',
  round(respuesta14a, 1),
  ' años.'
))

# P4.b) ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?
respuesta14b <- respuesta14 %>%
  mutate(country = as.character(country)) %>%
  arrange(desc(lifeExp))

print(paste0(
  '1.4.b) El país con mayor esperanza de vida para el año ',
  respuesta13[1, 'year'],
  ' fue ',
  respuesta14b[1, 'country'],
  ' con ',
  respuesta14b[1, 'lifeExp'],
  ' años.'
))

# P5.a) Considerando todos los años de observación, determine mediante el test
#de shapiro, indique los dos continentes que presentan un comportamiento normal
#en la distribución del logaritmo de lifeExp.
continentes = c()
p_values = c()
for (continente in levels(gapminder$continent)) {
  respuesta = gapminder %>%
    filter(continent == continente) %>%
    mutate(log_lifeExp = log(lifeExp)) %>% 
    select(log_lifeExp) %>%
    unlist() %>% 
    shapiro.test()
  if (respuesta$p.value >= 0.05) {
    continentes = c(continentes, continente)
    p_values = c(p_values, respuesta$p.value)
  }
}
respuesta15a <- data.frame(continentes, p_values)

print(paste0(
  'P1.5.a) Al analizar la distribución de los logaritmos de los valores de la ',
  'expectativa de vida por continente, sólo ',
  respuesta15a[1, 'continentes'],
  ' y ',
  respuesta15a[2, 'continentes'],
  ' tienen una distribución normal, pues en el test de Shapiro presentan ',
  'un p-value de ',
  round(respuesta15a[1, 'p_values'], 3),
  ' y ',
  round(respuesta15a[2, 'p_values'], 3),
  ', respectivamente, con lo cual podemos descartar que no tengan una ',
  'distribución normal con un 5% de significancia.'
))

# P5.b) Para los continentes encontrados en a) grafique los histogramas para el
#logaritmo de lifeExp. Considere añadir estimaciones de las densidades, dadas
#por geom_density así como una densidad normal con parámetros de media y
#desviación estandar igual a la media y desviación estandar muestral ¿qué opina
#sobre el histograma de Oceanía? Comente sobre posibles causas de su aspecto.
africa <- gapminder %>% 
  filter(continent == respuesta15a[[1, 'continentes']]) %>% 
  mutate(log_lifeExp = log(lifeExp))
respuesta15b1 <- africa%>% 
  ggplot() +
  aes(x = log_lifeExp) +
  facet_wrap(~ continent) +
  geom_histogram(aes(y = after_stat(density)), fill = '#999933') +
  geom_density(colour = '#FF3333') +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(africa$log_lifeExp), sd = sd(africa$log_lifeExp)),
    colour = '#33CC33'
  )

oceania <- gapminder %>% 
  filter(continent == respuesta15a[[2, 'continentes']]) %>% 
  mutate(log_lifeExp = log(lifeExp))
respuesta15b2 <- oceania%>% 
  ggplot() +
  aes(x = log_lifeExp) +
  facet_wrap(~ continent) +
  geom_histogram(aes(y = after_stat(density)), fill = '#339999') +
  geom_density(colour = '#FF3333') +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(oceania$log_lifeExp), sd = sd(oceania$log_lifeExp)),
    colour = '#33CC33'
  )

respuesta15b3 <- paste(
  'P1.5.b) Si bien el histograma de los valores del logaritmo de las',
  'expectativas de vida de Oceanía tiende a parecerse a una distribución',
  'normal, no tiene la forma de campana típica, como si la tiene el de África.',
  'Un factor que puede estar influyendo en la forma puede ser la presencia de',
  'países con distinto estándar de vida, con Australia y Nueva Zelanda en lo',
  'alto, junto con países de economías más modestas, como son los de las islas',
  'de la Polinesia.'
)

respuesta15b1
respuesta15b2
print(respuesta15b3)

# P5.c) Complemente lo anterior, visualizando los qqplots para el logaritmo de
#lifeExp. Considere la utilización de las funciones qqnorm() y qqline() para el
#contraste contra una distribución normal.
respuesta15c <- 


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
