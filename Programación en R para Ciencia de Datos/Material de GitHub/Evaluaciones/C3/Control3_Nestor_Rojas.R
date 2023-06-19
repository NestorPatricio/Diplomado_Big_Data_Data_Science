# Control 3

#Nombre1: Néstor Patricio Rojas Ríos

####################### Configuración preliminar #######################
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

####################### Configuración preliminar #######################

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
  respuesta13[1, 'year'], # 1992
  ' fue de ',
  round(respuesta14a, 1), # 64.2
  ' años.'
))

# P4.b) ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?
respuesta14b <- respuesta14 %>%
  arrange(desc(lifeExp)) %>%
  mutate(country = as.character(country))

print(paste0(
  '1.4.b) El país con mayor esperanza de vida para el año ',
  respuesta13[1, 'year'], # 1992
  ' fue ',
  respuesta14b[1, 'country'], # Japan
  ' con ',
  respuesta14b[1, 'lifeExp'], # 79.36
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
  respuesta15a[1, 'continentes'], # Africa
  ' y ',
  respuesta15a[2, 'continentes'], # Oceania
  ' tienen una distribución normal, pues en el test de Shapiro presentan ',
  'un p-value de ',
  round(respuesta15a[1, 'p_values'], 3), # 0.117
  ' y ',
  round(respuesta15a[2, 'p_values'], 3), # 0.091
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
respuesta15c <- gapminder %>% 
  mutate(log_lifeExp = log(lifeExp)) %>% 
  select(log_lifeExp) %>%
  unlist()

qqnorm(respuesta15c)
qqline(respuesta15c)

# P6) onsiderando el continente de África, y asumiendo normalidad en el
#logaritmo de lifeExp. Independiente del año ¿cuál es la probabilidad de que la
#expectativa de vida (lifeExp) sea superior a 54 años?
expectativa_africa <- gapminder %>% 
  filter(continent == 'Africa') %>% 
  mutate(log_lifeExp = log(lifeExp)) %>% 
  select(log_lifeExp) %>% 
  unlist()
respuesta16 <- pnorm(
  log(54),
  mean = mean(expectativa_africa),
  sd = sd(expectativa_africa),
  lower.tail = FALSE
)

print(paste0(
  'P1.6) Asumiendo una distribución normal para los valores del logaritmo de ',
  'la expectativa de vida, independiente del año, la probabilidad de tener una',
  ' vida superior a 54 años en el continente africano es de ',
  round((respuesta16 * 100), 2), # 26.35
  '%.'
))

print('-----------------------------------------------------------------------')

# SECCIÓN 2
# P1) Genere tres nuevas columnas, que contengan la hora, minutos y segundos de
#la transacción registrada.
respuesta21 <- cafeteria %>%
  separate(Time, c('hora', 'min', 'seg'), sep = ':')

View(respuesta21)

# P2) Genere una tabla resumen que contenga la siguiente información:
# hora: hora donde se registraron las transacciones. Por ejemplo, el valor 09
#indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
# total_trx: total de transacciones distintas generadas en el bloque horario
#respectivo.
# total_items: total de items vendidos en el bloque horario respectivo.
# total_items_unicos: total de items únicos venidos en el bloque horario
#respectivo.
respuesta22 <- respuesta21 %>% 
  filter(Item != 'NONE') %>% 
  group_by(hora) %>% 
  summarise(
    total_trx = n_distinct(Transaction),
    total_items = n(),
    total_items_unicos = n_distinct(Item)
  )

View(respuesta22)
print(paste(
  'Se decide trabajar los datos excluyendo los registros cuyo valor en Item',
  'es NONE.'
))

# P3) Con base en la tabla anterior, diremos que una hora pertenece al horario
#punta si la cantidad de transacciones distintas generadas en dicho bloque
#supera las 1000 transacciones.
# P3.a) ¿Qué horas comprende el horario punta?
respuesta23a <- respuesta22 %>% 
  filter(total_trx > 1000)

print(paste0(
  'P2.3a) Excluyendo los registros con la etiqueta NONE en los ítems, las ',
  'horas que comprenden el horario punta son las ',
  respuesta23a[1, 'hora'], # 09
  ', las ',
  respuesta23a[2, 'hora'], # 10
  ', las ',
  respuesta23a[3, 'hora'], # 11
  ', las ',
  respuesta23a[4, 'hora'], # 12
  ', las ',
  respuesta23a[5, 'hora'], # 13
  ' y las ',
  respuesta23a[6, 'hora'], # 14
  ' horas.'
))
# P3.b) En promedio ¿cuántas transacciones distintas por hora se dieron en
#horario punta? ¿Y en horario no punta?
respuesta23b1 <- respuesta23a %>%
  summarise(mean(total_trx))

respuesta23b2 <- respuesta22 %>% 
  filter(total_trx <= 1000) %>% 
  summarise(mean(total_trx))

print(paste0(
  'P2.3b) Excluyendo los registros con la etiqueta NONE en los ítems, en ',
  'horario punta se realizan, en promedio, unas ',
  round(respuesta23b1[1, 1]), # 1216
  ' transacciones por hora, mientras que el resto del tiempo, en promedio, se ',
  'realizan sólo ',
  round(respuesta23b2[1, 1]), # 180
  ' transacciones por hora.'
))

# P4) Se sabe que el total de personal disponible es capaz de atender como
#máximo, 1300 transacciones por hora, de modo que no se "sature" el sistema y
#que los tiempos de espera de los clientes sean razonables. Asumiendo que la
#cantidad de transacciones por hora tiene una distribución Poisson con parámetro
#igual al estimado en la pregunta 3.b (hora punta):
# P4.a) ¿Cuál es la probabilidad de que en horario punta se den más de 1300
#transacciones en una hora? ¿Cómo interpretaría este valor? Comente.
respuesta24a <- ppois(
  1300,
  lambda = round(respuesta23b1[[1, 1]]),
  lower.tail = FALSE
)

print(paste0(
  'P2.4a) Excluyendo los registros con la etiqueta NONE en los ítems, y ',
  'asumiendo una distribución de Poisson para la cantidad de transacciones por',
  ' hora, con un valor de lambda igual al promedio de transacciones por hora ',
  'en el bloque punta, la probabilidad de que en una hora se realicen más de ',
  '1.300 transacciones es de ',
  round((respuesta24a * 100), 2), # 0.82
  '%. Esto significa que menos del 1% de las transacciones se realizarán en un',
  ' tiempo que superará lo razonable.'
))

# P4.b) Con el objetivo de reducir costos, se propone limitar el personal
#disponible a modo de poder atender como máximo 1.250 transacciones por hora
#¿que tan probable es que se supere este máximo de transacciones por hora?
#¿Recomendaría usted esta medida?
respuesta24b <- ppois(
  1250,
  lambda = round(respuesta23b1[[1, 1]]),
  lower.tail = FALSE
)

print(paste0(
  'P2.4b) Excluyendo los registros con la etiqueta NONE en los ítems, y ',
  'asumiendo una distribución de Poisson para la cantidad de transacciones por',
  ' hora, con un valor de lambda igual al promedio de transacciones por hora ',
  'en el bloque punta, la probabilidad de que en una hora se realicen más de ',
  '1.250 transacciones es de ',
  round((respuesta24b * 100), 2), # 16.12
  '%. Personalmente, no recomendaría este recorte del personal, pues implica ',
  'que en una de cada cuatro transacciones el tiempo la espera será excesivo, ',
  'lo cual terminará espantando clientes.'
))

# P4.c) Usted sugiere modificar la cantidad de personal pero teniendo en cuenta
#de que se garantice la atención de al menos un 95% de las transacciones por
#hora ¿cuántas transacciones por hora se deberían poder gestionar en este
#escenario?
respuesta24c <- qpois(
  0.95,
  lambda = round(respuesta23b1[[1, 1]])
)

print(paste0(
  'P2.4c) Excluyendo los registros con la etiqueta NONE en los ítems, y ',
  'asumiendo una distribución de Poisson para la cantidad de transacciones por',
  ' hora, con un valor de lambda igual al promedio de transacciones por hora ',
  'en el bloque punta, si al menos un 95% de las transacciones se realizan en ',
  'un tiempo razonable, se debería contar con personal suficiente para ',
  'realizar ',
  respuesta24c, # 1274
  ' transacciones por hora.'
))

# P5) ¿Cuáles son los 5 items más vendidos? Ilustre mediante un gráfico de
#barras o una tabla.
respuesta25 <- respuesta21 %>% 
  group_by(Item) %>% 
  summarise(item_amount = n()) %>% 
  arrange(desc(item_amount)) %>% 
  head(5)

grafico_respuesta25 <- respuesta25 %>% 
  ggplot() +
  aes(
    y = reorder(Item, item_amount),
    x = item_amount,
    fill = reorder(Item, item_amount),
    label = item_amount,
  ) +
  geom_col() +
  geom_label(
    colour = '#000000',
    fill = '#FFFFFF',
    size = 6
  ) +
  labs(
    title = 'Los 5 productos más vendidos de la cafetería.',
    x = 'Cantidad vendiada de toda la muestra',
    y = 'Ítems'
  ) +
  theme(
    legend.position = 'none',
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    title = element_text(size = 17)
  )
grafico_respuesta25

print(paste0(
  'P2.5) Excluyendo los registros con la etiqueta NONE en los ítems, los 5 ',
  'productos más vendidos de la cafetería según la muestra son ',
  respuesta25[1, 'Item'], # Cofee
  ' con ',
  respuesta25[1, 'item_amount'], # 5471
  ' unidades vendidas, ',
  respuesta25[2, 'Item'], # Bread
  ' con ',
  respuesta25[2, 'item_amount'], # 3225
  ' unidades vendidas, ',
  respuesta25[3, 'Item'], # Tea
  ' con ',
  respuesta25[3, 'item_amount'], # 1435
  ' unidades vendidas, ',
  respuesta25[4, 'Item'], # Cake
  ' con ',
  respuesta25[4, 'item_amount'], # 1025
  ' unidades vendidas y ',
  respuesta25[5, 'Item'], # Pastry
  ' con ',
  respuesta25[5, 'item_amount'], # 856
  ' unidades vendidas. Se puede ver la misma información en el gráfico.'
))

# P6) ¿Cambian estos 5 ítems según el horario de atención? Para ello muestre los
#5 items más vendidos en los siguientes horarios:
# 07:00-11:59
# 12:00-16:59
# 17:00-23:59
cafeteria_con_horarios <- respuesta21 %>%
  filter(Item != 'NONE') %>%
  mutate(hour_band = case_match(
    hora,
    c('07', '08', '09', '10', '11') ~ '07:00 - 11:59',
    c('12', '13', '14', '15', '16') ~ '12:00 - 16:59',
    c('17', '18', '19', '20', '21', '22', '23') ~ '17:00 - 23:59',
  )) %>% 
  filter(!is.na(hour_band))
respuesta26 <- cafeteria_con_horarios %>% 
  group_by(hour_band, Item) %>% 
  summarise(selled_items = n()) %>% 
  arrange(hour_band, desc(selled_items)) %>% 
  group_by(hour_band) %>% 
  top_n(5)

grafico_respuesta26 <- respuesta26 %>% 
  ggplot() +
  aes(
    x = reorder(Item, selled_items),
    y = selled_items,
    fill = Item,
    labels = selled_items
  ) +
  facet_wrap(vars(hour_band)) +
  geom_col() +
  labs(
    title = 'Los 5 productos más vendidos de la cafetería por banda horaria.',
    x = 'Ítems',
    y = 'Cantidad vendiada de por banda horaria'
  ) +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 10, angle = 45),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13),
    title = element_text(size = 15)
  )
grafico_respuesta26

print(paste(
  'P2.6) Excluyendo los registros con la etiqueta NONE en los ítems, como se',
  'puede ver en el gráfico, los productos más vendidos y su cantidad de ventas',
  'cambia según la franja horaria. Sin embargo, se observa que en el top 5',
  'siempre están Cofee, Bread y Tea.'
))

# P7) Considerando un support mínimo de 0.02, un confidence mínimo de 0.1 y
#teniendo en cuenta que no se deben considerar reglas de asociación cuyo
#antecedente o consecuente sean vacíos:
reglas_manana <- cafeteria_con_horarios %>% 
  filter(hour_band == '07:00 - 11:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

reglas_tarde <- cafeteria_con_horarios %>% 
  filter(hour_band == '12:00 - 16:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

reglas_noche <- cafeteria_con_horarios %>% 
  filter(hour_band == '17:00 - 23:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(parameter = list(supp = 0.02, conf = 0.1, minlen = 2))

# P7.a) ¿Cuál es la regla de asociación más frecuente en cada uno de los
#horarios indicados en P6)?
respuesta27a1 <- reglas_manana %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27a2 <- reglas_tarde %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27a3 <- reglas_noche %>%
  sort(by = 'count', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

print(paste0(
  'P2.7a) Excluyendo los registros con la etiqueta NONE en los ítems, las ',
  'reglas de asociación más frecuentes por horario son ',
  respuesta27a1[1, 'rules'], # Bread -> Coffe
  ' presente en el ',
  round((respuesta27a1[1, 'support'] * 100), 2), # 9.41
  '% de la transacciones en el horario entre las 07:00 y las 11:59 horas, ',
  respuesta27a2[1, 'rules'], # Bread -> Coffe
  ' presente en el ',
  round((respuesta27a2[1, 'support'] * 100), 2), # 9
  '% de la transacciones en el horario entre las 12:00 y las 16:59 horas, y ',
  respuesta27a3[1, 'rules'], # Cake -> Coffee
  ' presente en el ',
  round((respuesta27a3[1, 'support'] * 100), 2), # 5.86
  '% de las transacciones en el horario entre las 17:00 y las 23:59 horas.'
))

# P7.b) ¿Cuál es la regla de asociación con mayor confidence en cada uno de los
#horarios indicados en P6)?
respuesta27b1 <- reglas_manana %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27b2 <- reglas_tarde %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27b3 <- reglas_noche %>%
  sort(by = 'confidence', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

print(paste0(
  'P2.7b) Excluyendo los registros con la etiqueta NONE en los ítems, las ',
  'reglas de asociación con mayor confianza por horario son ',
  respuesta27b1[1, 'rules'], # Toast -> Coffe
  ' con una confianza del ',
  round((respuesta27b1[1, 'confidence'] * 100), 2), # 72.06
  '% en el horario entre las 07:00 y las 11:59 horas, ',
  respuesta27b2[1, 'rules'], # Pastry -> Coffee
  ' con una confianza del ',
  round((respuesta27b2[1, 'confidence'] * 100), 2), # 55.79
  '% en el horario entre las 12:00 y las 16:59 horas y ',
  respuesta27b3[1, 'rules'], # Postcard -> Tshirt
  ' con una confianza del ',
  round((respuesta27b3[1, 'confidence'] * 100), 2), # 60
  '% en el horario entre las 17:00 y las 23:59 horas.'
))

# P7.c) ¿Cuál es la regla de asociación con mayor lift en cada uno de los
#horarios indicados en P6)?
respuesta27c1 <- reglas_manana %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27c2 <- reglas_tarde %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

respuesta27c3 <- reglas_noche %>%
  sort(by = 'lift', decreasing = TRUE) %>%
  as('data.frame') %>%
  first()

print(paste0(
  'P2.7c) Excluyendo los registros con la etiqueta NONE en los ítems, las ',
  'reglas de asociación con mayor lift por horario son ',
  respuesta27c1[1, 'rules'], # Toast -> Coffe
  ' con un lift de ',
  round(respuesta27c1[1, 'lift'], 2), # 1.4
  ' veces al llevarlos juntos en vez de por separado en el horario entre las ',
  '07:00 y las 11:59 horas, ',
  respuesta27c2[1, 'rules'], # Cake -> Tea
  ' con un lift de ',
  round(respuesta27c2[1, 'lift'], 2), # 1.4
  ' veces al llevarlos juntos en vez de por separado en el horario entre las ',
  '12:00 y las 16:59 horas y ',
  respuesta27c3[1, 'rules'], # Tsirt -> Postcard
  ' con un lift de ',
  round(respuesta27c3[1, 'lift'], 2), # 7.8
  ' veces al llevarlos juntos en vez de por separado en el horario entre las ',
  '17:00 y las 23:59 horas.'
))

# P8) Se quiere potenciar un segundo producto por la compra de un café en los
#tres horarios definidos previamente en P6:
# P8.a) Genere tres listados (uno por cada rango horario) con todas las reglas
#que contengan el producto Coffee en el antecedente.
respuesta28a1 <- cafeteria_con_horarios %>% 
  filter(hour_band == '07:00 - 11:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(
    parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee')
  )
View(inspect(respuesta28a1))

respuesta28a2 <- cafeteria_con_horarios %>% 
  filter(hour_band == '12:00 - 16:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(
    parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee')
  )
View(inspect(respuesta28a2))

respuesta28a3 <- cafeteria_con_horarios %>% 
  filter(hour_band == '17:00 - 23:59') %>% 
  transactions(format = 'long', cols = c('Transaction', 'Item')) %>%
  apriori(
    parameter = list(supp = 0.02, conf = 0.1, minlen = 2),
    appearance = list(lhs = 'Coffee')
  )
View(inspect(respuesta28a3))

# P8.b) ¿Qué promoción recomendaría en cada horario por la compra de un café?
#Justifique su respuesta basándose en los indicadores support, confidence y
#lift.

print(paste(
  'P2.8b) Excluyendo los registros con la etiqueta NONE en los ítems y',
  'considerando los valores anteriormente obtenidos, podría recomendar, para',
  'el horario entre las 07:00 y las 11:59 horas, la asociación de Coffe con',
  'Pastry pues, si bien no tiene el lift más alto, éste sigue siendo mayor a',
  '1, presentando además mayor support y confidence. Para el horario entre las',
  '12:00 y las 16:59 horas podría recomendar la asociación Coffe y Cake, por',
  'razones similares a las presentadas en el bloque anterior. Finalmente, para',
  'el horario entre las 17:00 y las 23:59 horas, el candidato natural es la',
  'asociación entre Coffe y Cake, pues presenta los mejores support,',
  'confidence y lift del bloque.'
))
