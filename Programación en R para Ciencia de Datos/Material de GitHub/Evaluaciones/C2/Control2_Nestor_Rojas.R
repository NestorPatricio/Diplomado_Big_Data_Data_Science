# Control 2

#Nombre1: Néstor Patricio Rojas Ríos

# SECCIÓN 1
# Carga de archivos:
general <- read.csv(paste0(
  'https://raw.githubusercontent.com/majorquev/DBDC_202304_Programacion_en_R_',
  'para_ciencia_de_datos/main/Evaluaciones/C2/datasets/generalinfo.csv'
  ))
location <- read.csv(paste0(
  'https://raw.githubusercontent.com/majorquev/DBDC_202304_Programacion_en_R_',
  'para_ciencia_de_datos/main/Evaluaciones/C2/datasets/location.csv'
  ))

# Importación de librerías:
library(dplyr)
library(forecats)
library(ggplot2)

# PREGUNTAS 1.1
# P1a) Basándose en la tabla general, ¿cuántos restaurants (id's) distintos hay
#en total?
respuesta11a <- nrow(general)
print(paste(
  '1.1a) Basándose en la tabla general hay',
  respuesta11a,
  'restaurantes distintos.'
))

# P1b) ¿En cuántos tipos de comida diferentes se clasifican los restaurants?
respuesta11b <- length(unique(general$food_type))
print(paste(
  '1.1b) Entre todos los restaurantes de la tabla general se pueden encontrar',
  respuesta11b,
  'tipos distintos de comidas.'
))

# P1c) ¿Cuántas ciudades distintas considera el sondeo?
respuesta11c <- length(unique(location$city))
print(paste(
  '1.1c) Según la tabla location el sondeo considera',
  respuesta11c,
  'ciudades.'
))

# P1d) Indique el tipo de comida y las ciudades donde se encuentra el restaurant
#"Great Wall Restaurant".
info_total <- general %>% 
  inner_join(
    location,
    by = join_by(id_restaurant == id_rest)
  )

respuesta11d <- info_total %>% 
  filter(tolower(label) == tolower('great wall restaurant')) %>% 
  select(food_type, city)
print(paste0(
  '1.1d) El tipo de comida que se sirve en el Great Wall Restaurant es ',
  respuesta11d[1, 'food_type'],
  ' y está presente en ',
  respuesta11d[1, 'city'],
  ' y ',
  respuesta11d[2, 'city'],
  '.'
))

# P1e) ¿Cuántos restaurantes de la ciudad de San Francisco tienen calificación
#mayor o igual a 3.8 y venden comida vegetariana (vegetarian)?
respuesta11e <- info_total %>% 
  filter(
    tolower(city) == tolower('San Francisco'),
    food_type == 'vegetarian',
    review >= 3.8
  ) %>% 
  nrow()
print(paste(
  '1.1e) En la ciudad de San Francisco existen',
  respuesta11e,
  'restaurantes que sirven comida vegetariana y tienen calificación mayor',
  'o igual a 3.8.'
))

print('-----------------------------------------------------------------------')

# PREGUNTAS 1.2
# P2a) Sin considerar San Francisco ¿cuál es la ciudad con mayor cantidad de
#restaurantes sondeados?
respuesta12a <- info_total %>% 
  filter(tolower(city) != tolower('San Francisco')) %>% 
  group_by(city) %>% 
  summarise(restaurants_by_city = n()) %>% 
  arrange(desc(restaurants_by_city))
print(paste(
  '1.2a) La ciudad de',
  respuesta12a$city[1],
  'es la que tiene mayor cantidad de restaurantes sondeados con',
  respuesta12a$restaurants_by_city[1],
  'locales.'
))

# P2b) ¿Cuáles son los 3 tipos de comida ofrecido más comunes?
respuesta12b <- info_total %>% 
  group_by(food_type) %>% 
  summarise(restaurants_by_food_type = n()) %>% 
  arrange(desc(restaurants_by_food_type))
print(paste0(
  '1.2b) Los 3 tipos de comidas que más ofrecen los restaurantes son ',
  respuesta12b$food_type[1],
  ' con ',
  respuesta12b$restaurants_by_food_type[1],
  ' locales, ',
  respuesta12b$food_type[2],
  ' con ',
  respuesta12b$restaurants_by_food_type[2],
  ' locales y ',
  respuesta12b$food_type[3],
  ' con ',
  respuesta12b$restaurants_by_food_type[3],
  ' locales.'
))

# P2c) Sin considerar San Francisco ¿cuáles son las 3 ciudades con mayor
#cantidad de restaurants que ofrecen comido tipo japanese?
respuesta12c <- info_total %>% 
  filter(
    tolower(city) != tolower('San Francisco'),
    food_type == 'japanese'
  ) %>% 
  group_by(city) %>% 
  summarise(
    restaurants_by_city = n(),
    mean_review = mean(review)
  ) %>% 
  arrange(desc(restaurants_by_city))
print(paste0(
  '1.2c) Las 3 ciudades con mayor cantidad de restaurantes de comida japonesa',
  ' son ',
  respuesta12c$city[1],
  ' con ',
  respuesta12c$restaurants_by_city[1],
  ' locales, ',
  respuesta12c$city[2],
  ' con ',
  respuesta12c$restaurants_by_city[2],
  ' locales y ',
  respuesta12c$city[3],
  ' con ',
  respuesta12c$restaurants_by_city[3],
  ' locales.'
))

# P2d) Usted decide viajar a una de las ciudades en cuestión, para ello calcula
#el promedio de las valoraciones medias (promedio de review) por cada ciudad, y
#escoje aquella con mayor review promedio ¿qué ciudad escoge?
respuesta12d <- respuesta12c %>% 
  head(n = 3) %>% 
  arrange(desc(mean_review))
print(paste(
  '1.2d) Tomando las 3 ciudades seleccionadas del ejercicio anterior, escojo',
  respuesta12d$city[1],
  'pues tiene la mejor review con',
  round(respuesta12d$mean_review[1], 1),
  'puntos en promedio para todos los locales de comida japonesa.'
))

# P2e) ¿Cuál es la ciudad con mejor valoración promedio de restaurantes tipo
#"barbeque"?
respuesta12e <- info_total %>% 
  filter(tolower(food_type) == tolower('barbeque')) %>% 
  group_by(city) %>% 
  summarise(mean_review = mean(review)) %>%
  arrange(desc(mean_review))
print(paste(
  '1.2e) La ciudad que tiene la mejor valoración promedio para comida barbeque',
  'es',
  respuesta12e$city[1],
  'con',
  round(respuesta12e$mean_review[1], 1),
  'puntos.'
))

print('-----------------------------------------------------------------------')

# PREGUNTAS 1.3
# P3a) En la pregunta 1d), se pudo observar que un mismo restaurant puede estar
#presente en más de una ciudad ¿cuántos restaurants tienen esta característica,
#es decir están en más de una ciudad distinta? (Ver info complementaria).
respuesta13 <- info_total %>% 
  group_by(label, city) %>% 
  summarise(restaurants_by_name = n()) %>% 
  group_by(label) %>% 
  summarise(cities_by_restaurant = n()) %>% 
  filter(cities_by_restaurant > 1)

respuesta13a <- nrow(respuesta13)
print(paste(
  '1.3a) Del total de restaurantes de la muestra',
  respuesta13a,
  'están en más de 1 ciudad.'
))

# P3b) ¿Cuál es el restaurant que tiene presencia en la mayor cantidad de
#ciudades distintas?¿En cuántas ciudades está presente?
respuesta13b <- respuesta13 %>% 
  arrange(desc(cities_by_restaurant)) %>% 
  first()
print(paste0(
  '1.3b) El restaurante que tiene presencia en más ciudades es ',
  respuesta13b$label,
  ', con locales en ',
  respuesta13b$cities_by_restaurant,
  ' ciudades distintas.'
))

# P3c) Muestre, mediante un gráfico de barras, los 15 restaurants con mayor
#cantidad de sucursales, donde la altura de la barra representa el total de
#sucursales de cada restaurant (ver info complementaria).
respuesta13c <- info_total %>% 
  group_by(label) %>% 
  summarise(locals_by_restaurants = n()) %>% 
  arrange(desc(locals_by_restaurants)) %>% 
  head(n = 15) %>% 
  mutate(label = fct_reorder(label, locals_by_restaurants)) %>% 
  ggplot() +
  aes(
    y = label,
    x = locals_by_restaurants,
    fill = label,
    label = locals_by_restaurants
  ) +
  geom_col() +
  geom_label(
    color = '#000000',
    fill = '#FFFFFF',
    size = 4
  ) +
  labs(
    title = 'Los 15 restaurantes con mayor cantidad de sucursales.',
    x = 'Cantidad de sucursales',
    y = 'Restaurantes'
  ) +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 12)
  )
respuesta13c

print('-----------------------------------------------------------------------')

# PREGUNTAS 1.4
# P4a) Genere una tabla llamada resumen, que contenga la siguiente información:
# city: Ciudad.
# food_type: Tipo de comida.
# n_rest: Cantidad de restaurantes por cada ciudad y tipo de comida.
# review_prom: Valoración promedio por cada ciudad y tipo de comida.
# total_rest: Total de restaurantes por cada ciudad (se puede repetir el valor
#por cada tipo de comida).
# review_prom_city: Valoración promedio de los restaurantes por cada ciudad (se
#puede repetir el valor por cada tipo de comida, ver info complementaria).
auxiliar_1 <- info_total %>% 
  group_by(city, food_type) %>% 
  summarise(
    n_rest = n(),
    review_prom = mean(review, na.rm = TRUE)
  )

auxiliar_2 <- info_total %>% 
  group_by(city) %>% 
  summarise(
    n_total = n(),
    review_prom_city = mean(review, na.rm = TRUE)
  )

respuesta14a <- auxiliar_1 %>% 
  inner_join(
    auxiliar_2,
    join_by(city == city)
  ) %>% 
  arrange(desc(n_rest))
View(respuesta14a)

# P4b) Basado en la tabla anterior, construya dos nuevas columnas:
# density_food_type: Representa el cuociente entre le total de restaurants por
#tipo de comida y ciudad, respecto del total de restaurantes de la ciudad 
#(n_rest/total_rest).
# ratio_review: Representa el cociente entre a valoración del restaurant por
#tipo de comida y ciudad, respecto de la valoración promedio de los resturants
#de la misma ciudad (review_prom/review_prom_city).
respuesta14b <- respuesta14a %>% 
  mutate(
    density_food_type = n_rest / n_total,
    rate_review = review_prom / review_prom_city
  )
View(respuesta14b)

# P4c) Mediante un gráfico de dispersión, muestre la relación entre
#density_food_type y ratio_review. Investigue sobre el parámetro alpha dentro de
#la capa geométrica para una mejor visualizaciónd de los puntos. Adicionalmente
#añada una curva de tendencia y, con base en él, indique si cabe la posibilidad
#de establecer algún tipo de dependencia entre density_food_type y ratio_review.
respuesta14c <- respuesta14b %>% 
  ggplot() +
  aes(x = density_food_type, y = rate_review) +
  geom_point(alpha = 0.3, aes(colour = city)) +
  geom_smooth(method = 'lm', colour = '#FF0000') +
  labs(
    title = paste(
      'Relación entre la tasa de puntuaciones y la densidad de',
      'establecimientos, por tipo de comida y ciudad.'
    ),
    caption = paste(
      'No parece existir una relación significativa entre ambas',
      'variables.'
    )
  ) +
  theme(legend.position = 'none')
respuesta14c

print('-----------------------------------------------------------------------')

# PREGUNTAS 1.5
# P5a) En la tabla resumen creada en P4a), genere una nueva columna llamada
#type_review, que contenga "review alto", si ratio_review >= 1 y "review bajo"
#ratio_review < 1 ¿qué indica esta variable? Comente.
respuesta15a <- respuesta14b %>% 
  mutate(type_review = ifelse(rate_review < 1, 'review bajo', 'review alto'))
View(respuesta15a)

print(paste(
  "1.5a) La variable 'type_review' indica cuándo el promedio de la puntuación",
  "obtenida por las revisiones de los restaurantes de un determinado tipo de",
  "cocina está por sobre el promedio de todos los restaurantes de la ciudad",
  "para dicha puntuación. Podría tomarse como un indicador de calidad del",
  "tipo de preparaciones que se hacen en una determinada ciudad."
))

# P5b) Para cada type_review, muestre a través de un gráfico de cajas (boxplot),
#la distribución de la densidad del tipo de comida density_food_type. ¿Qué puede
#observar?
respuesta15b <- respuesta15a %>% 
  ggplot() +
  aes(x = type_review, y = density_food_type, colour = type_review) + 
  geom_boxplot() +
  theme(legend.position = 'none')
respuesta15b

print(paste(
  ''
))