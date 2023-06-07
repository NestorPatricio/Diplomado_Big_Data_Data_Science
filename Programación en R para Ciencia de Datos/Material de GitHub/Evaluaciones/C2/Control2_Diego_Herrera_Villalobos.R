# Preliminar: Creación de las variables

general <- generalinfo
location <- location

# Pregunta 1a. Basándose en la tabla general, ¿cuántos restaurants (id's) 
# distintos hay en total?

library(dplyr)

general %>% distinct(id_restaurant) %>% n_distinct()

# Respuesta: Hay 9590 id´s restaurante distintos

# Pregunta 1b. ¿En cuántos tipos de comida diferentes se clasifican los restaurants?

general %>% distinct(food_type) %>% n_distinct()

# Respuesta: Hay 145 tipos de comida diferentes en los cuales 
# se clasifican los restaurantes


# Pregunta 1c. ¿Cuántas ciudades distintas considera el sondeo?

# paso 1, consolidar la tabla que contiene el sondeo (review) y 
# la tabla que contiene las ciudades (city)
join_variables <- left_join(x = general, y = location, by = c("id_restaurant" = "id_rest"))

# paso 2, calcular las ciudades distintas 

join_variables %>% distinct(city) %>% n_distinct()

# Respuesta: Hay 167 ciudades distintas en el sondeo

# Pregunta 1d. ¿Indique el tipo de comida y las ciudades donde se encuentra 
# el restaurant "great wall restaurant"?

join_variables %>% filter(label == "great wall restaurant" ) %>%
  select(food_type, city)

# Respuesta: el restaurant great wall se encuentra en las ciudades 
# San Francisco y San Leandro, y el tipo de comida que ofrece es chinese

# Pregunta 1e. ¿Cuántos restaurantes de la ciudad de san francisco tienen 
# calificación mayor o igual a 3.8 y venden comida vegetariana (vegetarian)?

join_variables %>% filter(city == "san francisco" & review >= 3.8 & food_type == "vegetarian") %>% nrow()

# Respuesta: 3 restaurantes

# Pregunta 2a. Sin considerar San Francisco, ¿cuál es la ciudad con mayor 
# cantidad de restaurantes sondeados?

join_variables %>% filter(city != "san francisco") %>%
  group_by(city) %>%
  summarise(numero_restaurantes = n()) %>%
  slice_max(order_by = numero_restaurantes)

# Respuesta: la ciudad con mayor restaurantes sondeados 
# es San Jose con 933 restaurantes


# Pregunta 2b. ¿Cuáles son los 3 tipos de comida ofrecido más comunes?

join_variables %>% group_by(food_type) %>%
  summarise(frecuencia = n()) %>%
  top_n(3, wt=frecuencia)

# Respuesta: los 3 tipos de comida mas comunes son cafe, chinese y pizza.

# Pregunta 2c. Sin considerar San Francisco, ¿Cuáles son las 3 ciudades 
# con mayor cantidad de restaurants que ofrecen comida tipo japanese?

join_variables %>% filter(city != "san francisco" & food_type == "japanese") %>%
  group_by(city) %>%
  summarise(cantidad_restaurantes = n()) %>%
  top_n(3,wt=cantidad_restaurantes)

# Respuesta: Las 3 ciudades son Berkeley, Oakland y San Jose.

# Pregunta 2d. Usted decide viajar a una de las ciudades en cuestión, 
# para ello calcula el promedio de las valoraciones medias (promedio de review) 
# por cada ciudad, y escoje aquella con mayor review promedio. ¿Qué ciudad escoge?

join_variables %>%
  filter(city != "san francisco" & food_type == "japanese") %>%
  group_by(city) %>%
  summarise(cantidad_restaurantes = n(), promedio_reviews = mean(review)) %>%
  top_n(3, wt = cantidad_restaurantes)

# Respuesta: De las 3 ciudades en cuestion, escogería la ciudad de San José 
# por tener el promedio de reviews más alto de 2.52

# Pregunta 2e. Cuál es la ciudad con mejor valoración promedio 
# de restaurantes tipo "barbeque"

join_variables %>% filter(food_type == "barbeque") %>%
  group_by(city) %>%
  summarise(valoracion = mean(review)) %>%
  slice_max(order_by = valoracion)

# Respuesta: La ciudad es Pleasant Hill con 3.7

# Pregunta 3a. En la pregunta 1d), se pudo observar que un mismo restaurante
# puede estar presente en más de una ciudad. 
# ¿Cuántos restaurants tienen esta característica, es decir, 
# están en más de una ciudad distinta ? 
# De ser de utilidad puede investigar y utilizar la función distinct().

join_variables %>%
  group_by(label) %>%
  summarise(ciudades = n_distinct(city)) %>%
  filter(ciudades > 1) %>%
  nrow()

# Respuesta: 559 restaurantes están en mas de una ciudad.

# Pregunta 3b ¿Cuál es el restaurant que tiene presencia en
# la mayor cantidad de ciudades distintas?
# ¿En cuántas ciudades está presente?

restaurantes_ciudades <- join_variables %>%
  group_by(label) %>%
  summarise(ciudades_distintas = n_distinct(city)) %>%
  arrange(desc(ciudades_distintas))

restaurante_cant_ciudades <- restaurantes_ciudades %>%
  filter(ciudades_distintas == max(ciudades_distintas))

print(restaurante_cant_ciudades)

# Respuesta: el restaurante de mayor presencia es Baskin Robbins
# y está presente en 49 ciudades.

# Pregunta 3c. Muestre, mediante un gráfico de barras, 
# los 15 restaurantes con mayor cantidad de sucursales, donde la altura de la barra
# representa el total de sucursales de cada restaurante.

library(ggplot2)

# paso 1 Ordenar los datos por total sucursales
datos_grafico <- join_variables %>%
  count(label, name = "Total_Sucursales") %>% 
  group_by(label)%>%
  top_n(15,wt = Total_Sucursales)

#paso 2 Identificar los top 15 restaurantes
top_15 <- datos_grafico %>%
  arrange(desc(Total_Sucursales)) %>%
  head(15)

# Paso 3 Graficar

top_15 %>%
  ggplot() +
  aes(x = reorder(label, -Total_Sucursales), y = Total_Sucursales, color = label, fill = label) +
  labs(x = "Restaurante", y = "Cantidad de Sucursales") +
  ggtitle("Los 15 restaurantes con mayor cantidad de sucursales") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_label(mapping = aes(label = Total_Sucursales), color = "black", position = position_dodge(width = 0.9)) +
  guides(fill = FALSE, color = FALSE)

# Pregunta 4.a Genere una tabla llamada resumen, que contenga 
# la siguiente información: city, food_type, n_rest, review_prom, total_rest, review_prom_city.

# paso 1 crear las columnas solicitadas:

# n_rest.
join_variables <- join_variables %>%
  group_by(city, food_type) %>%
  mutate(n_rest = n())

# review_prom
join_variables <- join_variables %>%
  group_by(city, food_type) %>%
  mutate(review_prom = mean(review))

# total_rest
join_variables <- join_variables %>%
  group_by(city) %>%
  mutate(total_rest = n())

# review_prom_city

join_variables <- join_variables %>%
  group_by(city) %>%
  mutate(review_prom_city = mean(review))


# paso 2 crear la tabla resumen

resumen <- select(join_variables, city, food_type, n_rest, review_prom, total_rest, review_prom_city)

# Pregunta 4b. Construya 2 nuevas columnas a la tabla resumen

resumen <- resumen %>%
    mutate(density_food_type = n_rest/total_rest, ratio_review = review_prom/review_prom_city)

# Pregunta 4c Mediante un gráfico de dispersión, muestre la relación entre 
# density_food_type y ratio_review

ggplot(resumen, aes(x = density_food_type, y = ratio_review)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Density Food Type", y = "Ratio Review") +
  ggtitle("Relación entre Density Food Type y Ratio Review")


# Respuesta: Se observa una curva con una pendiente negativa. 
# ya que a medida que los valores en el eje X aumentan (density food type)
# los valores en el eje Y (ratio review) tienden a disminuir, por lo tanto,
# existe una relación inversa entre ambas variables

# Pregunta 5a En la tabla resumen creada en P4a), 
# genere una nueva columna llamada type_review, que contenga "review alto"
# si ratio_review >= 1 y "review bajo" ratio_review < 1. 
# ¿Qué indica esta variable? Comente.

resumen <- resumen %>%
  mutate(type_review = ifelse(ratio_review >= 1, "review alto", "review bajo"))


# Respuesta:

# El ratio_review proporciona una medida relativa de la valoración
# de un restaurante en comparación con la ciudad en la que se encuentra.

# Esto significa que:

# si el ratio_review es mayor a 1, significa que la valoración promedio
# del restaurante es más alta que la valoración promedio de la ciudad. 
# Esto indica que el restaurante tiene una valoración relativamente alta 
# en comparación con otros restaurantes en la misma ciudad.

# Por otro lado, si el ratio_review es menor a 1, significa que
# la valoración promedio del restaurante es más baja que la valoración promedio
# de la ciudad. Esto indica que el restaurante tiene una valoración
# relativamente baja en comparación con otros restaurantes en la misma ciudad.

# Pregunta 5b Para cada type_review, muestre a través de un gráfico de cajas
# (boxplot), la distribución de la densidad del tipo de comida 
# density_food_type. ¿Qué puede observar?

ggplot(resumen, aes(x = type_review, y = density_food_type)) +
  geom_boxplot() +
  labs(x = "Type Review", y = "Density Food Type") +
  ggtitle("Distribución de Densidad Tipo de Comida por Tipo de Review")


# Respuesta: se observa que las cajas están achatadas, por lo que hay menos 
# dispersión entre los datos y hay mas concentración o densidad entre ellos.
