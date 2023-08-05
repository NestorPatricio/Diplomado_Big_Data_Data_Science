# Evaluación 1
# Visualización de datos aplicada
# Néstor Patricio Rojas Ríos


# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c("tidyverse", "lintr", "patchwork", "datos", "mapproj", "viridis", "lubridate")
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Se configura el directori de trabajo
setwd(paste0(
  "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Visualización de",
  " datos aplicada/Evaluaciones/Evaluación 1/"
))

# Se ejecuta el linter para evaluar el estilo del script
lint("eval1_Nestor_Rojas.R")


# Datos necesarios --------------------------------------------------------

df <- tibble(
  x = c(2, 1, 3, 5, 6, 4),
  y = c(2, 4, 6, 7, 4, 1),
  v = c(1, 1, 1, 2, 2, 2)
)

estados <- as_tibble(map_data("state"))

topdestinos <- vuelos %>%
  count(codigo_aeropuerto = destino, sort = TRUE) %>%
  left_join(
    y = aeropuertos %>% select(codigo_aeropuerto, latitud, longitud),
    by = join_by(codigo_aeropuerto)
  ) %>%
  head(20)


# Ítem 1 ------------------------------------------------------------------

graficos_1 <- ggplot(data = df, aes(x = x, y = y))
grafico_1_1 <- graficos_1 + geom_line()
grafico_1_2 <- graficos_1 + geom_path()
grafico_1_3 <- graficos_1 + geom_polygon()

grafico_1_1 + grafico_1_2 + grafico_1_3

# Describa qué hace geom_path() a diferencia de geom_line().
# Mencione qué hace geom_path() a diferencia de geom_line().
print(paste0(
  ""
))


# Ítem 2 ------------------------------------------------------------------

graficos_2 <- ggplot(data = df, aes(x = x, y = y, group = v))
grafico_2_1 <- graficos_2 + geom_line()
grafico_2_2 <- graficos_2 + geom_path()
grafico_2_3 <- graficos_2 + geom_polygon()

grafico_2_1 + grafico_2_2 + grafico_2_3

# Menciona los cambios obtenidos respecto a los resultados anteriormente obtenidos.
# ¿Cuál sería un tipo de datos donde geom_path() sea la función a utilizar sobre geom_line()?
print(paste0(
  ""
))


# Ítem 3 ------------------------------------------------------------------

grafico_3 <- ggplot(estados) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    col = "gray70",
    fill = "gray90"
  ) +
  labs(
    x = "Longitud Oeste",
    y = "Latitud Norte"
  ) +
  theme_minimal()
grafico_3


# Ítem 4 ------------------------------------------------------------------

grafico_4 <- grafico_3 +
  geom_point(
    data = topdestinos,
    mapping = aes(x = longitud, y = latitud,  color = n),
    alpha = 0.7,
    size = 2
  ) +
  scale_color_viridis() +
  labs(
    color = "Cantidad de\nvuelos arribados"
  ) + coord_map()
grafico_4

# Argumente la elección del tipo de canal que ha utilizado para representar la cantidad de vuelos.
# Revise por qué es relevante el uso de coord_map() y comente lo obtenido en el gráfico.
print(paste0(
  ""
))


# Ítem 5 ------------------------------------------------------------------

# Análisis de la posibilidad de inducir error en el caso expuesto, comparando:
#los atributos (variables) usados
#las marcas
#los canales
#qué información se codifica
#Cómo se codifica la información
# ¿De qué forma se puede mejorar el gráfico de la izquierda (territorio)?
# https://www.core77.com/posts/90771/A-Great-Example-of-Better-Data-Visualization-This-Voting-Map-GIF
print(paste0(
  ""
))


# Ítem 6 ------------------------------------------------------------------

colores <- c("#0015BC", "#DE0100")
presidencial_arreglado <- presidencial %>%
  mutate(
    anos_ejercicio = as.double(fin - inicio) / 365.25,
    anos_ejercicio = ifelse(partido == "Demócrata", anos_ejercicio * - 1, anos_ejercicio)
  )
presidencial_arreglado[8, "nombre"] <- "G.H.W. Bush"
presidencial_arreglado[10, "nombre"] <- "G.W. Bush"

grafico_presidentes <- ggplot(
  data = presidencial_arreglado,
  mapping = aes(
    y = reorder(nombre, inicio),
    x = anos_ejercicio,
    fill = partido,
    label = nombre
  )
) +
  geom_col() +
  scale_fill_manual(values = colores) +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  geom_label(
    color = '#000000',
    fill = '#FFFFFF'
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    #line = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Años de ejercicio",
    y = element_blank(),
    fill = "Partido"
  )
grafico_presidentes

partidos <- presidencial_arreglado %>%
  group_by(partido) %>%
  summarise(anos_ejercicio = sum(anos_ejercicio)) %>%
  mutate(anos_ejercicio = abs(anos_ejercicio))

grafico_partidos <- ggplot(
  data = partidos,
  mapping = aes(x = "", y = anos_ejercicio, fill = partido)
) +
  geom_bar(stat = "identity", width = 1, colour = "#FFFFFF") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = colores) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    line = element_blank()
  ) +
  labs(
    x = element_blank(),
    y = element_blank(),
    fill = "Partido"
  )

grafico_presidentes + grafico_partidos

# Explique el WHAT, HOW and WHY.
# Qué es lo que está codificando en términos de marcas y canales.
print(paste0(
  ""
))
