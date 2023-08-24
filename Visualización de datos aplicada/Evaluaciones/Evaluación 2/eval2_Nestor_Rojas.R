# Evaluación 2
# Visualización de datos aplicada
# Néstor Patricio Rojas Ríos


# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "tibble",
  "lubridate",
  "hms",
  "lintr",
  "data.table",
  "viridis",
  "chilemapas"
)
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Configuración del directorio de trabajo
# Trabajo con Linux y Windows, dependiendo de las circusntancias
if (Sys.info()["sysname"] == "Windows") {
  setwd(paste0(
    "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\",
    "Visualización de datos aplicada\\Evaluaciones\\Evaluación 2"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Visualización ",
    "de datos aplicada/Evaluaciones/Evaluación 2/"
  ))
}

# Se ejecuta el linter de R para evaluar el estilo del script
lint("eval2_Nestor_Rojas.R")


# Carga de datos necesarios -----------------------------------------------

# Datos 1
datos <- fread(
  input = "https://github.com/jbkunst/random-data/raw/main/movilidad.gz"
)
datos <- as_tibble(x = datos)
glimpse(x = datos)

# Datos 2
datos2 <- read.csv(
  file = paste0(
    "https://raw.githubusercontent.com/rivaquiroga/guaguas/main/data-raw/",
    "1920-2021.csv"
  )
)
glimpse(datos2)

# Funciones ---------------------------------------------------------------

formateador <- function(numero) {
  # Define el punto para separación de miles y la coma para separación decimal
  return(
    format(x = numero, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  )
}

# Ítem 0 ------------------------------------------------------------------

datos <- datos %>%
  arrange(device_id, timestamp) %>%
  mutate(
    fecha_hora = as_datetime(timestamp / 1000),
    hora2 = as_hms(fecha_hora),
    horario = case_when(
      hora2 < hm("6:00") ~ "Madrugada",
      hora2 < hm("12:00") ~ "Mañana",
      hora2 < hm("19:00") ~ "Tarde",
      hora2 < hm("24:00") ~ "Noche"
    ),
    horario = factor(
      x = horario,
      levels = c("Madrugada", "Mañana", "Tarde", "Noche")
    )
  )
glimpse(x = datos)

# Describa cada una dee las 3 columnas creadas y las funciones requeridas para
#la obtención de las mismas.
cat(
  "Bla, bla, bla..."
)


# Ítem 1 ------------------------------------------------------------------

grafico_1 <- ggplot(data = datos) +
  geom_histogram(
    mapping = aes(x = hora2),
    bins = 96,
    fill = viridis(n = 96, direction = -1)
  ) +
  labs(
    x = "Hora",
    y = "Cantidad de observaciones",
    title = "Observaciones de GPS de teléfonos móviles por hora",
    subtitle = "Área metropolitana de Santiago"
  ) +
  scale_x_time(
    breaks = hm(c("0:00", "6:00", "12:00", "19:00", "24:00"))
  ) +
  scale_y_continuous(labels = formateador) +
  theme_light()
grafico_1

# Describa el gráfico obtenido en función de marcas y señales.
cat(
  "Bla, bla, bla..."
)

# ¿Para qué sirve este tipo de visualización?
cat(
  "Bla, bla, bla..."
)


# Ítem 2 ------------------------------------------------------------------

datos2 <- datos %>%
  group_by(comuna, horario) %>%
  summarise(registros = n())

grafico2 <- ggplot(data = datos2) +
  geom_col(
    mapping = aes(x = horario, y = registros),
    fill = viridis(n = 148)
  ) +
  scale_y_continuous(labels = formateador) +
  facet_wrap(facets = vars(comuna)) +
  labs(
    x = "Horarios",
    y = "Registros de GPS de teléfonos móviles"
  ) +
  theme_light()
grafico2

# Comente lo observado en el gráfico, mencionando comunas que rompan el patrón.
cat(
  "Bla, bla, bla..."
)


# Ítem 3 ------------------------------------------------------------------

dataframe3 <- datos %>%
  group_by(device_id) %>%
  summarise(comunas = n_distinct(comuna)) %>%
  arrange(desc(comunas)) %>%
  head(n = 4)
dataframe3


# Ítem 4a -----------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = filter(mapa_zonas, codigo_region == "13"),
    mapping = aes(geometry = geometry),
    fill = "gray95",
    colour = "gray80",
    alpha = 0.5,
    size = 0.3
  ) +
  coord_sf(xlim = c(-70.9, -70.4), ylim = c(-33.3, -33.7)) +
  theme_minimal()

grafico4a <- p +
  geom_path(
    data = filter(datos, device_id %in% dataframe3$device_id),
    mapping = aes(x = longitude, y = latitude, colour = device_id)
  ) +
  facet_wrap(facets = vars(device_id)) +
  scale_colour_viridis_d() +
  labs(
    x = "Longitud oeste",
    y = "Latitud sur",
    colour = "Dispositivos"
  ) +
  theme(
    legend.position = "none"
  )
grafico4a

# Comente lo que se observa ¿Existe alguna ruta común entre los 4 dispositivos?
cat(
  "Bla, bla, bla..."
)


# Ítem 4b -----------------------------------------------------------------

# Explique ¿qué geometría usaría para mostrar la popularidad de un nombre a
#través del tiempo y por qué?
cat(
  "Considero que la mejor geometría para mostrar una tendencia en el tiempo es",
  "la geometría de línea en 2 dimensiones, donde uno de los ejes recorre el",
  "tiempo y el otro muestra la cantidad para cada una de las unidades de",
  "tiempo con las que se trabajará. Si bien podría usarse una geometría de",
  "barras, en donde la altura de la barra sea la cantidad que se desea evaluar",
  "las líneas permiten una mejor comparación, especialmente en el caso en que",
  "las tendencias se crucen, pues en el gráfico de barras puede que algún dato",
  "quede oculto por otras barras, mientras que las líneas no se solapan."
)

datos2 %>%
  filter(nombre %in% c("Néstor", "Nestor", "Patricio")) %>%
  group_by(anio)
# Pendiente de continuar
