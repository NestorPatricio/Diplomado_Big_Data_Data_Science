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
  "ggsci",
  "chilemapas",
  "patchwork"
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

agrega_registros_con_cero <- function(nombre, sexo, base = datos2) {
  # Completa el dataframe con valores 0 si no existen registros para un nombre
  anios <- unique(base[["anio"]])
  base_auxiliar <- tibble()

  for (anio in anios) {
    if (
      nrow(
        base[
          base$anio == anio &
            base$nombre == nombre &
            base$sexo == sexo,
        ]
      ) == 0
    ) {
      base_auxiliar <- bind_rows(base_auxiliar, tibble(
        anio = anio,
        nombre = nombre,
        sexo = sexo,
        n = 0,
        proporcion = 0.0
      ))
    }
  }

  return(bind_rows(base, base_auxiliar))
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

# Describa cada una de las 3 columnas creadas y las funciones requeridas para
#la obtención de las mismas.
cat(
  "La columna 'fecha_hora', como su nombre lo indica, entrega la fecha y la ",
  "hora a la que se realizó un registro, dejándola como clase 'datetime' ",
  "gracias a la función 'as_datetime()'. La información la obtiene desde la ",
  "columna 'timestamp' que guarda el dato como un número entero que marca el ",
  "tiempo UNIX (el número de milisegundos desde el 1 de enero de 1970 a la ",
  "medianoche).\n",
  "La columna 'hora2' guarda la hora en formato 'hora:minuto:segundo', mucho ",
  "más legible para las personas que el número entero que se guarda en la ",
  "columna 'hora'. La clase en la que almacena la información es 'time' y se ",
  "consiguió aplicando la función 'as_hms()' al dato proveniente de la columna",
  " 'fecha_hora'.\n",
  "Finalmente, la columna 'horario' define 4 categorías para la información ",
  "que proviene desde 'hora2': el dato cuantitativo de las horas se agrupó en ",
  "4 categorías cualitativas fácilmente reconocibles para las personas ",
  "mediante la función 'case_when()': el tiempo entre las 0:00:00 y las ",
  "6:00:00 horas corresponde a la 'Madrugada'; entre las 6:00:01 y las ",
  "12:00:00 horas corresponde a la 'Mañana'; la 'Tarde' es entre las 12:00:01 ",
  "y las 19:00:00 horas (el intervalo más largo); y la 'Noche' se define ",
  "entre las 19:00:01 y las 24:00:00 horas (el intervalo más corto). La clase ",
  "en la cual se guardó la información es de tipo 'factor', lo que facilita su",
  " tratamiento por parte de 'R', en vez de tenerlo como una cadena de ",
  "caracteres.",
  sep = ""
)


# Ítem 1 ------------------------------------------------------------------

grafico_1a <- ggplot(data = datos) +
  geom_histogram(
    mapping = aes(x = hora2),
    bins = 96,
    fill = viridis(n = 96, direction = -1)
  ) +
  labs(
    x = "Hora",
    y = "Cantidad de observaciones",
    title = "Observaciones de GPS de teléfonos móviles por hora (G1)",
    subtitle = "Área metropolitana de Santiago, 18 de marzo de 2021"
  ) +
  scale_x_time(
    breaks = as_hms(seq(0, 86400, 21600)),
    labels = as_hms(seq(0, 86400, 21600))
  ) +
  scale_y_continuous(labels = formateador) +
  theme_light()
grafico_1a

# Describa el gráfico obtenido en función de marcas y señales.
cat(
  "El gráfico es un histograma en el que se muestra la cantidad de registros",
  "provenientes del GPS de teléfonos móviles durante las 24 horas del 18 de",
  "marzo de 2021. Las marcas corresponden a barras de distinta altura, pero",
  "con un ancho fijo que presenta intervalos de 15 minutos. Como señal",
  "principal está la altura para identificar el número de observaciones de GPS",
  "durante los 15 minutos; por otro lado, el color de cada barra va en",
  "gradiente para señalar el paso del tiempo."
)

# ¿Para qué sirve este tipo de visualización (why?)?
cat(
  "Esta gráfico sirve para identificar las horas de mayor uso del sistema GPS ",
  "de los teléfonos móviles. Con esta información se podría planificar el uso ",
  "eficiente de los recursos de la red en función del horario para este ",
  "territorio en específico.\n",
  "Llama la atención que la distribución de registros tiene una sima alrededor",
  " de las 7 de la mañana, con un plateau máximo que, aproximadamente, va ",
  "desde las 11:00 hasta las 21:00 horas. La distribución se ve mejor si, en ",
  "vez de iniciar a la medianoche, el recuento de horas parte desde las 7:00 ",
  "horas. El gráfico con el eje horario (eje X) modificado se puede observar ",
  "en la variable 'grafico_1b'.",
  sep = ""
)

# Se modifican los datos para cambiar la escala de la hora.
# Se suma 1 día (86.400 segundos) a los registros anteriores a las 7:00 horas.
datos_modificados <- datos %>%
  mutate(
    hora = ifelse(hora2 < hm("7:00"), hora + 86400, hora)
  )

grafico_1b <- ggplot(data = datos_modificados) +
  geom_histogram(
    mapping = aes(x = hora),
    bins = 96,
    fill = viridis(n = 96, direction = -1, option = "C")
  ) +
  labs(
    x = "Hora",
    y = "Cantidad de observaciones",
    title = "Observaciones de GPS de teléfonos móviles por hora (G2)",
    subtitle = "Área metropolitana de Santiago, 18 de marzo de 2021"
  ) +
  scale_x_time(
    breaks = as_hms(seq(25200, 111600, 21600)),
    labels = as_hms(c(seq(25200, 86400, 21600), seq(0, 25200, 21600)))
  ) +
  scale_y_continuous(labels = formateador) +
  theme_light()
grafico_1b


# Ítem 2 ------------------------------------------------------------------

datos_agrupados <- datos %>%
  group_by(comuna, horario) %>%
  summarise(registros = n())

grafico_2 <- ggplot(data = datos_agrupados) +
  geom_col(
    mapping = aes(x = horario, y = registros),
    fill = viridis(n = 148)
  ) +
  scale_y_continuous(labels = formateador) +
  facet_wrap(facets = vars(comuna), scales = "free_y", ncol = 6) +
  labs(
    x = "Horarios",
    y = "Registros de GPS de teléfonos móviles",
    title = "Cantidad de registros de GPS desde teléfonos móviles por horario",
    subtitle = "Santiago, 18 de marzo de 2021"
  ) +
  theme_minimal()
grafico_2

# Comente lo observado en el gráfico, mencionando comunas que rompan el patrón.
cat(
  "Lo que se observa en la gráfica son la cantidad de registros de GPS ",
  "generados desde teléfonos móviles para distintas comunas de la ciudad de ",
  "Santiago de Chile en el día 18 de marzo de 2021. Como marca se usaron ",
  "barras de distintas alturas que indican la cantidad de registros dentro del",
  " bloque horario. Como canales tenemos la altura de la barra, la posición en",
  " el eje X que indica a cual horario pertenece, además del color y la faceta",
  " que diferencian las distintas comunas de Santiago. Para el eje vertical se",
  " decidió dejar las escalas libres, de forma de mejorar la comparación entre",
  " los distintos gráficos y evitar que las diferencias de magnitudes ",
  "entorpezcan el análisis.\n",
  "Al mirar la información salta a la vista que en todas las comunas el ",
  "horario de tarde es cuando más registros se hacen, siendo la noche el ",
  "horario en segundo lugar para casi todas las comunas. Sin embargo, se puede",
  " observar que se forman 2 grupos de comunas de a cuerdo a si se producen ",
  "más registros en la madrugada o en la mañana: en 17 de las 37 comunas se ",
  "aprecia mayor actividad en la madrugada que en la mañana, llegando en el ",
  "caso de Cerro Navia a ser el segundo horario con más registros; para las ",
  "otras 20 comunas la mañana presenta más registros que la madrugada, ",
  "destacando San Miguel donde es la segunda franja horaria con más registros.",
  sep = ""
)


# Ítem 3 ------------------------------------------------------------------

dataframe_3 <- datos %>%
  group_by(device_id) %>%
  summarise(comunas = n_distinct(comuna)) %>%
  arrange(desc(comunas)) %>%
  head(n = 4)
dataframe_3


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
  scale_x_continuous(
    labels = paste0(formateador(abs(rev(seq(70.4, 70.9, 0.1)))), "º")
  ) +
  scale_y_continuous(
    labels = paste0(formateador(abs(rev(seq(33.3, 33.7, 0.1)))), "º")
  ) +
  theme_minimal()

grafico_4a <- p +
  geom_path(
    data = filter(datos, device_id %in% dataframe_3$device_id),
    mapping = aes(x = longitude, y = latitude, colour = device_id)
  ) +
  facet_wrap(facets = vars(device_id)) +
  scale_colour_lancet() +
  labs(
    x = "Longitud oeste",
    y = "Latitud sur",
    title = "Recorrido de 4 dispositivos móviles",
    subtitle = "Santiago, 18 de marzo de 2021",
    colour = "Dispositivos"
  ) +
  theme(strip.text = element_blank())
grafico_4a

# Comente lo que se observa ¿Existe alguna ruta común entre los 4 dispositivos?
cat(
  "En el gráfico se observa el recorrido de 4 teléfonos móviles por la ciudad ",
  "de Santiago según el posicionamiento del GPS para el día 18 de marzo de ",
  "2021. Específicamente están los 4 dispositivos que mayor diversidad de ",
  "comunas presentaron ese día.\n",
  "Como marca se escogió la geometría de línea según el orden en que los ",
  "registros aparecen en el dataframe 'datos', lo que se grafica con la ",
  "función 'geom_path()'. Como marca se usaron colores distintos para cada ",
  "dispositivo y la posición en el espacio para 'dibujar' el recorrido. Además",
  ", cada dispositivo cuenta con su propia faceta, lo que mejora la capacidad ",
  "de comparación.\n",
  "Para quien conoce Santiago, son claramente reconocibles los recorridos por ",
  "algunas calles, avenidas o autopistas: la autopista Américo Vespucio, ya ",
  "sea en su recorrido completo o en un segmento importante de ella; también ",
  "es reconocible el eje Alameda - Providencia - Apoquindo que va de este a ",
  "oeste; por último, son destacables las distintas porciones de la Ruta 5, ",
  "especialmente los 2 brazos de la carretera 5 Sur, con sus entradas por el ",
  "centro y por avenida General Velásquez.",
  sep = ""
)


# Ítem 4b -----------------------------------------------------------------

# Explique ¿qué geometría usaría para mostrar la popularidad de un nombre a
#través del tiempo y por qué?
cat(
  "Considero que la mejor geometría para mostrar una tendencia en el tiempo es",
  "la geometría de línea en 2 dimensiones, donde uno de los ejes recorre el",
  "tiempo y el otro muestra la cantidad de lo que queremos medir para cada una",
  "de las unidades de tiempo con las que se trabajará. Si bien podría usarse",
  "una geometría de barras, en donde la altura de la barra sea la cantidad que",
  "se desea mostrar, las líneas permiten una mejor comparación, especialmente",
  "en el caso en que las tendencias se crucen, pues en el gráfico de barras",
  "puede que algún dato quede oculto por otras barras, mientras que las líneas",
  "no ocurre ese velamiento."
)

# En caso de que alguno de los nombres usados no tenga registros para un
#determinado año, se agregan registros con 'n' y 'proporcion' en 0, para que el
#gráfico muestre esos años correctamente.
datos2_con_ceros <- agrega_registros_con_cero(
  nombre = "Néstor",
  sexo = "M",
  base = datos2
)
datos2_con_ceros <- agrega_registros_con_cero(
  nombre = "Patricio",
  sexo = "M",
  base = datos2_con_ceros
)

grafico_4b1 <- datos2_con_ceros %>%
  filter(nombre %in% c("Néstor", "Patricio") & sexo == "M") %>%
  ggplot() +
  geom_line(
    mapping = aes(x = anio, y = n, colour = nombre)
  ) +
  scale_colour_lancet() +
  scale_x_continuous(
    breaks = seq(1920, 2021, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, 5500, 500),
    labels = formateador
  ) +
  theme_minimal() +
  labs(
    x = "Años",
    y = "Cantidad de recién nacidos registrados",
    title = "Registros de recién nacidos por año según su nombre en Chile",
    subtitle = "Registros en escala lineal",
    colour = "Nombres"
  )

grafico_4b2 <- datos2_con_ceros %>%
  filter(nombre %in% c("Néstor", "Patricio") & sexo == "M") %>%
  ggplot() +
  geom_line(
    mapping = aes(x = anio, y = n, colour = nombre)
  ) +
  scale_colour_lancet() +
  scale_x_continuous(
    breaks = seq(1920, 2021, 10)
  ) +
  scale_y_log10(
    breaks = c(0, 5, 10, 20, 50, 100, 200, 500, 1000, 1500, 2000, 2500, 3000),
    labels = formateador
  ) +
  theme_minimal() +
  labs(
    x = "Años",
    y = "Cantidad de recién nacidos registrados",
    title = "Registros de recién nacidos por año según su nombre en Chile",
    subtitle = "Registros en escala logarítmica en base 10",
    colour = "Nombres"
  )
grafico_4b1 + grafico_4b2

# ¿Ve algún patrón o comportamiento inusual en los datos?
cat(
  "Para la actividad se usaron mis dos nombres: 'Néstor' y 'Patricio'. Lo",
  "primero que destaca es que el nombre 'Patricio' tiene mucha más presencia,",
  "haciendo que la curva del nombre 'Néstor' apenas tenga cambios perceptibles",
  "usando una escala lineal. Sin mebargo se puede evidenciar una tendencia de",
  "ambos nombres a un aumento progresivo con un máximo entre 1970 y 1971. Esta",
  "tendencia conjunta se nota mucho más en el gráfico con los registros en",
  "escala logarítmica. En lo personal, llama la atención que para el año en",
  "que yo nací, 1984, ambos nombres ya estaban dejando de ser tendencia."
)


# Ítem 5 ------------------------------------------------------------------

grafico_5 <- datos2 %>%
  filter(
    nombre %in% c("Brian", "Bryan", "Braian", "Brallan", "Brayan") & sexo == "M"
  ) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = anio, y = n, colour = nombre)
  ) +
  scale_colour_lancet() +
  scale_x_continuous(
    breaks = seq(1920, 2021, 10)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1200, 200),
    labels = formateador
  ) +
  theme_minimal() +
  labs(
    x = "Años",
    y = "Cantidad de recién nacidos registrados",
    title = "Registros de recién nacidos por año según su nombre en Chile",
    colour = "Nombres"
  )
grafico_5

# ¿Existe algún patrón que pueda ser explicado por algún evento histórico?
cat(
  "En el gráfico se observa un aumento de las personas registradas con los",
  "nombres 'Bryan', 'Brayan' y 'Brian' hacia finales de la década de los",
  "noventa, siendo mayor para el nombre 'Bryan' en 1998 con 1.122 personas que",
  "fueron inscritas con ese nombre; incluso las grafías menos convencionales",
  "como 'Braian' y 'Brallan' muestran algún registros para esos años. Lo más",
  "probable es que esté relacionado a la fama del grupo de música pop",
  "'Backstreet Boys'. Los nombres de los cantantes se habrían hecho también",
  "populares, con lo que los hijos de los fans terminarían homenajeando a los",
  "miembros de la banda."
)
