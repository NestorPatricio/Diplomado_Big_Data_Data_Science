# Evaluación 1
# Visualización de datos aplicada
# Néstor Patricio Rojas Ríos


# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "tibble",
  "lintr",
  "datos",
  "viridis",
  "showtext",
  "maps",
  "mapproj"
)
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Se configura el directorio de trabajo
if (Sys.info()["sysname"] == "Windows") {
  setwd(paste0(
    "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\",
    "Visualización de datos aplicada\\Evaluaciones\\Evaluación 1"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Visualización ",
    "de datos aplicada/Evaluaciones/Evaluación 1/"
  ))
}

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

graficos_1 <- ggplot(data = df, mapping = aes(x = x, y = y))
grafico_1_1 <- graficos_1 + geom_line() + labs(title = "Con geom_line()")
grafico_1_2 <- graficos_1 + geom_path() + labs(title = "Con geom_path()")
grafico_1_3 <- graficos_1 + geom_polygon() + labs(title = "Con geom_polygon()")

grafico_1_1
grafico_1_2
grafico_1_3

# Describa qué hace geom_path() a diferencia de geom_line().
cat(
  "Tanto geom_path() como geom_line() son funciones que grafican una línea que",
  "pasa por todos los puntos de un conjunto de datos (dataframe, tibble,",
  "matriz, etc). La diferencia radica en que geom_line(), como si fuese una",
  "función, sigue el orden de uno de los ejes, generalmente el eje X; en",
  "cambio, geom_path() sigue el orden en el cual los datos aparecen en el set",
  "de datos."
)

# Qué hace geom_polygon() a diferencia de geom_path().
cat(
  "Por otra parte, geom_polygon() sigue todos los puntos del set de datos de",
  "igual forma que geom_path(), pero agrega el relleno dentro del polígono",
  "que se genera, con lo que podemos graficar una figura geométrica que ayuda",
  "a visualizar mejor los datos que poseen un componente espacial."
)


# Ítem 2 ------------------------------------------------------------------

graficos_2 <- ggplot(data = df, mapping = aes(x = x, y = y, group = v))
grafico_2_1 <- graficos_2 + geom_line() + labs(title = "Con geom_line()")
grafico_2_2 <- graficos_2 + geom_path() + labs(title = "Con geom_path()")
grafico_2_3 <- graficos_2 + geom_polygon() + labs(title = "Con geom_polygon()")

grafico_2_1
grafico_2_2
grafico_2_3

# Menciona los cambios obtenidos respecto a los resultados del Ítem 1.
cat(
  "El argumento group dentro de la función aes() permite hacer grupos de datos",
  "independientes en su trato, por lo que se generarán tantos gráficos como",
  "fatores existan en el atributo o variable que se establezca para group,",
  "siempre en la misma área de trabajo, en la misma trama (a diferencia de las",
  "facetas que generan cada una su propia trama para cada valor categórico).",
  "En este ítem la variable v tiene 2 valores distintos, por lo tanto los",
  "datos son agrupados en 2 conjuntos independientes, obviando la conexión",
  "entre los puntos que hacen el salto de una categorìa a otra."
)

# ¿Cuál sería un tipo de datos donde geom_path() sea la función a utilizar sobre
#geom_line()?
cat(
  "Los datos en los cuales usar la función geom_path() es más provechoso es en",
  "aquellos en que la información espacial es relevante, pues puede llevar a",
  "una representación en dos dimensiones que sea más explicativa que un trazo",
  "unidmensional. Por esto geom_path() es la función perfecta si es que",
  "queremos dibujar, por ejemplo, una capa de mapas de un territorio o una",
  "representación anatómica sobre la cual queramos destacar algunos puntos con",
  "alguna información relevante."
)


# Ítem 3 ------------------------------------------------------------------

grafico_3 <- ggplot(estados) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    col = "gray70",
    fill = "gray90"
  ) +
  labs(
    title = "Los 20 aeropuertos con más vuelos arribados en Estados Unidos",
    x = "Longitud Oeste",
    y = "Latitud Norte"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
grafico_3


# Ítem 4 ------------------------------------------------------------------

grafico_4 <- grafico_3 +
  geom_point(
    data = topdestinos,
    mapping = aes(x = longitud, y = latitud, size = n, color = n),
    alpha = 0.5
  ) +
  scale_color_viridis() +
  labs(color = "Cantidad de\nvuelos arribados") +
  guides(size = "none") +
  coord_map(projection = "azequalarea")
grafico_4

# Argumente la elección del tipo de canal que ha utilizado para representar la
#cantidad de vuelos.
cat(
  "Para indicar la cantidad de vuelos he decidio hacer uso de 2 canales que",
  "expresan magnitud: un gradiente de color y un gradiente de tamaño. La",
  "elección se basa en el tipo de información que se quiere transmitir (datos",
  "numéricos contínuos), para lo cual la longitud comparada es muy buena, pero",
  "no se encuentra disponible dado la restricción de usar la posición en el",
  "espacio como coordenadas geográficas. En ese sentido, tanto el color como",
  "el tamaño son más débiles para comparar magnitud, pero al combinarlos se",
  "obtiene más fuerza para expresar las diferencias en la cantidad de vuelos",
  "que recibe cada aeropuerto."
)

# Revise por qué es relevante el uso de coord_map() y comente lo obtenido.
cat(
  "La función coord_map() permite generar representaciones de la superficie",
  "terrestre, o de regiones de esta, de acuerdo a algunas de las proyecciones",
  "más populares de la cartografía (como la de Mercator, la cual usa por",
  "defecto). Esto posibilita que las representaciones gráficas de territorios",
  "que queramos generar mediante ggplot2 tengan la proporción correcta según",
  "la proyección escogida, no quedando esta al arbitrio del tamaño de la",
  "ventana en la cual se muestra. En este ejercicio usé la proyección",
  "'azequalarea' pues prefiero una proyección que sea más fiel a la real",
  "dimensión del territorio; acá las líneas de los meridianos tienden a",
  "conlfuir hacia el polo más cercano, el polo norte en este caso, con el",
  "consecuente estrechamiento del mapa arriba y la transformación de las",
  "líneas rectas de los paralelos en líneas curvas paralelas."
)


# Ítem 5 ------------------------------------------------------------------

# Análisis de la posibilidad de inducir error en el caso expuesto.
cat(
  "Los gráficos presentados son un ejemplo de lo importante que es tener en ",
  "mente un marco teórico adecuado al momento de generar una visualización de ",
  "datos ¿qué información es la que se está exponiendo? De ambos gráficos se ",
  "puede extraer todos los condados de todo Estados Unidos (dato categórico) y",
  " el partido político que ganó en dicho condado para la elección ",
  "presidencial de 2016 (también un dato categórico). La diferencia radica en ",
  "el dato cuantitativo que muestra cada uno: a la izquierda se aprecia la ",
  "superficie que tiene cada condado, mientras que en el de la derecha se ",
  "puede observar la cantidad de población que vive ahí.\n",

  "¿Por qué se habrá querido hacer esta visualización? La intención parece ser",
  " la misma en ambos casos: una consulta para comparar cuál fue el partido ",
  "más votado en el condado, el Partido Republicano o el Partido Demócrata, y ",
  "así hacerse una idea de qué candidato fue el vencedor.\n",

  "¿Cómo se realizó la visualización de los datos? Para ambos gráficos se ",
  "utilizaron como marcas la superficie de figuras geométricas para el dato ",
  "cuantitativo, siguiendo el área del condado en el primer caso y el tamaño ",
  "de una circunferencia en el segundo caso. En cuanto a los canales, para ",
  "identificar al condado se usó la posición espacial de cada condado en el ",
  "mapa de Estados Unidos, además del color del partido vencedor y, ",
  "naturalmente, el tamaño como canal para el dato cuantitativo.\n\n",

  "Luego de este pequeño análisis nos podemos preguntar ¿es el gráfico de la ",
  "izquierda un error? Parece evidente que sí, más quisiera argumentar que la ",
  "crítica del artículo es incompleta: claramente mostrar al ganador de una ",
  "elección presidencial en Estados Unidos mediante la superficie de cada ",
  "condado no nos dice mucho pues, como se resalta en el artículo, son las ",
  "personas las que votan, no los territorios. Sin embargo, el sistema de ",
  "elección en Norteamérica es de votación indirecta, donde lo que se vota ",
  "realmente es cuál opción debe escoger el total de electores que el estado ",
  "tiene asignado; con esto, mostrar la cantidad de gente que votó en cada ",
  "condado junto con la opción ganadora tampoco nos ayuda a saber quién ",
  "finalmente es el vencedor en el país. Quizás la mejor opción para saber ",
  "quién fue el vencedor sería usar el tamaño de los estados de acuerdo a la ",
  "cantidad de electores que tiene, junto al color del partido que resultó ",
  "ganador.\n",

  "Ahora, si la intención es también mostrar cómo votó la mayoría de la ",
  "población de Estados Unidos, pero como un todo (al igual que se escoje ",
  "presidente en Chile), se debería agregar algún canal que indique la ",
  "proporción o porcentaje por el cual un candidato ganó, aunque hacerlo a ",
  "nivel de condado puede aumentar el ruido, por lo que tomar una unidad ",
  "territorial mayor, como un estado, puede ser más apropiado. De hecho, para ",
  "la Ciencia Política podría ser interesante comparar estas 2 visualizaciones",
  ", pues es sabido que el 2016 Trump (Republicano) ganó por electores, pero ",
  "Clinton (Demócrata) ganó en total de población.",
  sep = ""
)


# Ítem 6 ------------------------------------------------------------------

# Se definen algunos estilos previos
colores <- c("Partido Demócrata" = "#0015BC", "Partido Republicano" = "#DE0100")
font_add_google(name = "Playfair Display", family = "playdis")
font_add_google(name = "Barlow", family = "barlow")
showtext_auto()

# Se modican los datos para el gráfico
presidencial_arreglado <- presidencial %>%
  mutate(
    anos_ejercicio = as.double(fin - inicio) / 365.25,
    anos_ejercicio = ifelse(
      test = partido == "Demócrata",
      yes = anos_ejercicio * -1,
      no = anos_ejercicio
    )
  )
presidencial_arreglado[8, "nombre"] <- "G.H.W. Bush"
presidencial_arreglado[10, "nombre"] <- "G.W. Bush"

# Se genera el gráfico
grafico_presidentes <- ggplot(
  data = presidencial_arreglado,
  mapping = aes(
    y = reorder(nombre, inicio),
    x = anos_ejercicio,
    fill = paste("Partido", partido),
    label = nombre
  )
) +
  geom_col() +
  scale_fill_manual(values = colores) +
  scale_x_continuous(
    breaks = seq(-8, 8, by = 4),
    labels = ifelse(
      test = abs(seq(-8, 8, by = 4)) == 0,
      yes = 0,
      no = paste(abs(seq(-8, 8, by = 4)), "años")
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "playdis"),
    plot.subtitle = element_text(hjust = 0.5, family = "playdis"),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(family = "barlow"),
    legend.position = "bottom",
    legend.text = element_text(family = "playdis")
  ) +
  labs(
    title = "Años de gobierno por presidente",
    subtitle = "Estados Unidos entre 1953 y 2021",
    y = element_blank(),
    fill = element_blank()
  )
grafico_presidentes

# Explique el qué, el porqué y el cómo.
cat(
  "En este gráfico se busca presentar información proveniente de un conjunto",
  " de datos tabulados: el tiempo de ejercicio de todos los presidentes de ",
  "Estados Unidos entre los años 1953 y 2021.\n",
  "La intención detrás del gráfico es realizar una consulta de comparación ",
  "entre los distintos períodos presidenciales, poniendo especial énfasis en ",
  "el partido político al cual pertenece cada uno para poder responder quiénes",
  ", Demócratas o Republicanos, han estado más tiempo en el poder ejecutivo.\n",
  "Para mostrar la información se prefirió usar la geometría de barras con ",
  "orientación horizontal en donde el largo, el color y la posición de estas ",
  "con respecto a un eje central aportan la información que se quiere ",
  "transmitir.",
  sep = ""
)

# Qué es lo que está codificando en términos de marcas y canales.
cat(
  "Como marca se usó una línea gruesa en forma de barra, las cuales fueron ",
  "dispuestas de forma horizontal para marcar el tiempo en años que cada ",
  "presidente cumplió en su mandato.\n",
  "Como canales se usaron:\n",
  "\ta) El largo en el eje X, que indica la duración del mandato, siendo el ",
  "único canal que se usó explícitamente para expresar magnitud. Al demarcar",
  " el eje se usaron múltiplos de 4 pues corresponde a la duración de un ",
  "período presidencial y, al tener opción a 1 reelección, puede completar los",
  " 8 años de gobierno.\n",
  "\tb) La posición horizontal de la barra con respecto al eje medio, que ",
  "informa el partido político al cual pertenece o pertenecía el presidente.\n",
  "\tc) La posición en el eje Y, que no solo sirve para individualizar a los ",
  "presidentes, sino que también los ordena cronológicamente, desde el más ",
  "distante abajo hasta el más reciente arriba. No se explicitó la cronología ",
  "pues no era el foco de la presentación, pero es reconocible para quien ",
  "sabe de historia norteamericana.\n",
  "\td) El color, que indica, al igual que la posición de las barras en el eje",
  " horizontal, el partido político al cual pertenecía el presidente al ",
  "momento de su mandato. Se redundó en esta información pues, en combinación ",
  "con la posición horizontal, se obtiene una mejor apreciasión de qué partido",
  " ha estado más tiempo en el poder, como si fueran 2 histogramas divergentes",
  ", uno hacia cada lado, resaltando la naturaleza bipartidista del sistema ",
  "norteamericano. Los colores usados son los característicos de cada partido ",
  "político; sin embargo, alguien que no esté familiarizado con el contexto ",
  "podría percibir ruido con la selección, pues el rojo es también color ",
  "simbólico del partido comunista, tant en Chile como en el mundo.",
  sep = ""
)
