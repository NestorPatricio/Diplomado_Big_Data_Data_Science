# Evaluación 3b
# Néstor Patricio Rojas Ríos


# Prolegómenos ------------------------------------------------------------
# Configuración del directorio de trabajo
if (Sys.info()["sysname"] == "Windows") {
  setwd(paste0(
    "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\",
    "\\Minería de datos\\Evaluaciones\\Evaluación 3b\\"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de ",
    "datos/Evaluaciones/Evaluación 3b/"
  ))
}

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "lintr",
  "arules",
  "arulesViz"
)
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Se activa el linter para R
lint("Tratamiento de la información.R")

# Carga de datos
playlists <- read.csv(
  file = "lastfm.csv",
  header = TRUE,
  sep = ";"
)


# Funciones ---------------------------------------------------------------



# Desarrollo --------------------------------------------------------------

# Pregunta 1 --------------------------------------------------------------
# Se generan las transacciones
transacciones <- playlists %>%
  transactions(format = "long", cols = c("user", "artist"))
summary(transacciones)

# Se realiza una primera aproximación a los datos
resultado_todos <- apriori(
  data = transacciones,
  parameter = list(
    supp = 0.03,
    conf = 0.05,
    minlen = 2,
    maxlen = 15,
    maxtime = 15
  )
) %>%
  sort(by = "support", decreasing = TRUE)
summary(object = resultado_todos)
inspect(x = resultado_todos)

# El menor valor de confianza es de 0.029.
# El mayor valor de confianza es de 1.
# El menor valor de soporte es variable; con 0.0005 queda en 0.000533.
# El mayor valor de soporte es de 0.0582.


# Pregunta 3 --------------------------------------------------------------
# Artistas a encontrar recomendaciones:
# Coldplay, Jimi Hendrix, Metallica, Muse, Slayer, The Killers, The Shins, Wilco

# Arctic Monkeys
resultado_recomendacion_1 <- apriori(
  data = transacciones,
  parameter = list(
    supp = 0.01,
    conf = 0.04,
    minlen = 2,
    maxlen = 15,
    maxtime = 15
  ),
  appearance = list(lhs = "arctic monkeys")
) %>%
  sort(by = "support", decreasing = TRUE)
summary(object = resultado_recomendacion_1)
inspect(x = resultado_recomendacion_1)

# Beyonce
resultado_recomendacion_2 <- apriori(
  data = transacciones,
  parameter = list(
    supp = 0.01,
    conf = 0.04,
    minlen = 2,
    maxlen = 15,
    maxtime = 15
  ),
  appearance = list(lhs = "beyonce")
) %>%
  sort(by = "support", decreasing = TRUE)
summary(object = resultado_recomendacion_2)
inspect(x = resultado_recomendacion_2)
