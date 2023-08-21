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



