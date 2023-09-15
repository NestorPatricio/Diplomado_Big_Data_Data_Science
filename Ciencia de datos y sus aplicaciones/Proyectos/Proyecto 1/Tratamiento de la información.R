# Ciencia de datos y sus aplicaciones
# Proyecto 1

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "readxl",
  "lintr"
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
    "Ciencia de datos y sus aplicaciones\\Proyectos\\Proyecto 1"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Ciencia de ",
    "datos y sus aplicaciones/Proyectos/Proyecto 1/"
  ))
}

# Se ejecuta el linter de R para evaluar el estilo del script
lint("Tratamiento de la información.R")


# Carga de la información -------------------------------------------------
dataset_original <- tibble(read_xls("proyecto_1_datos_fuga.xls"))


# Generación de nuevos datasets -------------------------------------------
dataset_solo_telefono <- dataset_original %>%
  filter(!is.na(`Cargo Mensual LLamadas`))

dataset_solo_internet <- dataset_original %>%
  filter(`Servicio Internet` == "Si")
