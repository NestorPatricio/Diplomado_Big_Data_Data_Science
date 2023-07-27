# Apuntes Clase 5

# Se configura el directorio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Visualización de datos aplicada/'
))

# Instalación de paquetes de R
#install.packages('devtools')
#install_github('gastonstat/colortools')
#install.packages('showtext')

# development version

library(colortools)
library(RColorBrewer)
library(showtext)
library(ggplot2)

data("diamonds")
