# Evaluación 1.

# Configurando área de trabajo e importando datos.
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Evaluaciones/Evaluación 1'
))
datos_paises <- read.csv('DatosPaises.csv', sep = ';')
summary(datos_paises)
head(datos_paises, n = 10)

for(dato in names(datos_paises)) {
  if(dato != 'PAIS') {
    boxplot(datos_paises[, dato])
    title(dato)
  }
}

