# Evaluación 2
# Néstor Patricio Rojas Ríos

############################## Prolegómenos ##############################
# Configuración del espacio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Minería de datos/Evaluaciones/Evaluación 2'
))

# Instalación de librerías
#install.packages('openxlsx')

# Carga de librerías
library(dplyr)
library(ggplot2)
library(openxlsx)
library(rpart)
library(rpart.plot)

# Carga de datos
datos_banco <- read.csv('SouthGermanCredit.csv', header = TRUE, sep = ';')


############################## Desarrollo ##############################

############################## Pregunta 1 ##############################
# Generación de los 2 subsets:
set.seed(11)
set_entrenamiento <- slice_sample(datos_banco, prop = 0.75, replace = FALSE)
set_validacion <- setdiff(datos_banco, set_entrenamiento)

# Comparación del set rincipal y los 2 subsets
comparacion_credito_df <- data.frame(
  Datos = c(
    'Datos originales',
    'Datos de entrenamiento',
    'Datos de validación'
  ),
  'Número_elementos' = c(
    nrow(datos_banco),
    nrow(set_entrenamiento),
    nrow(set_validacion)
  ),
  'Promedio_credit' = c(
    mean(datos_banco$credit),
    mean(set_entrenamiento$credit),
    mean(set_validacion$credit)
  ),
  'Desv_estándar_credit' = c(
    sd(datos_banco$credit),
    sd(set_entrenamiento$credit),
    sd(set_validacion$credit)
  ),
  'Registros_NA_credit' = c(
    sum(is.na(datos_banco$credit)),
    sum(is.na(set_entrenamiento$credit)),
    sum(is.na(set_validacion$credit))
  )
)

# Se guarda tabla en un archivo xlsx para insertarla en el informe
write.xlsx(
  comparacion_credito_df,
  file = 'Tablas relevantes.xlsx',
  sheetName = 'Comparación de sets 2',
  borders = 'all',
  overwrite = TRUE
)


############################## Pregunta 2 ##############################
# Árbol por defecto
# Criterio de clasificación de ganancia de información por entropía
modelo_arbol_1_entropia <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'information'),
  control = rpart.control(
    minsplit = 20,
    minbucket = 6,
    cp = 0,
    maxdepth = 30
  )
)

printcp(modelo_arbol_1_entropia)
rpart.plot(modelo_arbol_1_entropia)

# Árbol por defecto
# Criterio de clasificación de ganancia de información Gini
modelo_arbol_1_gini <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'gini'),
  control = rpart.control(
    minsplit = 20,
    minbucket = 6,
    cp = 0,
    maxdepth = 30
  )
)

printcp(modelo_arbol_1_gini)
rpart.plot(modelo_arbol_1_gini)


############################## Pregunta 3 ##############################


############################## Pregunta 4 ##############################



