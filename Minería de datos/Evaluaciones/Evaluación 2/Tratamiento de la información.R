# Evaluación 2
# Néstor Patricio Rojas Ríos

############################## Prolegómenos ##############################
# Configuración del espacio de trabajo
# Linux
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Minería de datos/Evaluaciones/Evaluación 2'
))

# Windows
setwd(paste0(
  'C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ',
  'datos\\Evaluaciones\\Evaluación 2'
))

# Instalación de librerías
#install.packages(c('openxlsx', 'rpart', 'rpart.plot'))

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
  parms = list(split = 'information')
)

printcp(modelo_arbol_1_entropia)
rpart.plot(modelo_arbol_1_entropia)

# Árbol por defecto
# Criterio de clasificación de ganancia de información Gini
modelo_arbol_1_gini <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'gini')
)

printcp(modelo_arbol_1_gini)
rpart.plot(modelo_arbol_1_gini)

# Cambiando el costo de complejidad
modelo_arbol_2_cp <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'information'),
  control = rpart.control(
    cp = 0.016
  )
)

printcp(modelo_arbol_2_cp)
rpart.plot(modelo_arbol_2_cp)

# Cambiando el minsplit
modelo_arbol_3_minsplit <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'information'),
  control = rpart.control(
    minsplit = 75 # 10% del total
  )
)

printcp(modelo_arbol_3_minsplit)
rpart.plot(modelo_arbol_3_minsplit)

# Cambiando el minbucket
modelo_arbol_3_minbucket <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'information'),
  control = rpart.control(
    minbucket = 30 # 4% del total
  )
)

printcp(modelo_arbol_3_minbucket)
rpart.plot(modelo_arbol_3_minbucket)

# Cambiando el maxdepth
modelo_arbol_3_maxdepth <- rpart(
  formula = credit ~ .,
  data = set_entrenamiento,
  method = 'class',
  parms = list(split = 'information'),
  control = rpart.control(
    maxdepth = 5 # 5 de 10 niveles, considerando nodo final
  )
)

printcp(modelo_arbol_3_maxdepth)
rpart.plot(modelo_arbol_3_maxdepth)


# Validación de los datos
# Criterio de clasificación de ganancia de información por entropía
prediccion_modelo_arbol_1_entropia <- predict(
  modelo_arbol_1_entropia,
  type = 'class'
)

matriz_modelo_arbol_1_entropia <- table(
  set_entrenamiento$credit,
  prediccion_modelo_arbol_1_entropia
)
matriz_modelo_arbol_1_entropia

precision_modelo_arbol_1_entropia <- sum(
  set_entrenamiento$credit == prediccion_modelo_arbol_1_entropia
) / nrow(set_entrenamiento)
precision_modelo_arbol_1_entropia

validacion_modelo_arbol_1_entropia <- predict(
  modelo_arbol_1_entropia,
  newdata = set_validacion,
  type = 'class'
)

matriz_validacion_modelo_arbol_1_entropia <- table(
  set_validacion$credit,
  validacion_modelo_arbol_1_entropia
)
matriz_validacion_modelo_arbol_1_entropia

precision_valicacion_modelo_arbol_1_entropia <- sum(
  set_validacion$credit == validacion_modelo_arbol_1_entropia
) / nrow(set_validacion)
precision_valicacion_modelo_arbol_1_entropia

# Criterio de clasificación de ganancia de información Gini
prediccion_modelo_arbol_1_gini <- predict(
  modelo_arbol_1_gini,
  type = 'class'
)

matriz_modelo_arbol_1_gini <- table(
  set_entrenamiento$credit,
  prediccion_modelo_arbol_1_gini
)
matriz_modelo_arbol_1_gini

precision_modelo_arbol_1_gini <- sum(
  set_entrenamiento$credit == prediccion_modelo_arbol_1_gini
) / nrow(set_entrenamiento)
precision_modelo_arbol_1_gini

validacion_modelo_arbol_1_gini <- predict(
  modelo_arbol_1_gini,
  newdata = set_validacion,
  type = 'class'
)

matriz_validacion_modelo_arbol_1_gini <- table(
  set_validacion$credit,
  validacion_modelo_arbol_1_gini
)
matriz_validacion_modelo_arbol_1_gini

precision_valicacion_modelo_arbol_1_gini <- sum(
  set_validacion$credit == validacion_modelo_arbol_1_gini
) / nrow(set_validacion)
precision_valicacion_modelo_arbol_1_gini


############################## Pregunta 3 ##############################


############################## Pregunta 4 ##############################



