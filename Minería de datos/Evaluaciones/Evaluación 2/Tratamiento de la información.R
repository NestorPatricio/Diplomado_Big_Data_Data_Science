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
library(gridExtra)
library(rpart)
library(rpart.plot)
library(class)

# Carga de datos
datos_banco <- read.csv('SouthGermanCredit.csv', header = TRUE, sep = ';')


############################## Funciones ##############################
heterogeneidad_entropia <- function(p_factor) {
  if(p_factor == 0) {
    return(0)
  }
  factor_1 = -1 * p_factor * log(p_factor, base = 2)
  factor_2 = -1 * (1 - p_factor) * log((1 - p_factor), base = 2)
  return(factor_1 + factor_2)
}

heterogeneidad_gini <- function(p_factor) {
  return(p_factor * (1 - p_factor) * 2)
}

arbol_clasificacion <- function(
    datos,
    metodo = 'class',
    metodo_div = 'information',
    p_complejidad = 0.01,
    min_datos_padre = FALSE,
    min_datos_hijo = FALSE,
    max_profundidad = 30
) {
  if(!min_datos_padre & !min_datos_hijo) {
    min_datos_padre = 20
    min_datos_hijo = round(min_datos_padre / 3)
  } else if(!min_datos_padre) {
    min_datos_padre = min_datos_hijo * 3
  } else {
    min_datos_hijo = round(min_datos_padre / 3)
  }
  
  modelo <- rpart(
    formula = credit ~ .,
    data = datos,
    method = metodo,
    parms = list(split = metodo_div),
    model = TRUE,
    control = rpart.control(
      cp = p_complejidad,
      minsplit = min_datos_padre,
      minbucket = min_datos_hijo,
      maxdepth = max_profundidad
    )
  )
  
  return(modelo)
}

validacion_matriz_conf <- function(
  modelo,
  datos_entrena,
  datos_valida
) {
  prediccion_entrena <- predict(
    object = modelo,
    type = 'class'
  )
  prediccion_valida <- predict(
    object = modelo,
    newdata = datos_valida,
    type = 'class'
  )
  
  matriz_entrena <- table(
    datos_entrena[, 'credit'],
    prediccion_entrena
  )
  matriz_valida <- table(
    datos_valida[, 'credit'],
    prediccion_valida
  )
  
  exactitud_entrena <- sum(datos_entrena[, 'credit'] == prediccion_entrena) /
    nrow(datos_entrena)
  exactitud_valida <- sum(datos_valida[, 'credit'] == prediccion_valida) /
    nrow(datos_valida)
  
  return(list(
    matriz_entrena,
    exactitud_entrena,
    matriz_valida,
    exactitud_valida
  ))
}

dataframe_exactitud <- function(
    var_exogenas_entrena,
    var_endogena_entrena,
    var_exogenas_valida,
    var_endogena_valida,
    k_max,
    intentos,
    por_promedio = FALSE
) {
  # Requiere de las librerías class o FNN, según el caso, para calcular knn
  
  # Se genera la matriz con valores NA
  matriz = matrix(NA, k_max, intentos)
  matriz = cbind(matriz, Valor_K = 1:k_max)
  
  # Se realizan los cálculos del modelo KNN y obtienen los valores de exactitud
  for(intento in 1:intentos) {
    valor_k = 1
    vector_columna = 1
    
    for(valor_k in 1:k_max) {
      # Acá se entrena cada uno de los modelos de knn
      modelo_knn <- knn(
        train = var_exogenas_entrena,
        test = var_exogenas_valida,
        cl = var_endogena_entrena,
        k = valor_k
      )
      
      # Acá se define la exactitud
      vector_columna[valor_k] = sum(var_endogena_valida == modelo_knn) /
        length(var_endogena_valida)
    }
    
    matriz[, intento] = vector_columna
  }
  
  # Se pone nombre a las columnas con los intentos
  columna = 1
  vector_nombre = 1
  for(columna in 1:intentos) {
    vector_nombre[columna] = paste0('Intento_', as.character(columna))
  }
  colnames(matriz)[1:intentos] = vector_nombre
  
  # Se calcula el promedio de los valores obtenidos para cada valor de K
  vector_promedio = 1
  for(valor_k in 1:k_max) {
    vector_promedio[valor_k] = round(mean(matriz[valor_k, 1:intentos]), 3)
  }
  matriz = matriz[, c(intentos + 1, 1:intentos)]
  matriz = cbind(matriz, Promedios = vector_promedio)
  
  df_final = as.data.frame(matriz)
  if(por_promedio) {
    return(df_final[order(-df_final$Promedios), ])
  }
  return(df_final)
}


############################## Desarrollo ##############################

############################## Pregunta 1 ##############################
# Generación de los 2 subsets:
set.seed(11)
set_entrenamiento <- slice_sample(datos_banco, prop = 0.75, replace = FALSE)
set_validacion <- setdiff(datos_banco, set_entrenamiento)

p_c1_total <- nrow(filter(datos_banco, credit == 1)) / nrow(datos_banco)
p_c1_ent <- nrow(filter(set_entrenamiento, credit == 1)) / nrow(set_entrenamiento)
p_c1_val <- nrow(filter(set_validacion, credit == 1)) / nrow(set_validacion)

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
  'Probabilidad_credit_1' = c(
    p_c1_total,
    p_c1_ent,
    p_c1_val
  ),
  'Heterogeneidad_por_entropía' = c(
    heterogeneidad_entropia(p_c1_total),
    heterogeneidad_entropia(p_c1_ent),
    heterogeneidad_entropia(p_c1_val)
  ),
  'Heterogeneidad_por_Gini' = c(
    heterogeneidad_gini(p_c1_total),
    heterogeneidad_gini(p_c1_ent),
    heterogeneidad_gini(p_c1_val)
  )
)

# Se genera la tabla como imagen
grid.table(
  d = comparacion_credito_df,
  row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c('lightblue1', 'azure'), col = 'black')
    ),
    colhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white')
    )
  )
)


############################## Pregunta 2 ##############################
# Constantes previas
formato_tablas <- ttheme_default(
  core = list(
    bg_params = list(fill = c('grey95'), col = 'black')
  ),
  colhead = list(
    bg_params = list(fill = 'deepskyblue2', col = 'black'),
    fg_params = list(col = 'white')
  ),
  rowhead = list(
    bg_params = list(fill = 'deepskyblue4', col = 'black'),
    fg_params = list(col = 'white')
  )
)


# Comparación por método de división
# Entropía de la información
modelo_entropia <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'information'
)
print(modelo_entropia)
printcp(modelo_entropia)
rpart.plot(
  x = modelo_entropia,
  main = 'Árbol de clasificación por\nEntropía de la información',
  extra = 8
)

validacion_entropia <- validacion_matriz_conf(
  modelo = modelo_entropia,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_entropia <- validacion_entropia[[1]]
matriz_valida_entropia <- validacion_entropia[[3]]
exactitud_entrena_entropia <- validacion_entropia[[2]]
exactitud_valida_entropia <- validacion_entropia[[4]]

# Tabla de parámetros de complejidad
grid.table(
  d = round(modelo_entropia$cptable, digits = 4),
  row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c('lightblue1', 'azure'), col = 'black')
    ),
    colhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white')
    )
  )
)

# Índice de impuridad de Gini
modelo_gini <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'gini'
)
print(modelo_gini)
printcp(modelo_gini)
rpart.plot(
  modelo_gini,
  main = 'Árbol de clasificación por\nÍndice de impuridad de Gini',
  extra = 8
)

validacion_gini <- validacion_matriz_conf(
  modelo = modelo_gini,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_gini <- validacion_gini[[1]]
matriz_valida_gini <- validacion_gini[[3]]
exactitud_entrena_gini <- validacion_gini[[2]]
exactitud_valida_gini <- validacion_gini[[4]]

# Comparación de matrices de confusión con datos de validación
grid.arrange(
  tableGrob(matriz_valida_entropia, theme = formato_tablas),
  tableGrob(matriz_valida_gini, theme = formato_tablas),
  nrow = 1
)

# Tabla de parámetros de complejidad
grid.table(
  d = round(modelo_gini$cptable, digits = 4),
  row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c('lightblue1', 'azure'), col = 'black')
    ),
    colhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white')
    )
  )
)


# Comparación por parámetro de complejidad
# Entropìa de la información
modelo_entropia_cp <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'information',
  p_complejidad = 0.016
)
print(modelo_entropia_cp)
printcp(modelo_entropia_cp)
rpart.plot(
  modelo_entropia_cp,
  main = 'Árbol de clasificación con\nparámetro de complejidad 0,016 (Entropía)',
  extra = 8
)

validacion_entropia_cp <- validacion_matriz_conf(
  modelo = modelo_entropia_cp,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_entropia_cp <- validacion_entropia_cp[[1]]
matriz_valida_entropia_cp <- validacion_entropia_cp[[3]]
exactitud_entrena_entropia_cp <- validacion_entropia_cp[[2]]
exactitud_valida_entropia_cp <- validacion_entropia_cp[[4]]

# Índice de impuridad de Gini
modelo_gini_cp <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'gini',
  p_complejidad = 0.016
)
print(modelo_gini_cp)
printcp(modelo_gini_cp)
rpart.plot(
  modelo_gini_cp,
  main = 'Árbol de clasificación con\nparámetro de complejidad 0,016 (Gini)',
  extra = 8
)

validacion_gini_cp <- validacion_matriz_conf(
  modelo = modelo_gini_cp,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_gini_cp <- validacion_gini_cp[[1]]
matriz_valida_gini_cp <- validacion_gini_cp[[3]]
exactitud_entrena_gini_cp <- validacion_gini_cp[[2]]
exactitud_valida_gini_cp <- validacion_gini_cp[[4]]

# Comparación de matrices de confusión con datos de validación
grid.arrange(
  tableGrob(matriz_valida_entropia, theme = formato_tablas),
  tableGrob(matriz_valida_entropia_cp, theme = formato_tablas),
  nrow = 1
)


# Comparacion por criterios de parada
# Cantidad por nodo predivisión
modelo_entropia_predivision <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'information',
  min_datos_padre = 75 # 10% de la muestra
)
print(modelo_entropia_predivision)
printcp(modelo_entropia_predivision)
rpart.plot(
  modelo_entropia_predivision,
  main = 'Árbol de clasificación por\ntamaño del nodo de división en 75 (Entropía)',
  extra = 8
)

validacion_entropia_predivision <- validacion_matriz_conf(
  modelo = modelo_entropia_predivision,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_entropia_predivision <- validacion_entropia_predivision[[1]]
matriz_valida_entropia_predivision <- validacion_entropia_predivision[[3]]
exactitud_entrena_entropia_predivision <- validacion_entropia_predivision[[2]]
exactitud_valida_entropia_predivision <- validacion_entropia_predivision[[4]]

# Cantidad por nodo posdivisión
modelo_entropia_posdivision <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'information',
  min_datos_hijo = 30 # 4% de la muestra
)
print(modelo_entropia_posdivision)
printcp(modelo_entropia_posdivision)
rpart.plot(
  modelo_entropia_posdivision,
  main = 'Árbol de clasificación por\ntamaño del nodo terminal en 30 (Entropía)',
  extra = 8
)

validacion_entropia_posdivision <- validacion_matriz_conf(
  modelo = modelo_entropia_posdivision,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_entropia_posdivision <- validacion_entropia_posdivision[[1]]
matriz_valida_entropia_posdivision <- validacion_entropia_posdivision[[3]]
exactitud_entrena_entropia_posdivision <- validacion_entropia_posdivision[[2]]
exactitud_valida_entropia_posdivision <- validacion_entropia_posdivision[[4]]

# Profundidad
modelo_entropia_profundidad <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'information',
  max_profundidad = 5 # 5 de 10 niveles
)
print(modelo_entropia_profundidad)
printcp(modelo_entropia_profundidad)
rpart.plot(
  modelo_entropia_profundidad,
  main = 'Árbol de clasificación por\nprofundidad del árbol en 5 (Entropía)',
  extra = 8
)

validacion_entropia_profundidad <- validacion_matriz_conf(
  modelo = modelo_entropia_profundidad,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_entropia_profundidad <- validacion_entropia_profundidad[[1]]
matriz_valida_entropia_profundidad <- validacion_entropia_profundidad[[3]]
exactitud_entrena_entropia_profundidad <- validacion_entropia_profundidad[[2]]
exactitud_valida_entropia_profundidad <- validacion_entropia_profundidad[[4]]

# Comparación de matrices de confusión con datos de validación
grid.arrange(
  tableGrob(matriz_valida_entropia, theme = formato_tablas),
  tableGrob(matriz_valida_entropia_predivision, theme = formato_tablas),
  tableGrob(matriz_valida_entropia_posdivision, theme = formato_tablas),
  tableGrob(matriz_valida_entropia_profundidad, theme = formato_tablas),
  nrow = 1
)


############################## Pregunta 3 ##############################
# Se separa la variable endógena de la exógena en los sets de entrenamiento y
#validación
var_endogena_entrena <- set_entrenamiento[, 17]
var_exogenas_entrena <- set_entrenamiento[, 1:16]
var_endogena_valida <- set_validacion[, 17]
var_exogenas_valida <- set_validacion[, 1:16]

# Una primera evaluación con todas las variables exógenas
knn_todas <- dataframe_exactitud(
  var_exogenas_entrena,
  var_endogena_entrena,
  var_exogenas_valida,
  var_endogena_valida,
  k_max = 20,
  intentos = 20,
  por_promedio = TRUE
)
knn_todas
plot(
  x = knn_todas$Valor_K,
  y = knn_todas$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud'
)

Positivo: 1
Negativo: 0



############################## Pregunta 4 ##############################



