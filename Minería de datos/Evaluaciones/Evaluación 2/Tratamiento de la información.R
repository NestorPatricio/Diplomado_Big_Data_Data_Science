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
library(caret)
library(e1071)

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
    prediccion_entrena,
    datos_entrena[, 'credit']
  )
  matriz_valida <- table(
    prediccion_valida,
    datos_valida[, 'credit']
  )
  
  exactitud_entrena <- sum(datos_entrena[, 'credit'] == prediccion_entrena) /
    nrow(datos_entrena)
  exactitud_valida <- sum(datos_valida[, 'credit'] == prediccion_valida) /
    nrow(datos_valida)
  
  especificidad_entrena <- sum(
    datos_entrena[, 'credit'] == prediccion_entrena & prediccion_entrena == 0
  ) / sum(datos_entrena[, 'credit'] == 0)
  especificidad_valida <- sum(
    datos_valida[, 'credit'] == prediccion_valida & prediccion_valida == 0
  ) / sum(datos_valida[, 'credit'] == 0)
  
  return(list(
    matriz_entrena,
    exactitud_entrena,
    especificidad_entrena,
    matriz_valida,
    exactitud_valida,
    especificidad_valida
  ))
}

normalizacion <- function(valor_i, valor_minimo, valor_maximo) {
  numerador = valor_i - valor_minimo
  denominador = valor_maximo - valor_minimo
  return(numerador / denominador)
}

dataframe_exactitud_knn <- function(
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

dataframe_especificidad_knn <- function(
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
      
      # Acá se define la especificidad
      vector_columna[valor_k] = sum(
        var_endogena_valida == modelo_knn & modelo_knn == 0
      ) / sum(var_endogena_valida == 0)
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

p_c0_total <- nrow(filter(datos_banco, credit == 0)) / nrow(datos_banco)
p_c0_ent <- nrow(filter(set_entrenamiento, credit == 0)) / nrow(set_entrenamiento)
p_c0_val <- nrow(filter(set_validacion, credit == 0)) / nrow(set_validacion)

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
  'Probabilidad_credit_0' = c(
    p_c0_total,
    p_c0_ent,
    p_c0_val
  ),
  'Heterogeneidad_por_entropía' = c(
    heterogeneidad_entropia(p_c0_total),
    heterogeneidad_entropia(p_c0_ent),
    heterogeneidad_entropia(p_c0_val)
  ),
  'Heterogeneidad_por_Gini' = c(
    heterogeneidad_gini(p_c0_total),
    heterogeneidad_gini(p_c0_ent),
    heterogeneidad_gini(p_c0_val)
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
matriz_valida_entropia <- validacion_entropia[[4]]
exactitud_entrena_entropia <- validacion_entropia[[2]]
exactitud_valida_entropia <- validacion_entropia[[5]]
especificidad_entrena_entropia <- validacion_entropia[[3]]
especificidad_valida_entropia <- validacion_entropia[[6]]

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
matriz_valida_gini <- validacion_gini[[4]]
exactitud_entrena_gini <- validacion_gini[[2]]
exactitud_valida_gini <- validacion_gini[[5]]
especificidad_entrena_gini <- validacion_gini[[3]]
especificidad_valida_gini <- validacion_gini[[6]]

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
matriz_valida_entropia_cp <- validacion_entropia_cp[[4]]
exactitud_entrena_entropia_cp <- validacion_entropia_cp[[2]]
exactitud_valida_entropia_cp <- validacion_entropia_cp[[5]]
especificidad_entrena_entropia_cp <- validacion_entropia_cp[[3]]
especificidad_valida_entropia_cp <- validacion_entropia_cp[[6]]

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
matriz_valida_gini_cp <- validacion_gini_cp[[4]]
exactitud_entrena_gini_cp <- validacion_gini_cp[[2]]
exactitud_valida_gini_cp <- validacion_gini_cp[[5]]
especificidad_entrena_gini_cp <- validacion_gini_cp[[3]]
especificidad_valida_gini_cp <- validacion_gini_cp[[6]]

# Comparación de matrices de confusión con datos de validación
grid.arrange(
  tableGrob(matriz_valida_entropia, theme = formato_tablas),
  tableGrob(matriz_valida_entropia_cp, theme = formato_tablas),
  nrow = 1
)


# Comparacion por criterios de parada
# Cantidad por nodo predivisión
modelo_gini_predivision <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'gini',
  min_datos_padre = 75 # 10% de la muestra
)
print(modelo_gini_predivision)
printcp(modelo_gini_predivision)
rpart.plot(
  modelo_gini_predivision,
  main = 'Árbol de clasificación por\ntamaño del nodo de división en 75 (Gini)',
  extra = 8
)

validacion_gini_predivision <- validacion_matriz_conf(
  modelo = modelo_gini_predivision,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_gini_predivision <- validacion_gini_predivision[[1]]
matriz_valida_gini_predivision <- validacion_gini_predivision[[4]]
exactitud_entrena_gini_predivision <- validacion_gini_predivision[[2]]
exactitud_valida_gini_predivision <- validacion_gini_predivision[[5]]
especificidad_entrena_gini_predivision <- validacion_gini_predivision[[3]]
especificidad_valida_gini_predivision <- validacion_gini_predivision[[6]]

# Cantidad por nodo posdivisión
modelo_gini_posdivision <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'gini',
  min_datos_hijo = 30 # 4% de la muestra
)
print(modelo_gini_posdivision)
printcp(modelo_gini_posdivision)
rpart.plot(
  modelo_gini_posdivision,
  main = 'Árbol de clasificación por\ntamaño del nodo terminal en 30 (Gini)',
  extra = 8
)

validacion_gini_posdivision <- validacion_matriz_conf(
  modelo = modelo_gini_posdivision,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_gini_posdivision <- validacion_gini_posdivision[[1]]
matriz_valida_gini_posdivision <- validacion_gini_posdivision[[4]]
exactitud_entrena_gini_posdivision <- validacion_gini_posdivision[[2]]
exactitud_valida_gini_posdivision <- validacion_gini_posdivision[[5]]
especificidad_entrena_gini_posdivision <- validacion_gini_posdivision[[3]]
especificidad_valida_gini_posdivision <- validacion_gini_posdivision[[6]]

# Profundidad
modelo_gini_profundidad <- arbol_clasificacion(
  datos = set_entrenamiento,
  metodo_div = 'gini',
  max_profundidad = 5 # 5 de 10 niveles
)
print(modelo_gini_profundidad)
printcp(modelo_gini_profundidad)
rpart.plot(
  modelo_gini_profundidad,
  main = 'Árbol de clasificación por\nprofundidad en 5 (Gini)',
  extra = 8
)

validacion_gini_profundidad <- validacion_matriz_conf(
  modelo = modelo_gini_profundidad,
  datos_entrena = set_entrenamiento,
  datos_valida = set_validacion
)
matriz_entrena_gini_profundidad <- validacion_gini_profundidad[[1]]
matriz_valida_gini_profundidad <- validacion_gini_profundidad[[4]]
exactitud_entrena_gini_profundidad <- validacion_gini_profundidad[[2]]
exactitud_valida_gini_profundidad <- validacion_gini_profundidad[[5]]
especificidad_entrena_gini_profundidad  <- validacion_gini_profundidad [[3]]
especificidad_valida_gini_profundidad  <- validacion_gini_profundidad [[6]]

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

# Se normaliza amount
amount_entrena_min =  min(var_exogenas_entrena$amount)
amount_entrena_max =  max(var_exogenas_entrena$amount)
amount_valida_min =  min(var_exogenas_valida$amount)
amount_valida_max =  max(var_exogenas_valida$amount)

# Todas las variables exógenas sin modificación
knn_exactitud_todas <- dataframe_exactitud_knn(
  var_exogenas_entrena,
  var_endogena_entrena,
  var_exogenas_valida,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_todas
plot(
  x = knn_exactitud_todas$Valor_K,
  y = knn_exactitud_todas$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de todas las variables'
)

knn_especificidad_todas <- dataframe_especificidad_knn(
  var_exogenas_entrena,
  var_endogena_entrena,
  var_exogenas_valida,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_todas
plot(
  x = knn_especificidad_todas$Valor_K,
  y = knn_especificidad_todas$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de todas las variables'
)
# Exactitud: {31; 33: 0.700, 22: 0.697, 21: 0.696, 24; 30: 0.695}
# Especificidad: {1: 0.333, 2: 0.307, 3: 0.256, 4:0.208, 5: 0.199}


# Usando todas las variables que aparecen en alguno de los árboles
var_exogenas_entrena_1 <- var_exogenas_entrena %>% 
  select(status, history, amount, savings, employed, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0)
  ) %>% 
  select(!c(status, history, savings, employed, rate, property))
var_exogenas_valida_1 <- var_exogenas_valida %>% 
  select(status, history, amount, savings, employed, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0)
  ) %>% 
  select(!c(status, history, savings, employed, rate, property))

knn_exactitud_1 <- dataframe_exactitud_knn(
  var_exogenas_entrena_1,
  var_endogena_entrena,
  var_exogenas_valida_1,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_1
plot(
  x = knn_exactitud_1$Valor_K,
  y = knn_exactitud_1$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 1'
)

knn_especificidad_1 <- dataframe_especificidad_knn(
  var_exogenas_entrena_1,
  var_endogena_entrena,
  var_exogenas_valida_1,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_1
plot(
  x = knn_especificidad_1$Valor_K,
  y = knn_especificidad_1$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 1'
)
# Exactitud: {31: 0.700, 33: 0.699, 24: 0.697, 21; 22; 30: 0.696}
# Especificidad: {1: 0.282, 3:0.265, 2: 0.243, 7: 0.224, 6: 0.222}

# Usando variables que están en todos los árboles
var_exogenas_entrena_2 <- var_exogenas_entrena %>% 
  select(status, history, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0)
  ) %>% 
  select(!c(status, history, property))
var_exogenas_valida_2 <- var_exogenas_valida %>% 
  select(status, history, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0)
  ) %>% 
  select(!c(status, history, property))

knn_exactitud_2 <- dataframe_exactitud_knn(
  var_exogenas_entrena_2,
  var_endogena_entrena,
  var_exogenas_valida_2,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_2
plot(
  x = knn_exactitud_2$Valor_K,
  y = knn_exactitud_2$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 2'
)

knn_especificidad_2 <- dataframe_especificidad_knn(
  var_exogenas_entrena_2,
  var_endogena_entrena,
  var_exogenas_valida_2,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_2
plot(
  x = knn_especificidad_2$Valor_K,
  y = knn_especificidad_2$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 2'
)
# Exactitud: {31: 0.700, 33: 0.699, 30: 0.697, 21; 24: 0.696}
# Especificidad: {1: 0.282, 3:0.256, 2: 0.242, 7: 0.226, 6: 0.215}


# Usando todas las variables que aparecen en alguno de los árboles (amount norm)

var_exogenas_entrena_3 <- var_exogenas_entrena %>% 
  select(status, history, amount, savings, employed, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, savings, employed, rate, property))
var_exogenas_valida_3 <- var_exogenas_valida %>% 
  select(status, history, amount, savings, employed, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, savings, employed, rate, property))

knn_exactitud_3 <- dataframe_exactitud_knn(
  var_exogenas_entrena_3,
  var_endogena_entrena,
  var_exogenas_valida_3,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_3
plot(
  x = knn_exactitud_3$Valor_K,
  y = knn_exactitud_3$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 3'
)

knn_especificidad_3 <- dataframe_especificidad_knn(
  var_exogenas_entrena_3,
  var_endogena_entrena,
  var_exogenas_valida_3,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_3
plot(
  x = knn_especificidad_3$Valor_K,
  y = knn_especificidad_3$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 3'
)
# Exactitud: {14; 17: 0.718, 15; 20: 0.716, 19; 21; 48: 0.714}
# Especificidad: {1: 0.436, 2; 3:0.410, 5; 7: 0.404}

# Usando variables que están en todos los árboles (amount norm)
var_exogenas_entrena_4 <- var_exogenas_entrena %>% 
  select(status, history, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, property))
var_exogenas_valida_4 <- var_exogenas_valida %>% 
  select(status, history, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, property))

knn_exactitud_4 <- dataframe_exactitud_knn(
  var_exogenas_entrena_4,
  var_endogena_entrena,
  var_exogenas_valida_4,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_4
plot(
  x = knn_exactitud_4$Valor_K,
  y = knn_exactitud_4$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 4'
)

knn_especificidad_4 <- dataframe_especificidad_knn(
  var_exogenas_entrena_4,
  var_endogena_entrena,
  var_exogenas_valida_4,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_4
plot(
  x = knn_especificidad_4$Valor_K,
  y = knn_especificidad_4$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 4'
)
# Exactitud: {9: 0.736, 10: 0.735, 21: 0.733, 11; 35: 0.732}
# Especificidad: {3: 0.418, 2:0.412, 1: 0.410, 4: 0.401, 6: 0.376}


# Usando variables que están en todos los árboles + savings + amount norm
var_exogenas_entrena_5 <- var_exogenas_entrena %>% 
  select(status, history, savings, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, savings, property))
var_exogenas_valida_5 <- var_exogenas_valida %>% 
  select(status, history, amount, savings, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, savings, property))

knn_exactitud_5 <- dataframe_exactitud_knn(
  var_exogenas_entrena_5,
  var_endogena_entrena,
  var_exogenas_valida_5,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_5
plot(
  x = knn_exactitud_5$Valor_K,
  y = knn_exactitud_5$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 5'
)

knn_especificidad_5 <- dataframe_especificidad_knn(
  var_exogenas_entrena_5,
  var_endogena_entrena,
  var_exogenas_valida_5,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_5
plot(
  x = knn_especificidad_5$Valor_K,
  y = knn_especificidad_5$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 5'
)
# Exactitud: {48; 49; 65: 0.738, 47; 51; 66: 0.736}
# Especificidad: {1: 0.474, 7:0.462, 6: 0.457, 3: 0.456, 2: 0.451}

# Usando variables que están en todos los árboles + employed + amount norm
var_exogenas_entrena_6 <- var_exogenas_entrena %>% 
  select(status, history, employed, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, employed, property))
var_exogenas_valida_6 <- var_exogenas_valida %>% 
  select(status, history, amount, employed, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, employed, property))

knn_exactitud_6 <- dataframe_exactitud_knn(
  var_exogenas_entrena_6,
  var_endogena_entrena,
  var_exogenas_valida_6,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_6
plot(
  x = knn_exactitud_6$Valor_K,
  y = knn_exactitud_6$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 6'
)

knn_especificidad_6 <- dataframe_especificidad_knn(
  var_exogenas_entrena_6,
  var_endogena_entrena,
  var_exogenas_valida_6,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_6
plot(
  x = knn_especificidad_6$Valor_K,
  y = knn_especificidad_6$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 6'
)
# Exactitud: {15: 0.733; 14: 0.730; 13: 0.729; 9; 29: 0.728}
# Especificidad: {2: 0.453, 1:0.436, 6: 0.408, 3: 0.390, 8: 0.388}

# Usando variables que están en todos los árboles + rate + amount norm
var_exogenas_entrena_7 <- var_exogenas_entrena %>% 
  select(status, history, rate, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, rate, property))
var_exogenas_valida_7 <- var_exogenas_valida %>% 
  select(status, history, amount, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, rate, property))

knn_exactitud_7 <- dataframe_exactitud_knn(
  var_exogenas_entrena_7,
  var_endogena_entrena,
  var_exogenas_valida_7,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_7
plot(
  x = knn_exactitud_7$Valor_K,
  y = knn_exactitud_7$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 7'
)

knn_especificidad_7 <- dataframe_especificidad_knn(
  var_exogenas_entrena_7,
  var_endogena_entrena,
  var_exogenas_valida_7,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_7
plot(
  x = knn_especificidad_7$Valor_K,
  y = knn_especificidad_7$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 7'
)
# Exactitud: {6: 0.740, 13: 0.724, 5; 9; 14: 0.723}
# Especificidad: {1: 0.462, 2:0.424, 4; 6: 0.394, 3: 0.392}

# Usando variables que están en todos los árboles + savings + rate + amount norm
var_exogenas_entrena_8 <- var_exogenas_entrena %>% 
  select(status, history, savings, rate, amount, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max)
  ) %>% 
  select(!c(status, history, savings, rate, property))
var_exogenas_valida_8 <- var_exogenas_valida %>% 
  select(status, history, amount, savings, rate, property) %>% 
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    amount = normalizacion(amount, amount_valida_min, amount_valida_max)
  ) %>% 
  select(!c(status, history, savings, rate, property))

knn_exactitud_8 <- dataframe_exactitud_knn(
  var_exogenas_entrena_8,
  var_endogena_entrena,
  var_exogenas_valida_8,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_8
plot(
  x = knn_exactitud_8$Valor_K,
  y = knn_exactitud_8$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 8'
)

knn_especificidad_8 <- dataframe_especificidad_knn(
  var_exogenas_entrena_8,
  var_endogena_entrena,
  var_exogenas_valida_8,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_8
plot(
  x = knn_especificidad_8$Valor_K,
  y = knn_especificidad_8$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 8'
)
# Exactitud: {13: 0.749, 31; 37: 0.736, 9; 30: 0.734}
# Especificidad: {7: 0.443, 5:0.440, 6: 0.437, 10: 0.431, 8: 0.413}


# Usando todas las variables en binario + amount y age normalizadas
age_entrena_min =  min(var_exogenas_entrena$age)
age_entrena_max =  max(var_exogenas_entrena$age)
age_valida_min =  min(var_exogenas_valida$age)
age_valida_max =  max(var_exogenas_valida$age)

var_exogenas_entrena_9 <- var_exogenas_entrena %>%
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    personal_1 = ifelse(personal == 1, 1, 0),
    personal_2 = ifelse(personal == 2, 1, 0),
    personal_3 = ifelse(personal == 3, 1, 0),
    personal_4 = ifelse(personal == 4, 1, 0),
    residence_1 = ifelse(residence == 1, 1, 0),
    residence_2 = ifelse(residence == 2, 1, 0),
    residence_3 = ifelse(residence == 3, 1, 0),
    residence_4 = ifelse(residence == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    housing_1 = ifelse(housing == 1, 1, 0),
    housing_2 = ifelse(housing == 2, 1, 0),
    housing_3 = ifelse(housing == 3, 1, 0),
    credits_1 = ifelse(credits == 1, 1, 0),
    credits_2 = ifelse(credits == 2, 1, 0),
    credits_3 = ifelse(credits == 3, 1, 0),
    credits_4 = ifelse(credits == 4, 1, 0),
    job_1 = ifelse(job == 1, 1, 0),
    job_2 = ifelse(job == 2, 1, 0),
    job_3 = ifelse(job == 3, 1, 0),
    job_4 = ifelse(job == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max),
    age = normalizacion(age, age_entrena_min, age_entrena_max)
  ) %>% 
  select(!c(status, history, savings, employed, rate,
    personal, residence, property, housing, credits, job))
var_exogenas_valida_9 <- var_exogenas_valida %>%
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    status_4 = ifelse(status == 4, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    savings_1 = ifelse(savings == 1, 1, 0),
    savings_2 = ifelse(savings == 2, 1, 0),
    savings_3 = ifelse(savings == 3, 1, 0),
    savings_4 = ifelse(savings == 4, 1, 0),
    savings_5 = ifelse(savings == 5, 1, 0),
    employed_1 = ifelse(employed == 1, 1, 0),
    employed_2 = ifelse(employed == 2, 1, 0),
    employed_3 = ifelse(employed == 3, 1, 0),
    employed_4 = ifelse(employed == 4, 1, 0),
    employed_5 = ifelse(employed == 5, 1, 0),
    rate_1 = ifelse(rate == 1, 1, 0),
    rate_2 = ifelse(rate == 2, 1, 0),
    rate_3 = ifelse(rate == 3, 1, 0),
    rate_4 = ifelse(rate == 4, 1, 0),
    personal_1 = ifelse(personal == 1, 1, 0),
    personal_2 = ifelse(personal == 2, 1, 0),
    personal_3 = ifelse(personal == 3, 1, 0),
    personal_4 = ifelse(personal == 4, 1, 0),
    residence_1 = ifelse(residence == 1, 1, 0),
    residence_2 = ifelse(residence == 2, 1, 0),
    residence_3 = ifelse(residence == 3, 1, 0),
    residence_4 = ifelse(residence == 4, 1, 0),
    property_1 = ifelse(property == 1, 1, 0),
    property_2 = ifelse(property == 2, 1, 0),
    property_3 = ifelse(property == 3, 1, 0),
    property_4 = ifelse(property == 4, 1, 0),
    housing_1 = ifelse(housing == 1, 1, 0),
    housing_2 = ifelse(housing == 2, 1, 0),
    housing_3 = ifelse(housing == 3, 1, 0),
    credits_1 = ifelse(credits == 1, 1, 0),
    credits_2 = ifelse(credits == 2, 1, 0),
    credits_3 = ifelse(credits == 3, 1, 0),
    credits_4 = ifelse(credits == 4, 1, 0),
    job_1 = ifelse(job == 1, 1, 0),
    job_2 = ifelse(job == 2, 1, 0),
    job_3 = ifelse(job == 3, 1, 0),
    job_4 = ifelse(job == 4, 1, 0),
    amount = normalizacion(amount, amount_entrena_min, amount_entrena_max),
    age = normalizacion(age, age_entrena_min, age_entrena_max)
  ) %>% 
  select(!c(status, history, savings, employed, rate,
    personal, residence, property, housing, credits, job))

knn_exactitud_9 <- dataframe_exactitud_knn(
  var_exogenas_entrena_9,
  var_endogena_entrena,
  var_exogenas_valida_9,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_exactitud_9
plot(
  x = knn_exactitud_9$Valor_K,
  y = knn_exactitud_9$Promedios,
  xlab = 'Valores de K',
  ylab = 'Exactitud de modelo 9'
)

knn_especificidad_9 <- dataframe_especificidad_knn(
  var_exogenas_entrena_9,
  var_endogena_entrena,
  var_exogenas_valida_9,
  var_endogena_valida,
  k_max = 100,
  intentos = 20,
  por_promedio = TRUE
)
knn_especificidad_9
plot(
  x = knn_especificidad_9$Valor_K,
  y = knn_especificidad_9$Promedios,
  xlab = 'Valores de K',
  ylab = 'Especificidad de modelo 9'
)
# Exactitud: {30: 0.715; 31: 0.714, 28: 0.713, 27: 0.712, 32: 0.708}
# Especificidad: {2: 0.403, 1:0.397, 3: 0.321, 4: 0.316, 5: 0.300}


##### Resumen:
# Todas las variables exógenas sin modificación
# Exactitud: {31; 33: 0.700, 22: 0.697, 21: 0.696, 24; 30: 0.695}
# Especificidad: {1: 0.333, 2: 0.307, 3: 0.256, 4:0.208, 5: 0.199}

# Usando todas las variables que aparecen en alguno de los árboles
# Exactitud: {31: 0.700, 33: 0.699, 24: 0.697, 21; 22; 30: 0.696}
# Especificidad: {1: 0.282, 3:0.265, 2: 0.243, 7: 0.224, 6: 0.222}

# Usando variables que están en todos los árboles
# Exactitud: {31: 0.700, 33: 0.699, 30: 0.697, 21; 24: 0.696}
# Especificidad: {1: 0.282, 3:0.256, 2: 0.242, 7: 0.226, 6: 0.215}

# Usando todas las variables que aparecen en alguno de los árboles (amount norm)
# Exactitud: {14; 17: 0.718, 15; 20: 0.716, 19; 21; 48: 0.714}
# Especificidad: {1: 0.436, 2; 3:0.410, 5; 7: 0.404}

# Usando variables que están en todos los árboles (amount norm)
# Exactitud: {9: 0.736, 10: 0.735, 21: 0.733, 11; 35: 0.732}
# Especificidad: {3: 0.418, 2:0.412, 1: 0.410, 4: 0.401, 6: 0.376}

################################## Modelo 5 ###
# Usando variables que están en todos los árboles + savings + amount norm
# Exactitud: {48; 49; 65: 0.738, 47; 51; 66: 0.736} **
# Especificidad: {1: 0.474, 7:0.462, 6: 0.457, 3: 0.456, 2: 0.451} ****

################################## Modelo 6 ###
# Usando variables que están en todos los árboles + employed + amount norm
# Exactitud: {15: 0.733; 14: 0.730; 13: 0.729; 9; 29: 0.728} *
# Especificidad: {2: 0.453, 1:0.436, 6: 0.408, 3: 0.390, 8: 0.388} **

################################## Modelo 7 ###
# Usando variables que están en todos los árboles + rate + amount norm
# Exactitud: {6: 0.740, 13: 0.724, 5; 9; 14: 0.723} ***
# Especificidad: {1: 0.462, 2:0.424, 4; 6: 0.394, 3: 0.392} ***

################################## Modelo 8 ###
# Usando variables que están en todos los árboles + savings + rate + amount norm
# Exactitud: {13: 0.749, 31; 37: 0.736, 9; 30: 0.734} ****
# Especificidad: {7: 0.443, 5:0.440, 6: 0.437, 10: 0.431, 8: 0.413} *

# Usando todas las variables en binario + amount y age normalizadas
# Exactitud: {30: 0.715; 31: 0.714, 28: 0.713, 27: 0.712, 32: 0.708}
# Especificidad: {2: 0.403, 1:0.397, 3: 0.321, 4: 0.316, 5: 0.300}

# Se busca la mejor combinación variables - KNN
data.frame(
  knn_exactitud_7$Valor_K,
  knn_especificidad_7$Valor_K,
  knn_exactitud_7$Promedio,
  knn_especificidad_7$Promedio
)
#5: 0.723 + 0.365
#6: 0.740 + 0.394
#7: 0.721 + 0.347
#9: 0.723 + 0.322
#13: 0.724 + 0.335

data.frame(
  knn_exactitud_5$Valor_K,
  knn_especificidad_5$Valor_K,
  knn_exactitud_5$Promedio,
  knn_especificidad_5$Promedio
)
#1: 0.652 + 0.474 **
#7: 0.730 + 0.462 ***** 
#24: 0.734 + 0.426
#25: 0.728 + 0.436
#26: 0.726 + 0.412
#19: 0.730 + 0.372

data.frame(
  knn_exactitud_8$Valor_K,
  knn_especificidad_8$Valor_K,
  knn_exactitud_8$Promedio,
  knn_especificidad_8$Promedio
)
#5: 0.733 + 0.440
#7: 0.733 + 0.443
#9: 0.734 + 0.402
#11: 0.729 + 0.406
#12: 0.731 + 0.405
# 13: 0.749 + 0.389 **

data.frame(
  knn_exactitud_6$Valor_K,
  knn_especificidad_6$Valor_K,
  knn_exactitud_6$Promedio,
  knn_especificidad_6$Promedio
)
#9: 0.728 + 0.371
#10: 0.720 + 0.340
#13: 0.729 + 0.322
#14: 0.730 + 0.335
#15: 0.733 + 0.340
#16: 0.723 + 0.308
#17: 0.722 + 0.312
#18: 0.726 + 0.304
#20: 0.721 + 0.288

# Máx exactitud: status, history, savings, rate, property, amount norm + KNN 13
# Modelo 8: 0.749
# Máx especificidad: status, history, savings, property, amount norm + KNN 1
# Modelo 5: 0.474
# Mejor proporción: status, history, savings, property, amount norm + KNN 7
# Modelo 5: Exactitud 0.730; Especificidad 0.462


# Se trabaja en base de los datos del modelo 
knn_final <- knn(
  train = var_exogenas_entrena_5,
  test = var_exogenas_valida_5, 
  cl = var_endogena_entrena,
  k = 7
)
#matriz_confusion_knn_728 <- confusionMatrix(
#matriz_confusion_knn_732 <- confusionMatrix(
confusionMatrix(
  data = knn_final,
  reference = as.factor(var_endogena_valida),
  positive = '1'
)

# Estas variables hay que buscarlas, pues se definen una u otra aleatoriamente
matriz_confusion_knn_732
matriz_confusion_knn_728

# Las dos matrices de confusión 
grid.table(
  d = matriz_confusion_knn_728$table,
  #row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c('lightblue1', 'azure'), col = 'black')
    ),
    colhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white')
    ),
    rowhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white', fontface = 4L, hjust = 0, x = 0.3)
    )
  )
)

grid.table(
  d = matriz_confusion_knn_732$table,
  #row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c('lightblue1', 'azure'), col = 'black')
    ),
    colhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white')
    ),
    rowhead = list(
      bg_params = list(fill = 'deepskyblue2', col = 'black'),
      fg_params = list(col = 'white', fontface = 4L, hjust = 0, x = 0.3)
    )
  )
)

############################## Pregunta 4 ##############################
# Comparación de los modelos de las preguntss 3 y 4.
