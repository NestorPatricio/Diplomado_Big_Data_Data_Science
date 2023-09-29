# Ciencia de datos y sus aplicaciones
# Proyecto 1

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "tidyr",
  "ggplot2",
  "patchwork",
  "readxl",
  "xlsx",
  "caret",
  "rpart",
  "randomForest",
  "class",
  "e1071",
  "nnet",
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
lint(filename = "Selección de modelo.R")


# Funciones ---------------------------------------------------------------

normalizacion <- function(valores) {
  # Realiza normalización de los datos
  minimo <-  min(valores, na.rm = TRUE)
  maximo <- max(valores, na.rm = TRUE)
  return((valores - minimo) / (maximo - minimo))
}

probab_fugado_si <- function(grupos) {
  vector <- dataset_categorizado$Fugado[grupos]
  return(x = sum(vector) / length(vector))
}


# Carga de datos y primera transformación ---------------------------------

dataset_original <- tibble(read_xls(
  path = "proyecto_1_datos_fuga.xls",
  range = cell_cols(2:28)
))

dataset_categorizado <- dataset_original %>%
  mutate(
    Sexo = ifelse(Sexo == "Mujer", 0, 1), # Hombre: 0, Mujer: 1
    Casado = ifelse(Casado == "No", 0, 1), # No: 0, Si: 1
    Plan = case_match(
      Plan,
      "Ninguno" ~ 0,
      "Plan A" ~ 1,
      "Plan B" ~ 2,
      "Plan C" ~ 3,
      "Plan D" ~ 4,
      "Plan E" ~ 5
    ),
    `Multiples lineas` = ifelse(`Multiples lineas` == "Si", 1, 0),
    `Servicio Internet` = ifelse(`Servicio Internet` == "Si", 1, 0),
    `Servicio Adicional Antivirus` = ifelse(
      `Servicio Adicional Antivirus` == "Si",
      1,
      0
    ),
    `Servicio Respaldo en la Nube` = ifelse(
      `Servicio Respaldo en la Nube` == "Si",
      1,
      0
    ),
    `Seguro Proteccion Equipo` = ifelse(
      `Seguro Proteccion Equipo` == "Si",
      1,
      0
    ),
    `Servicio Soporte Premium` = ifelse(
      `Servicio Soporte Premium` == "Si",
      1,
      0
    ),
    `Usa Streaming TV` = ifelse(`Usa Streaming TV` == "Si", 1, 0),
    `Usa Streaming Peliculas` = ifelse(
      `Usa Streaming Peliculas` == "Si",
      1,
      0
    ),
    `Usa Streaming Musica` = ifelse(`Usa Streaming Musica` == "Si", 1, 0),
    `Plan Ilimitado Datos` = ifelse(`Plan Ilimitado Datos` == "Si", 1, 0),
    `Tipo Contrato` = case_match(
      `Tipo Contrato`,
      "Mensual" ~ 1,
      "Anual" ~ 2,
      "Otro mas largo" ~ 3
    ),
    Fugado = ifelse(Fugado == "Si", 1, 0),
    `Causa Fuga` = case_match(
      `Causa Fuga`,
      "Otra" ~ 0,
      "Disconforme" ~ 1,
      "Mejor la Competencia" ~ 2,
      "Por Precio" ~ 3,
      "Servicio Cliente" ~ 4
    ),
    `Servicio contratado` = case_when(
      `Servicio Internet` == 0 ~ 0,
      is.na(`Cargo Mensual LLamadas`) ~ 1,
      `Servicio Internet` == 1 & !is.na(`Cargo Mensual LLamadas`) ~ 2
    ) # Sólo teléfono: 0, Sólo internet: 1, Ambos servicios: 2
  )

dataset_tratado <- dataset_categorizado %>%
  mutate(
    Plan_A = ifelse(Plan == 1, 1, 0),
    Plan_B = ifelse(Plan == 2, 1, 0),
    Plan_C = ifelse(Plan == 3, 1, 0),
    Plan_D = ifelse(Plan == 4, 1, 0),
    Plan_E = ifelse(Plan == 5, 1, 0),
    Tipo_Contrato_mensual = ifelse(`Tipo Contrato` == 1, 1, 0),
    Tipo_Contrato_anual = ifelse(`Tipo Contrato` == 2, 1, 0),
    Fuga_Otra = ifelse(`Causa Fuga` == 0, 1, 0),
    Fuga_Disconforme = ifelse(`Causa Fuga` == 1, 1, 0),
    Fuga_por_Competencia = ifelse(`Causa Fuga` == 2, 1, 0),
    Fuga_Precio = ifelse(`Causa Fuga` == 3, 1, 0),
    Fuga_Servicio_Cliente = ifelse(`Causa Fuga` == 4, 1, 0)
  ) %>%
  select(!c(Plan, `Tipo Contrato`, `Causa Fuga`))

dataset_ponderado <- dataset_tratado %>%
  select(!`Servicio contratado`) %>%
  lapply(FUN = normalizacion) %>%
  as.data.frame() %>%
  mutate(
    Plan_A = Plan_A / 6,
    Plan_B = Plan_B / 6,
    Plan_C = Plan_C / 6,
    Plan_D = Plan_D / 6,
    Plan_E = Plan_E / 6,
    Tipo_Contrato_mensual = Tipo_Contrato_mensual / 3,
    Tipo_Contrato_anual = Tipo_Contrato_anual / 3,
    Fuga_Otra = Fuga_Otra / 5,
    Fuga_Disconforme = Fuga_Disconforme / 5,
    Fuga_por_Competencia = Fuga_por_Competencia / 5,
    Fuga_Precio = Fuga_Precio / 5,
    Fuga_Servicio_Cliente = Fuga_Servicio_Cliente / 5
  )

# Se genera el dataset sin variables binarias y sin los valores NA
dataset_original2 <- dataset_original %>%
  mutate(
    Sexo = factor(
      x = dataset_original$Sexo,
      levels = unique(dataset_original$Sexo)
    ),
    Casado = factor(
      x = dataset_original$Casado,
      levels = unique(dataset_original$Casado)
    ),
    Plan = factor(
      x = dataset_original$Plan,
      levels = unique(dataset_original$Plan)
    ),
    `Multiples lineas` = factor(
      x = dataset_original$`Multiples lineas`,
      levels = unique(dataset_original$`Multiples lineas`)
    ),
    `Servicio Internet` = factor(
      x = dataset_original$`Servicio Internet`,
      levels = unique(dataset_original$`Servicio Internet`)
    ),
    `Servicio Adicional Antivirus` = factor(
      x = dataset_original$`Servicio Adicional Antivirus`,
      levels = unique(dataset_original$`Servicio Adicional Antivirus`)
    ),
    `Servicio Respaldo en la Nube` = factor(
      x = dataset_original$`Servicio Respaldo en la Nube`,
      levels = unique(dataset_original$`Servicio Respaldo en la Nube`)
    ),
    `Seguro Proteccion Equipo` = factor(
      x = dataset_original$`Seguro Proteccion Equipo`,
      levels = unique(dataset_original$`Seguro Proteccion Equipo`)
    ),
    `Servicio Soporte Premium` = factor(
      x = dataset_original$`Servicio Soporte Premium`,
      levels = unique(dataset_original$`Servicio Soporte Premium`)
    ),
    `Usa Streaming TV` = factor(
      x = dataset_original$`Usa Streaming TV`,
      levels = unique(dataset_original$`Usa Streaming TV`)
    ),
    `Usa Streaming Peliculas` = factor(
      x = dataset_original$`Usa Streaming Peliculas`,
      levels = unique(dataset_original$`Usa Streaming Peliculas`)
    ),
    `Usa Streaming Musica` = factor(
      x = dataset_original$`Usa Streaming Musica`,
      levels = unique(dataset_original$`Usa Streaming Musica`)
    ),
    `Plan Ilimitado Datos` = factor(
      x = dataset_original$`Plan Ilimitado Datos`,
      levels = unique(dataset_original$`Plan Ilimitado Datos`)
    ),
    `Tipo Contrato` = factor(
      x = dataset_original$`Tipo Contrato`,
      levels = unique(dataset_original$`Tipo Contrato`)
    ),
    Fugado = factor(
      x = dataset_original$Fugado,
      levels = unique(dataset_original$Fugado)
    ),
  ) %>%
  select(!c(`Causa Fuga`))
dataset_variables_asignadas1 <- rfImpute(
  x = Fugado ~ .,
  data = dataset_original2,
  iter = 6,
  ntree = 300
)

# Se genera el dataset con variables binarias y sin los valores NA
dataset_ponderado_2 <- dataset_ponderado %>%
  mutate(Fugado = factor(
    x = dataset_original$Fugado,
    levels = unique(dataset_original$Fugado)
  )) %>%
  select(!c(
    Fuga_Otra,
    Fuga_Disconforme,
    Fuga_por_Competencia,
    Fuga_Precio,
    Fuga_Servicio_Cliente
  ))
dataset_variables_asignadas2 <- rfImpute(
  x = Fugado ~ .,
  data = dataset_ponderado_2,
  iter = 6,
  ntree = 300
)

# Exploración de los datos ------------------------------------------------

dataset_categorizado %>% summary()
# Fugado (Probabilidad) 28,37%
# Número de Dependientes (Probabilidad) < 25%
# Múltiples lìneas (Probabilidad) 49,22%
# Cobro Mensual (Mediana/Promedio) $36.946/$33.816
# Historico Cargos extra datos (Promedio) $3.728
# Historico Cargos Llamadas (Mediana/Promedio) $245.794/$415.005
# Historico Cobro Acumulado (Mediana/Promedio) $1.235.754/$1.682.313

# Resumen de las variables según los distintos servicios contratados
dataset_categorizado %>% filter(`Servicio contratado` == 0) %>% summary()
# Fugado (Probabilidad) 8,41%
# Número de Dependientes (Probabilidad) > 25%
# Múltiples lìneas (Probabilidad) 24,78%
# Cobro Mensual (Mediana/Promedio) $10.478/$10.771
# Historico Cargos extra datos (Promedio) $0
# Historico Cargos Llamadas (Mediana/Promedio) $313.118/$452.173
# Historico Cobro Acumulado (Mediana/Promedio) $670.558/$840.592

dataset_categorizado %>% filter(`Servicio contratado` == 1) %>% summary()
# Fugado (Probabilidad) 26,40%
# Número de Dependientes (Probabilidad) < 25%
# Múltiples lìneas (Probabilidad) NA
# Cobro Mensual (Mediana/Promedio) $21.359/$21.830
# Historico Cargos extra datos (Promedio) $4.917
# Historico Cargos Llamadas (Mediana/Promedio) $0/$0
# Historico Cobro Acumulado (Mediana/Promedio) $671.463/$826.965

dataset_categorizado %>% filter(`Servicio contratado` == 2) %>% summary()
# Fugado (Probabilidad) 34,47%
# Número de Dependientes (Probabilidad) < 25%
# Múltiples lìneas (Probabilidad) 56,36%
# Cobro Mensual (Mediana/Promedio) $43.550/$42.225
# Historico Cargos extra datos (Promedio) $4.651
# Historico Cargos Llamadas (Mediana/Promedio) $302.640/$462.236
# Historico Cobro Acumulado (Mediana/Promedio) $1.756.971/$2.047.912

# Resumen de las variables según Fuga
dataset_categorizado %>% filter(Fugado == 0) %>% summary()
dataset_categorizado %>% filter(Fugado == 1) %>% summary()

# Inspección visual de las variables numéricas por Fugado
boxplot_edad <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = Edad)) +
  theme_minimal()

boxplot_n_depedientes <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Numero Dependientes`)) +
  theme_minimal()

boxplot_recomendaciones <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Recomendaciones realizadas`)) +
  theme_minimal()

boxplot_meses <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Meses como Cliente`)) +
  theme_minimal()

boxplot_mensual_llamadas <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Cargo Mensual LLamadas`)) +
  theme_minimal()

boxplot_mensual_gb <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `GB mensuales consumidos`)) +
  theme_minimal()

boxplot_cobro_mensual <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Cobro Mensual`)) +
  theme_minimal()

boxplot_historico_devoluciones <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Historico de Devoluciones`)) +
  theme_minimal()

boxplot_cargos_extra <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Historico Cargos extra datos`)) +
  theme_minimal()

boxplot_hist_cargo_llamadas <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Historico Cargos Llamadas`)) +
  theme_minimal()

boxplot_hist_cobro_acumulado <- ggplot(
  dataset_categorizado,
  mapping = aes(x = Fugado, group = Fugado)
) +
  geom_boxplot(mapping = aes(y = `Historico Cobro Acumulado`)) +
  theme_minimal()

(boxplot_edad + boxplot_recomendaciones + boxplot_n_depedientes) /
  (boxplot_cargos_extra + boxplot_mensual_gb + boxplot_mensual_llamadas) /
  (boxplot_hist_cobro_acumulado + boxplot_hist_cargo_llamadas) /
  (boxplot_historico_devoluciones + boxplot_cobro_mensual + boxplot_meses)

# Sin correlación a la visualización de las variables
#dataset_categorizado %>% select(
#  Edad, `Numero Dependientes`, `Recomendaciones realizadas`,
#  `Meses como Cliente`, `Cargo Mensual LLamadas`, `GB mensuales consumidos`,
#  `Cobro Mensual`, `Historico de Devoluciones`, `Historico Cargos extra datos`,
#  `Historico Cargos Llamadas`, `Historico Cobro Acumulado`, Fugado,
#  `Servicio contratado`
#) %>% plot()


# Generación de los grupos para Cross-validation --------------------------

set.seed(seed = 111)

particiones_10 <- createFolds(y = dataset_ponderado$Fugado, k = 10)
numero_partes <- length(particiones_10)

# Evaluación de los grupos generados
matriz_grupos <- tibble(
  grupo = c("Todos los datos", names(particiones_10)),
  registros = c(nrow(dataset_categorizado), lapply(particiones_10, length)),
  prob_Fugado_SI = c(
    round(x = mean(dataset_categorizado$Fugado), digit = 4),
    lapply(lapply(particiones_10, probab_fugado_si), round, digit = 4)
  )
)
view(matriz_grupos)


# Evaluación de modelos ---------------------------------------------------

comparador_modelos_1 <- tibble(
  Modelo = character(),
  Exactitud = numeric(),
  Sensibilidad = numeric(),
  Especificidad = numeric(),
  Diferencia_SE = numeric(),
  Valor_F1 = numeric(),
  Verdaderos_Positivos = integer(),
  Falsos_Negativos = integer(),
  Falsos_Positivos = integer(),
  Verdaderos_Negativos = integer(),
  Observaciones = integer(),
  .rows = 0
)

##### Regresión logística #####
# Se declaran las variables que guardarán la suma
suma_vp_rl <- 0
suma_fn_rl <- 0
suma_fp_rl <- 0
suma_vn_rl <- 0

variables_no_significativas_rl <- list()
variables_significativas_rl <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_rl <- dataset_variables_asignadas2[
    -particiones_10[[particion]],
  ]
  datos_validacion_rl <- dataset_variables_asignadas2[
    particiones_10[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_rl <- glm(
    formula = Fugado ~ .,
    data = datos_entrenamiento_rl,
    family = binomial
  )

  # Se obtiene la importancia de las variables
  significancia_variables <- coef(summary(modelo_auxiliar_rl)) %>%
    as_tibble(rownames = "id") %>%
    arrange(`Pr(>|z|)`)

  # Se obtienen las variables que no aportan al modelo significativamente
  nombre_part <- paste("Fold", as.character(particion))
  variables_no_significativas_rl[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` > 0.05) %>%
    select(id) %>%
    unlist()

  # Se obtienen las variables más significativas del modelo
  variables_significativas_rl[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` < 0.001) %>%
    arrange(desc(`Pr(>|z|)`)) %>%
    select(id) %>%
    unlist()

  # Se genera la predicción
  prediccion_auxiliar_rl <- predict(
    object = modelo_auxiliar_rl,
    newdata = datos_validacion_rl,
    type = "response"
  )
  prediccion_auxiliar_rl <- ifelse(prediccion_auxiliar_rl < 0.50, "No", "Si")

  # Se genera la matriz de confusión
  matriz_auxiliar_rl <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_rl),
    reference = datos_validacion_rl$Fugado,
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_rl <- suma_vp_rl + matriz_auxiliar_rl$table[1]
  suma_fn_rl <- suma_fn_rl + matriz_auxiliar_rl$table[2]
  suma_fp_rl <- suma_fp_rl + matriz_auxiliar_rl$table[3]
  suma_vn_rl <- suma_vn_rl + matriz_auxiliar_rl$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_rl <- round(suma_vp_rl / numero_partes)
promedio_fn_rl <- round(suma_fn_rl / numero_partes)
promedio_fp_rl <- round(suma_fp_rl / numero_partes)
promedio_vn_rl <- round(suma_vn_rl / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Regresión logística",
    Exactitud = round(
      x = (promedio_vp_rl + promedio_vn_rl) /
        (promedio_vp_rl + promedio_fn_rl + promedio_fp_rl + promedio_vn_rl),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_rl / (promedio_vp_rl + promedio_fn_rl),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_rl / (promedio_fp_rl + promedio_vn_rl),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_rl) /
        (2 * promedio_vp_rl + promedio_fn_rl + promedio_fp_rl),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_rl,
    Falsos_Negativos = promedio_fn_rl,
    Falsos_Positivos = promedio_fp_rl,
    Verdaderos_Negativos = promedio_vn_rl,
    Observaciones = promedio_vp_rl + promedio_fn_rl + promedio_fp_rl +
      promedio_vn_rl
  )
)

# Variables no significativas para todos los modelos de regresión logística
# Pr(> |z|) > 0.05
maximo_variables_no_significativas_rl <- lapply(
  X = variables_no_significativas_rl,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_no_significativas_rl <- data.frame(lapply(
  X = variables_no_significativas_rl,
  FUN = `length<-`,
  maximo_variables_no_significativas_rl
))

# Variables significativas para todos los modelos de regresión logística
# Pr(> |z|) < 0.001
maximo_variables_significativas_rl <- lapply(
  X = variables_significativas_rl,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_significativas_rl <- data.frame(lapply(
  X = variables_significativas_rl,
  FUN = `length<-`,
  maximo_variables_significativas_rl
))

##### Árbol de decisiones #####
# Se declaran las variables que guardarán la suma
suma_vp_ad <- 0
suma_fn_ad <- 0
suma_fp_ad <- 0
suma_vn_ad <- 0

variables_significativas_ad <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_ad <- dataset_variables_asignadas1[
    -particiones_10[[particion]],
  ]
  datos_validacion_ad <- dataset_variables_asignadas1[
    particiones_10[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_ad <- rpart(
    formula = Fugado ~ .,
    data = datos_entrenamiento_ad,
    method = "class",
    parms = list(split = "information"),
    model = TRUE,
    control = rpart.control(
      cp = 0.01,
      minsplit = 20,
      minbucket = round(x = 20 / 3, digit = 0),
      maxdepth = 30
    )
  )

  # Se obtienen las variables significativas del modelo
  variables_significativas_ad[[particion]] <- names(
    x = modelo_auxiliar_ad$variable.importance
  )

  # Se genera la predicción
  prediccion_auxiliar_ad <- predict(
    object = modelo_auxiliar_ad,
    newdata = datos_validacion_ad,
    type = "class"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar_ad <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_ad),
    reference = as.factor(datos_validacion_ad$Fugado),
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_ad <- suma_vp_ad + matriz_auxiliar_ad$table[1]
  suma_fn_ad <- suma_fn_ad + matriz_auxiliar_ad$table[2]
  suma_fp_ad <- suma_fp_ad + matriz_auxiliar_ad$table[3]
  suma_vn_ad <- suma_vn_ad + matriz_auxiliar_ad$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_ad <- round(suma_vp_ad / numero_partes)
promedio_fn_ad <- round(suma_fn_ad / numero_partes)
promedio_fp_ad <- round(suma_fp_ad / numero_partes)
promedio_vn_ad <- round(suma_vn_ad / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Árbol de decisiones",
    Exactitud = round(
      x = (promedio_vp_ad + promedio_vn_ad) /
        (promedio_vp_ad + promedio_fn_ad + promedio_fp_ad + promedio_vn_ad),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_ad / (promedio_vp_ad + promedio_fn_ad),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_ad / (promedio_fp_ad + promedio_vn_ad),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_ad) /
        (2 * promedio_vp_ad + promedio_fn_ad + promedio_fp_ad),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_ad,
    Falsos_Negativos = promedio_fn_ad,
    Falsos_Positivos = promedio_fp_ad,
    Verdaderos_Negativos = promedio_vn_ad,
    Observaciones = promedio_vp_ad + promedio_fn_ad + promedio_fp_ad +
      promedio_vn_ad
  )
)

# Variables significativas para todos los modelos de árboles de decisiones
maximo_variables_significativas_ad <- lapply(
  X = variables_significativas_ad,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_significativas_ad <- data.frame(lapply(
  X = variables_significativas_ad,
  FUN = `length<-`,
  maximo_variables_significativas_ad
))

##### Random forest #####
# Se declaran las variables que guardarán la suma
suma_vp_rf <- 0
suma_fn_rf <- 0
suma_fp_rf <- 0
suma_vn_rf <- 0

variables_significativas_rf <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_rf <- dataset_variables_asignadas1[
    -particiones_10[[particion]],
  ]
  datos_validacion_rf <- dataset_variables_asignadas1[
    particiones_10[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_rf <- randomForest(
    x = datos_entrenamiento_rf[, -1], # variables exógenas
    y = datos_entrenamiento_rf$Fugado, # variable endógena como factor
    ntree = 500, # número de árboles generados
    mtry = floor(sqrt(length(datos_entrenamiento_rf[, -1])))
    # número de variables escogidas aleatoreamente por división (5 en este caso)
  )

  # Se obtienen las variables significativas del modelo
  variables_significativas_rf[[particion]] <- names(
    x = modelo_auxiliar_rf$importance[
      order(-modelo_auxiliar_rf$importance[, 1]),
    ][1:14]
  )

  # Se genera la predicción
  prediccion_auxiliar_rf <- predict(
    object = modelo_auxiliar_rf,
    newdata = datos_validacion_rf,
    type = "response"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar_rf <- confusionMatrix(
    data = prediccion_auxiliar_rf,
    reference = datos_validacion_rf$Fugado,
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_rf <- suma_vp_rf + matriz_auxiliar_rf$table[1]
  suma_fn_rf <- suma_fn_rf + matriz_auxiliar_rf$table[2]
  suma_fp_rf <- suma_fp_rf + matriz_auxiliar_rf$table[3]
  suma_vn_rf <- suma_vn_rf + matriz_auxiliar_rf$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_rf <- round(suma_vp_rf / numero_partes)
promedio_fn_rf <- round(suma_fn_rf / numero_partes)
promedio_fp_rf <- round(suma_fp_rf / numero_partes)
promedio_vn_rf <- round(suma_vn_rf / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Random forest",
    Exactitud = round(
      x = (promedio_vp_rf + promedio_vn_rf) /
        (promedio_vp_rf + promedio_fn_rf + promedio_fp_rf + promedio_vn_rf),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_rf / (promedio_vp_rf + promedio_fn_rf),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_rf / (promedio_fp_rf + promedio_vn_rf),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_rf) /
        (2 * promedio_vp_rf + promedio_fn_rf + promedio_fp_rf),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_rf,
    Falsos_Negativos = promedio_fn_rf,
    Falsos_Positivos = promedio_fp_rf,
    Verdaderos_Negativos = promedio_vn_rf,
    Observaciones = promedio_vp_rf + promedio_fn_rf + promedio_fp_rf +
      promedio_vn_rf
  )
)

# Variables significativas para todos los modelos de random forest
variables_significativas_rf <- as.data.frame(variables_significativas_rf)

##### KNN #####
# Evaluación de las variables significativas de Regresión logística, Árboles de
#decisiones y Random forest

view(variables_significativas_rl)
variables_rl <- sort(variables_significativas_rl[c(2:9, 11:13), 1])
view(variables_significativas_ad)
variables_ad <- sort(variables_significativas_ad[c(1:12, 16:18), 1])
view(variables_significativas_rf)
variables_rf <- sort(variables_significativas_rf[c(1:13), 1])
maxima_variables <- max(
  length(variables_rl),
  length(variables_ad),
  length(variables_rf)
)

variables_rl <- c(
  variables_rl,
  rep(NA, maxima_variables - length(variables_rl))
)
variables_ad <- c(
  variables_ad,
  rep(NA, maxima_variables - length(variables_ad))
)
variables_rf <- c(
  variables_rf,
  rep(NA, maxima_variables - length(variables_rf))
)

variables_significativas_df <- tibble(
  regresion_logistica = variables_rl,
  arbol_decisiones = variables_ad,
  random_forest = variables_rf
)
view(variables_significativas_df)

# Variables exógenas comunes a todos los modelos
variables_significativas_1 <- unname(unlist(
  variables_significativas_df[c(3:5, 6:11), 1]
))

# Se prueban los distintos valores de K
valores_k <- 50

comparador_knn_1 <- tibble(
  valor_k = character(),
  exactitud = numeric(),
  sensibilidad = numeric(),
  especificidad = numeric(),
  dif_sen_esp = numeric(),
  valor_F1 = numeric(),
  verdaderos_positivos = integer(),
  falsos_negativos = integer(),
  falsos_positivos = integer(),
  verdaderos_negativos = integer(),
  observaciones = integer(),
  .rows = 0
)

for (valor_k in 1:valores_k) {
  # Se declaran las variables que guardarán la suma
  suma_vp_knn <- 0
  suma_fn_knn <- 0
  suma_fp_knn <- 0
  suma_vn_knn <- 0

  for (particion in 1:numero_partes) {
    # Se generan los datasets de entrenamiento y validación de cada iteración
    datos_entrenamiento_knn <- dataset_variables_asignadas2[
      -particiones_10[[particion]],
    ]
    datos_entrenamiento_ex_knn <- datos_entrenamiento_knn %>%
      select(all_of(variables_significativas_1))
    datos_entrenamiento_en_knn <- datos_entrenamiento_knn %>%
      select(Fugado) %>%
      unlist()

    datos_validacion_knn <- dataset_variables_asignadas2[
      particiones_10[[particion]],
    ]
    datos_validacion_ex_knn <- datos_validacion_knn %>%
      select(all_of(variables_significativas_1))
    datos_validacion_en_knn <- datos_validacion_knn %>%
      select(Fugado) %>%
      unlist()

    # Se entrena el modelo de la iteración
    modelo_auxiliar_knn <- knn(
      train = datos_entrenamiento_ex_knn,
      test = datos_validacion_ex_knn,
      cl = datos_entrenamiento_en_knn, # Debe ser un vector
      k = valor_k
    )

    # Se genera la matriz de confusión
    matriz_auxiliar_knn <- confusionMatrix(
      data = as.factor(modelo_auxiliar_knn),
      reference = datos_validacion_en_knn,
      positive = "Si"
    )

    # Se suman VP, VN, FP y FN
    suma_vp_knn <- suma_vp_knn + matriz_auxiliar_knn$table[1]
    suma_fn_knn <- suma_fn_knn + matriz_auxiliar_knn$table[2]
    suma_fp_knn <- suma_fp_knn + matriz_auxiliar_knn$table[3]
    suma_vn_knn <- suma_vn_knn + matriz_auxiliar_knn$table[4]
  }

  # Se promedian los valores por el número de partes
  promedio_vp_knn <- round(suma_vp_knn / numero_partes)
  promedio_fn_knn <- round(suma_fn_knn / numero_partes)
  promedio_fp_knn <- round(suma_fp_knn / numero_partes)
  promedio_vn_knn <- round(suma_vn_knn / numero_partes)

  comparador_knn_1 <- bind_rows(
    comparador_knn_1,
    tibble(
      valor_k = as.character(valor_k),
      exactitud = round(
        x = (promedio_vp_knn + promedio_vn_knn) /
          (promedio_vp_knn + promedio_fn_knn + promedio_fp_knn +
             promedio_vn_knn),
        digit = 4
      ),
      sensibilidad = round(
        x = promedio_vp_knn / (promedio_vp_knn + promedio_fn_knn),
        digit = 4
      ),
      especificidad = round(
        x = promedio_vn_knn / (promedio_fp_knn + promedio_vn_knn),
        digit = 4
      ),
      dif_sen_esp = abs(sensibilidad - especificidad),
      valor_F1 = round(
        x = (2 * promedio_vp_knn) /
          (2 * promedio_vp_knn + promedio_fn_knn + promedio_fp_knn),
        digit = 4
      ),
      verdaderos_positivos = promedio_vp_knn,
      falsos_negativos = promedio_fn_knn,
      falsos_positivos = promedio_fp_knn,
      verdaderos_negativos = promedio_vn_knn,
      observaciones = promedio_vp_knn + promedio_fn_knn + promedio_fp_knn +
        promedio_vn_knn
    )
  ) %>%
    arrange(-valor_F1)
}

# Evaluación de los distintos conjuntos de variables exógenas
view(comparador_knn_1)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = paste(comparador_knn_1$valor_k[[1]], "vecinos cercanos"),
    Exactitud = comparador_knn_1$exactitud[[1]],
    Sensibilidad = comparador_knn_1$sensibilidad[[1]],
    Especificidad = comparador_knn_1$especificidad[[1]],
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = comparador_knn_1$valor_F1[[1]],
    Verdaderos_Positivos = comparador_knn_1$verdaderos_positivos[[1]],
    Falsos_Negativos = comparador_knn_1$falsos_negativos[[1]],
    Falsos_Positivos = comparador_knn_1$falsos_positivos[[1]],
    Verdaderos_Negativos = comparador_knn_1$verdaderos_negativos[[1]],
    Observaciones = comparador_knn_1$observaciones[[1]]
  )
)

##### Support Vector Machine #####
# Se declaran las variables que guardarán la suma
suma_vp_svm <- 0
suma_fn_svm <- 0
suma_fp_svm <- 0
suma_vn_svm <- 0

variables_significativas_svm <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_svm <- dataset_variables_asignadas1[
    -particiones_10[[particion]],
  ]
  datos_validacion_svm <- dataset_variables_asignadas1[
    particiones_10[[particion]],
  ]

  modelo_svm <- tune(
    METHOD = svm,
    train.x = Fugado ~ .,
    data = datos_entrenamiento_svm,
    kernel = "linear",
    cost = 0.1,
    scale = TRUE
  )
  modelo_auxiliar_svm <- modelo_svm$best.model

  # Se genera la predicción
  prediccion_auxiliar_svm <- predict(
    object = modelo_auxiliar_svm,
    newdata = datos_validacion_svm,
    type = "class"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar_svm <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_svm),
    reference = as.factor(datos_validacion_svm$Fugado),
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_svm <- suma_vp_svm + matriz_auxiliar_svm$table[1]
  suma_fn_svm <- suma_fn_svm + matriz_auxiliar_svm$table[2]
  suma_fp_svm <- suma_fp_svm + matriz_auxiliar_svm$table[3]
  suma_vn_svm <- suma_vn_svm + matriz_auxiliar_svm$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_svm <- round(suma_vp_svm / numero_partes)
promedio_fn_svm <- round(suma_fn_svm / numero_partes)
promedio_fp_svm <- round(suma_fp_svm / numero_partes)
promedio_vn_svm <- round(suma_vn_svm / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Support vector machines",
    Exactitud = round(
      x = (promedio_vp_svm + promedio_vn_svm) /
        (promedio_vp_svm + promedio_fn_svm + promedio_fp_svm + promedio_vn_svm),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_svm / (promedio_vp_svm + promedio_fn_svm),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_svm / (promedio_fp_svm + promedio_vn_svm),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_svm) /
        (2 * promedio_vp_svm + promedio_fn_svm + promedio_fp_svm),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_svm,
    Falsos_Negativos = promedio_fn_svm,
    Falsos_Positivos = promedio_fp_svm,
    Verdaderos_Negativos = promedio_vn_svm,
    Observaciones = promedio_vp_svm + promedio_fn_svm + promedio_fp_svm +
      promedio_vn_svm
  )
)

##### Naive Bayes Gaussiano #####
# Se declaran las variables que guardarán la suma
suma_vp_nb <- 0
suma_fn_nb <- 0
suma_fp_nb <- 0
suma_vn_nb <- 0

variables_significativas_nb <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_nb <- dataset_variables_asignadas2[
    -particiones_10[[particion]],
  ]
  datos_validacion_nb <- dataset_variables_asignadas2[
    particiones_10[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_nb <- naiveBayes(
    formula = Fugado ~ .,
    data = datos_entrenamiento_nb,
    na.action = na.omit
  )

  # Se genera la predicción
  prediccion_auxiliar_nb <- predict(
    object = modelo_auxiliar_nb,
    newdata = datos_validacion_nb,
    type = "class"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar_nb <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_nb),
    reference = datos_validacion_nb$Fugado,
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_nb <- suma_vp_nb + matriz_auxiliar_nb$table[1]
  suma_fn_nb <- suma_fn_nb + matriz_auxiliar_nb$table[2]
  suma_fp_nb <- suma_fp_nb + matriz_auxiliar_nb$table[3]
  suma_vn_nb <- suma_vn_nb + matriz_auxiliar_nb$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_nb <- round(suma_vp_nb / numero_partes)
promedio_fn_nb <- round(suma_fn_nb / numero_partes)
promedio_fp_nb <- round(suma_fp_nb / numero_partes)
promedio_vn_nb <- round(suma_vn_nb / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Naive Bayes Gaussiano",
    Exactitud = round(
      x = (promedio_vp_nb + promedio_vn_nb) /
        (promedio_vp_nb + promedio_fn_nb + promedio_fp_nb + promedio_vn_nb),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_nb / (promedio_vp_nb + promedio_fn_nb),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_nb / (promedio_fp_nb + promedio_vn_nb),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_nb) /
        (2 * promedio_vp_nb + promedio_fn_nb + promedio_fp_nb),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_nb,
    Falsos_Negativos = promedio_fn_nb,
    Falsos_Positivos = promedio_fp_nb,
    Verdaderos_Negativos = promedio_vn_nb,
    Observaciones = promedio_vp_nb + promedio_fn_nb + promedio_fp_nb +
      promedio_vn_nb
  )
)

##### Redes neuronales artificiales #####
# Se declaran las variables que guardarán la suma
suma_vp_nn <- 0
suma_fn_nn <- 0
suma_fp_nn <- 0
suma_vn_nn <- 0

variables_significativas_nn <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_nn <- dataset_variables_asignadas2[
    -particiones_10[[particion]],
  ]
  datos_validacion_nn <- dataset_variables_asignadas2[
    particiones_10[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_nn <- nnet(
    formula = Fugado ~ .,
    data = datos_entrenamiento_nn,
    size = 31,
    maxit = 1000
  )

  # Se genera la predicción
  prediccion_auxiliar_nn <- predict(
    object = modelo_auxiliar_nn,
    newdata = datos_validacion_nn,
    type = "raw"
  )
  prediccion_auxiliar_nn <- ifelse(prediccion_auxiliar_nn < 0.5, "No", "Si")

  # Se genera la matriz de confusión
  matriz_auxiliar_nn <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_nn),
    reference = datos_validacion_nn$Fugado,
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_nn <- suma_vp_nn + matriz_auxiliar_nn$table[1]
  suma_fn_nn <- suma_fn_nn + matriz_auxiliar_nn$table[2]
  suma_fp_nn <- suma_fp_nn + matriz_auxiliar_nn$table[3]
  suma_vn_nn <- suma_vn_nn + matriz_auxiliar_nn$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_nn <- round(suma_vp_nn / numero_partes)
promedio_fn_nn <- round(suma_fn_nn / numero_partes)
promedio_fp_nn <- round(suma_fp_nn / numero_partes)
promedio_vn_nn <- round(suma_vn_nn / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Red neuronal (1 x 31)",
    Exactitud = round(
      x = (promedio_vp_nn + promedio_vn_nn) /
        (promedio_vp_nn + promedio_fn_nn + promedio_fp_nn + promedio_vn_nn),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_nn / (promedio_vp_nn + promedio_fn_nn),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_nn / (promedio_fp_nn + promedio_vn_nn),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_nn) /
        (2 * promedio_vp_nn + promedio_fn_nn + promedio_fp_nn),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_nn,
    Falsos_Negativos = promedio_fn_nn,
    Falsos_Positivos = promedio_fp_nn,
    Verdaderos_Negativos = promedio_vn_nn,
    Observaciones = promedio_vp_nn + promedio_fn_nn + promedio_fp_nn +
      promedio_vn_nn
  )
)


# Tablas importantes ------------------------------------------------------

##### Visualización de las tablas #####
view(comparador_modelos_1)

##### Traspaso de las tablas a un archivo xlsx #####
write.xlsx(
  x = comparador_modelos_1,
  file = "Datos y Modelos.xlsx",
  sheetName = "Rendimiento_Modelos",
  append = TRUE, # Se crea el archivo
  showNA = FALSE
)

# Experimentos ------------------------------------------------------------

##### Support Vector Machine 2 #####
# Se declaran las variables que guardarán la suma
suma_vp_svm2 <- 0
suma_fn_svm2 <- 0
suma_fp_svm2 <- 0
suma_vn_svm2 <- 0

variables_significativas_svm2 <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_svm2 <- dataset_variables_asignadas1[
    -particiones_10[[particion]],
  ]
  datos_validacion_svm2 <- dataset_variables_asignadas1[
    particiones_10[[particion]],
  ]

  modelo_svm2 <- fit(
    x = Fugado ~ .,
    data = datos_entrenamiento_svm,
    model = "svm",
    kernel = "vanilladot",
    C = 0.1
  )

  # Se obtienen las variables significativas del modelo
  importancia_svm2 <- Importance(modelo_svm2, data = datos_entrenamiento_svm2)
  importancia_df_svm2 <- tibble(
    variable = names(datos_entrenamiento_svm2),
    importancia = importancia_svm2$imp
  ) %>%
    arrange(-importancia) %>%
    head(n = 14)
  variables_significativas_svm2[[particion]] <- importancia_df_svm2 %>%
    select(variable) %>%
    unlist()

  # Se genera la predicción
  prediccion_probabilidad_svm2 <- predict(
    object = modelo_svm2,
    newdata = datos_validacion_svm2,
    type = "class"
  ) > 0.5
  prediccion_probabilidad_svm2 <- as_tibble(prediccion_probabilidad_svm2)
  prediccion_probabilidad_svm2 <- prediccion_probabilidad_svm2 %>%
    mutate(
      valor = ifelse(No == TRUE, "No", "Si"),
      valor = factor(x = valor, levels = c("No", "Si"))
    )
  prediccion_auxiliar_svm2 <- prediccion_probabilidad_svm2$valor

  # Se genera la matriz de confusión
  matriz_auxiliar_svm2 <- confusionMatrix(
    data = prediccion_auxiliar_svm2,
    reference = datos_validacion_svm2$Fugado,
    positive = "Si"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_svm2 <- suma_vp_svm2 + matriz_auxiliar_svm2$table[1]
  suma_fn_svm2 <- suma_fn_svm2 + matriz_auxiliar_svm2$table[2]
  suma_fp_svm2 <- suma_fp_svm2 + matriz_auxiliar_svm2$table[3]
  suma_vn_svm2 <- suma_vn_svm2 + matriz_auxiliar_svm2$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_svm2 <- round(suma_vp_svm2 / numero_partes)
promedio_fn_svm2 <- round(suma_fn_svm2 / numero_partes)
promedio_fp_svm2 <- round(suma_fp_svm2 / numero_partes)
promedio_vn_svm2 <- round(suma_vn_svm2 / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos_1 <- bind_rows(
  comparador_modelos_1,
  tibble(
    Modelo = "Support vector machines 2",
    Exactitud = round(
      x = (promedio_vp_svm2 + promedio_vn_svm2) /
        (promedio_vp_svm + promedio_fn_svm2 + promedio_fp_svm2 +
           promedio_vn_svm2),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_svm2 / (promedio_vp_svm2 + promedio_fn_svm2),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_svm2 / (promedio_fp_svm2 + promedio_vn_svm2),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_svm2) /
        (2 * promedio_vp_svm2 + promedio_fn_svm2 + promedio_fp_svm2),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_svm2,
    Falsos_Negativos = promedio_fn_svm2,
    Falsos_Positivos = promedio_fp_svm2,
    Verdaderos_Negativos = promedio_vn_svm2,
    Observaciones = promedio_vp_svm2 + promedio_fn_svm2 + promedio_fp_svm2 +
      promedio_vn_svm2
  )
)
view(comparador_modelos_1)
