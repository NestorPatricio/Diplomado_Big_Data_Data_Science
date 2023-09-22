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
  "caret",
  "rpart",
  "randomForest",
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

comparador_modelos <- tibble(
  modelo = character(),
  exactitud = numeric(),
  sensibilidad = numeric(),
  especificidad = numeric(),
  verdaderos_positivos = integer(),
  falsos_negativos = integer(),
  falsos_positivos = integer(),
  verdaderos_negativos = integer(),
  .rows = 0
)

##### Regresión logística #####
# Se declaran las variables que guardarán la suma
suma_vp <- 0
suma_fn <- 0
suma_fp <- 0
suma_vn <- 0

variables_no_significativas <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento <- dataset_ponderado[-particiones_10[[particion]], ] %>%
    select(!c(
      Fuga_Otra,
      Fuga_Disconforme,
      Fuga_por_Competencia,
      Fuga_Precio,
      Fuga_Servicio_Cliente
    )) %>%
    drop_na() # El modelo requiere no tener datos NA
  datos_validacion <- dataset_ponderado[particiones_10[[particion]], ] %>%
    select(!c(
      Fuga_Otra,
      Fuga_Disconforme,
      Fuga_por_Competencia,
      Fuga_Precio,
      Fuga_Servicio_Cliente
    )) %>%
    drop_na() # El modelo requiere no tener datos NA

  # Se entrena el modelo de la iteración
  modelo_auxiliar <- glm(
    formula = Fugado ~ .,
    data = datos_entrenamiento,
    family = binomial
  )

  # Se obtienen las variables que no aportan al modelo significativamente
  variables_auxiliar <- rownames(
    coef(summary(modelo_auxiliar))[
      coef(summary(modelo_auxiliar))[, "Pr(>|z|)"] > 0.05,
    ]
  )
  variables_no_significativas[[particion]] <- variables_auxiliar

  # Se genera la predicción
  prediccion_auxiliar <- predict(
    object = modelo_auxiliar,
    newdata = datos_validacion,
    type = "response"
  )
  prediccion_auxiliar <- ifelse(prediccion_auxiliar < 0.58, 0, 1)

  # Se genera la matriz de confusión
  matriz_auxiliar <- confusionMatrix(
    data = as.factor(prediccion_auxiliar),
    reference = as.factor(datos_validacion$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp <- suma_vp + matriz_auxiliar$table[1]
  suma_fn <- suma_fn + matriz_auxiliar$table[2]
  suma_fp <- suma_fp + matriz_auxiliar$table[3]
  suma_vn <- suma_vn + matriz_auxiliar$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp <- round(suma_vp / numero_partes)
promedio_fn <- round(suma_fn / numero_partes)
promedio_fp <- round(suma_fp / numero_partes)
promedio_vn <- round(suma_vn / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Regresión logística",
    exactitud = round(
      x = (promedio_vp + promedio_vn) /
        (promedio_vp + promedio_fn + promedio_fp + promedio_vn),
      digit = 4
    ),
    sensibilidad = round((promedio_vp) / (promedio_vp + promedio_fn), 4),
    especificidad = round((promedio_vn) / (promedio_fp + promedio_vn), 4),
    verdaderos_positivos = promedio_vp,
    falsos_negativos = promedio_fn,
    falsos_positivos = promedio_fp,
    verdaderos_negativos = promedio_vn
  )
)
view(comparador_modelos)

# Variables no significativas para todos los modelos de regresión logística
variables_no_significativas <- as.data.frame(
  do.call(cbind, variables_no_significativas)
)

##### Árbol de decisiones #####
# Se declaran las variables que guardarán la suma
suma_vp <- 0
suma_fn <- 0
suma_fp <- 0
suma_vn <- 0

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento <- dataset_ponderado[-particiones_10[[particion]], ]
  datos_validacion <- dataset_ponderado[particiones_10[[particion]], ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar <- rpart(
    formula = Fugado ~ .,
    data = datos_entrenamiento,
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

  # Se genera la predicción
  prediccion_auxiliar <- predict(
    object = modelo_auxiliar,
    newdata = datos_validacion,
    type = "class"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar <- confusionMatrix(
    data = as.factor(prediccion_auxiliar),
    reference = as.factor(datos_validacion$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp <- suma_vp + matriz_auxiliar$table[1]
  suma_fn <- suma_fn + matriz_auxiliar$table[2]
  suma_fp <- suma_fp + matriz_auxiliar$table[3]
  suma_vn <- suma_vn + matriz_auxiliar$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp <- round(suma_vp / numero_partes)
promedio_fn <- round(suma_fn / numero_partes)
promedio_fp <- round(suma_fp / numero_partes)
promedio_vn <- round(suma_vn / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Árbol de decisiones",
    exactitud = round(
      x = (promedio_vp + promedio_vn) /
        (promedio_vp + promedio_fn + promedio_fp + promedio_vn),
      digit = 4
    ),
    sensibilidad = round((promedio_vp) / (promedio_vp + promedio_fn), 4),
    especificidad = round((promedio_vn) / (promedio_fp + promedio_vn), 4),
    verdaderos_positivos = promedio_vp,
    falsos_negativos = promedio_fn,
    falsos_positivos = promedio_fp,
    verdaderos_negativos = promedio_vn
  )
)
view(comparador_modelos)

##### Random forest #####
# Se declaran las variables que guardarán la suma
suma_vp <- 0
suma_fn <- 0
suma_fp <- 0
suma_vn <- 0

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento <- dataset_ponderado[-particiones_10[[particion]], ] %>%
    select(!c(
      Fuga_Otra,
      Fuga_Disconforme,
      Fuga_por_Competencia,
      Fuga_Precio,
      Fuga_Servicio_Cliente
    )) %>%
    drop_na() # El modelo requiere no tener datos NA
  datos_validacion <- dataset_ponderado[particiones_10[[particion]], ] %>%
    select(!c(
      Fuga_Otra,
      Fuga_Disconforme,
      Fuga_por_Competencia,
      Fuga_Precio,
      Fuga_Servicio_Cliente
    )) %>%
    drop_na() # El modelo requiere no tener datos NA

  # Se entrena el modelo de la iteración
  modelo_auxiliar <- randomForest(
    x = datos_entrenamiento[, -24], # variables exógenas
    y = as.factor(datos_entrenamiento$Fugado), # variable endógena como factor
    ntree = 500, # número de árboles generados
    mtry = floor(sqrt(length(datos_entrenamiento_ex)))
    # número de variables escogidas aleatoreamente por división (5 en este caso)
  )

  # Se genera la predicción
  prediccion_auxiliar <- predict(
    object = modelo_auxiliar,
    newdata = datos_validacion,
    type = "response"
  )

  # Se genera la matriz de confusión
  matriz_auxiliar <- confusionMatrix(
    data = as.factor(prediccion_auxiliar),
    reference = as.factor(datos_validacion$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp <- suma_vp + matriz_auxiliar$table[1]
  suma_fn <- suma_fn + matriz_auxiliar$table[2]
  suma_fp <- suma_fp + matriz_auxiliar$table[3]
  suma_vn <- suma_vn + matriz_auxiliar$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp <- round(suma_vp / numero_partes)
promedio_fn <- round(suma_fn / numero_partes)
promedio_fp <- round(suma_fp / numero_partes)
promedio_vn <- round(suma_vn / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Random forest",
    exactitud = round(
      x = (promedio_vp + promedio_vn) /
        (promedio_vp + promedio_fn + promedio_fp + promedio_vn),
      digit = 4
    ),
    sensibilidad = round((promedio_vp) / (promedio_vp + promedio_fn), 4),
    especificidad = round((promedio_vn) / (promedio_fp + promedio_vn), 4),
    verdaderos_positivos = promedio_vp,
    falsos_negativos = promedio_fn,
    falsos_positivos = promedio_fp,
    verdaderos_negativos = promedio_vn
  )
)
view(comparador_modelos)

##### KNN #####
# Nestitor

##### Support Vector Machine #####
# Wladito

##### Naive Bayes #####
# Ramirito
