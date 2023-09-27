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
  #"rpart",
  #"randomForest",
  #"class",
  #"e1071",
  #"nnet",
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
lint(filename = "Ajuste de modelo.R")


# Funciones ---------------------------------------------------------------

normalizacion <- function(valores) {
  # Realiza normalización de los datos
  minimo <-  min(valores, na.rm = TRUE)
  maximo <- max(valores, na.rm = TRUE)
  return((valores - minimo) / (maximo - minimo))
}

probab_fugado_si <- function(grupos, dataset) {
  vector <- dataset$Fugado[grupos]
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

# Se separan los dataset según el servicio contratado, sin Causa de fuga
dataset_telefono <- dataset_ponderado %>%
  filter(Servicio.contratado == 0) %>%
  select(!c(
    Servicio.Internet,
    GB.mensuales.consumidos,
    Servicio.Adicional.Antivirus,
    Servicio.Respaldo.en.la.Nube,
    Seguro.Proteccion.Equipo,
    Servicio.Soporte.Premium,
    Usa.Streaming.TV,
    Usa.Streaming.Peliculas,
    Usa.Streaming.Musica,
    Plan.Ilimitado.Datos,
    Historico.Cargos.extra.datos,
    Servicio.contratado
  ))
dataset_telefono_sin_fuga <- dataset_telefono %>%
  select(!c(
    Fuga_Otra,
    Fuga_Disconforme,
    Fuga_por_Competencia,
    Fuga_Precio,
    Fuga_Servicio_Cliente
  ))

dataset_internet <- dataset_ponderado %>%
  filter(Servicio.contratado == 0.5) %>%
  select(!c(
    Servicio.Internet,
    Cargo.Mensual.LLamadas,
    Multiples.lineas,
    Historico.Cargos.Llamadas,
    Servicio.contratado
  ))
dataset_internet_sin_fuga <- dataset_internet %>%
  select(!c(
    Fuga_Otra,
    Fuga_Disconforme,
    Fuga_por_Competencia,
    Fuga_Precio,
    Fuga_Servicio_Cliente
  ))

dataset_ambos <- dataset_ponderado %>%
  filter(Servicio.contratado == 1) %>%
  select(!c(
    Servicio.Internet,
    Servicio.contratado
  ))
dataset_ambos_sin_fuga <- dataset_ambos %>%
  select(!c(
    Fuga_Otra,
    Fuga_Disconforme,
    Fuga_por_Competencia,
    Fuga_Precio,
    Fuga_Servicio_Cliente
  ))


# Generación de los grupos para Cross-validation --------------------------

set.seed(seed = 961169)
numero_partes <- 10

particiones_telefono <- createFolds(
  y = dataset_telefono_sin_fuga$Fugado,
  k = numero_partes
)
particiones_internet <- createFolds(
  y = dataset_internet_sin_fuga$Fugado,
  k = numero_partes
)
particiones_ambos <- createFolds(
  y = dataset_ambos_sin_fuga$Fugado,
  k = numero_partes
)

# Evaluación de los grupos generados
partes_telefono <- tibble(
  grupo = c("Todos los datos", names(particiones_telefono)),
  registros = c(
    nrow(dataset_telefono_sin_fuga),
    lapply(particiones_telefono, length) %>% unlist()
  ),
  prob_Fugado_SI = c(
    round(x = mean(dataset_telefono_sin_fuga$Fugado), digit = 4),
    lapply(
      X = lapply(
        X = particiones_telefono,
        FUN = probab_fugado_si,
        dataset = dataset_telefono_sin_fuga
      ),
      FUN = round,
      digit = 4
    ) %>% unlist()
  )
)
partes_internet <- tibble(
  grupo = c("Todos los datos", names(particiones_internet)),
  registros = c(
    nrow(dataset_internet_sin_fuga),
    lapply(particiones_internet, length) %>% unlist()
  ),
  prob_Fugado_SI = c(
    round(x = mean(dataset_internet_sin_fuga$Fugado), digit = 4),
    lapply(
      X = lapply(
        X = particiones_internet,
        FUN = probab_fugado_si,
        dataset = dataset_internet_sin_fuga
      ),
      FUN = round,
      digit = 4
    ) %>% unlist()
  )
)
partes_ambos <- tibble(
  grupo = c("Todos los datos", names(particiones_ambos)),
  registros = c(
    nrow(dataset_ambos_sin_fuga),
    lapply(particiones_ambos, length) %>% unlist()
  ),
  prob_Fugado_SI = c(
    round(x = mean(dataset_ambos_sin_fuga$Fugado), digit = 4),
    lapply(
      X = lapply(
        X = particiones_ambos,
        FUN = probab_fugado_si,
        dataset = dataset_ambos_sin_fuga
      ),
      FUN = round,
      digit = 4
    ) %>% unlist()
  )
)
summary(partes_telefono$prob_Fugado_SI)
summary(partes_internet$prob_Fugado_SI)
summary(partes_ambos$prob_Fugado_SI)

sort(partes_telefono$prob_Fugado_SI)
sort(partes_internet$prob_Fugado_SI)
sort(partes_ambos$prob_Fugado_SI)


# Evaluación de modelos ---------------------------------------------------

comparador_modelos <- tibble(
  modelo = character(),
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

##### Regresión logística teléfono #####
# Se declaran las variables que guardarán la suma
suma_vp_fono <- 0
suma_fn_fono <- 0
suma_fp_fono <- 0
suma_vn_fono <- 0

variables_no_significativas_fono <- list()
variables_significativas_fono <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_fono <- dataset_telefono_sin_fuga[
    -particiones_telefono[[particion]],
  ]
  datos_validacion_fono <- dataset_telefono_sin_fuga[
    particiones_telefono[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_fono <- glm(
    formula = Fugado ~ .,
    data = datos_entrenamiento_fono,
    family = binomial
  )

  # Se obtiene la importancia de las variables
  significancia_variables <- coef(summary(modelo_auxiliar_fono)) %>%
    as_tibble(rownames = "id") %>%
    arrange(`Pr(>|z|)`)

  # Se obtienen las variables que no aportan al modelo significativamente
  nombre_part <- paste("Fold", as.character(particion))
  variables_no_significativas_fono[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` > 0.05) %>%
    select(id) %>%
    unlist()

  # Se obtienen las variables más significativas del modelo
  variables_significativas_fono[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` < 0.001) %>%
    arrange(desc(`Pr(>|z|)`)) %>%
    select(id) %>%
    unlist()

  # Se genera la predicción
  prediccion_auxiliar_fono <- predict(
    object = modelo_auxiliar_fono,
    newdata = datos_validacion_fono,
    type = "response"
  )
  prediccion_auxiliar_fono <- ifelse(prediccion_auxiliar_fono < 0.50, 0, 1)

  # Se genera la matriz de confusión
  matriz_auxiliar_fono <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_fono),
    reference = as.factor(datos_validacion_fono$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_fono <- suma_vp_fono + matriz_auxiliar_fono$table[1]
  suma_fn_fono <- suma_fn_fono + matriz_auxiliar_fono$table[2]
  suma_fp_fono <- suma_fp_fono + matriz_auxiliar_fono$table[3]
  suma_vn_fono <- suma_vn_fono + matriz_auxiliar_fono$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_fono <- round(suma_vp_fono / numero_partes)
promedio_fn_fono <- round(suma_fn_fono / numero_partes)
promedio_fp_fono <- round(suma_fp_fono / numero_partes)
promedio_vn_fono <- round(suma_vn_fono / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Regresión logística teléfono",
    exactitud = round(
      x = (promedio_vp_fono + promedio_vn_fono) /
        (promedio_vp_fono + promedio_fn_fono + promedio_fp_fono +
           promedio_vn_fono),
      digit = 4
    ),
    sensibilidad = round(
      x = promedio_vp_fono / (promedio_vp_fono + promedio_fn_fono),
      digit = 4
    ),
    especificidad = round(
      x = promedio_vn_fono / (promedio_fp_fono + promedio_vn_fono),
      digit = 4
    ),
    dif_sen_esp = abs(sensibilidad - especificidad),
    valor_F1 = round(
      x = (2 * promedio_vp_fono) /
        (2 * promedio_vp_fono + promedio_fn_fono + promedio_fp_fono),
      digit = 4
    ),
    verdaderos_positivos = promedio_vp_fono,
    falsos_negativos = promedio_fn_fono,
    falsos_positivos = promedio_fp_fono,
    verdaderos_negativos = promedio_vn_fono,
    observaciones = promedio_vp_fono + promedio_fn_fono + promedio_fp_fono +
      promedio_vn_fono
  )
)
view(comparador_modelos)

# Variables no significativas para todos los modelos de regresión logística
# Pr(> |z|) > 0.05
maximo_variables_no_significativas_fono <- lapply(
  X = variables_no_significativas_fono,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_no_significativas_fono <- data.frame(lapply(
  X = variables_no_significativas_fono,
  FUN = `length<-`,
  maximo_variables_no_significativas_fono
))

# Variables significativas para todos los modelos de regresión logística
# Pr(> |z|) < 0.001
maximo_variables_significativas_fono <- lapply(
  X = variables_significativas_fono,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_significativas_fono <- data.frame(lapply(
  X = variables_significativas_fono,
  FUN = `length<-`,
  maximo_variables_significativas_fono
))

##### Regresión logística internet #####
# Se declaran las variables que guardarán la suma
suma_vp_int <- 0
suma_fn_int <- 0
suma_fp_int <- 0
suma_vn_int <- 0

variables_no_significativas_int <- list()
variables_significativas_int <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_int <- dataset_internet_sin_fuga[
    -particiones_internet[[particion]],
  ]
  datos_validacion_int <- dataset_internet_sin_fuga[
    particiones_internet[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_int <- glm(
    formula = Fugado ~ .,
    data = datos_entrenamiento_int,
    family = binomial
  )

  # Se obtiene la importancia de las variables
  significancia_variables <- coef(summary(modelo_auxiliar_int)) %>%
    as_tibble(rownames = "id") %>%
    arrange(`Pr(>|z|)`)

  # Se obtienen las variables que no aportan al modelo significativamente
  nombre_part <- paste("Fold", as.character(particion))
  variables_no_significativas_int[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` > 0.05) %>%
    select(id) %>%
    unlist()

  # Se obtienen las variables más significativas del modelo
  variables_significativas_int[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` < 0.001) %>%
    arrange(desc(`Pr(>|z|)`)) %>%
    select(id) %>%
    unlist()

  # Se genera la predicción
  prediccion_auxiliar_int <- predict(
    object = modelo_auxiliar_int,
    newdata = datos_validacion_int,
    type = "response"
  )
  prediccion_auxiliar_int <- ifelse(prediccion_auxiliar_int < 0.50, 0, 1)

  # Se genera la matriz de confusión
  matriz_auxiliar_int <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_int),
    reference = as.factor(datos_validacion_int$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_int <- suma_vp_int + matriz_auxiliar_int$table[1]
  suma_fn_int <- suma_fn_int + matriz_auxiliar_int$table[2]
  suma_fp_int <- suma_fp_int + matriz_auxiliar_int$table[3]
  suma_vn_int <- suma_vn_int + matriz_auxiliar_int$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_int <- round(suma_vp_int / numero_partes)
promedio_fn_int <- round(suma_fn_int / numero_partes)
promedio_fp_int <- round(suma_fp_int / numero_partes)
promedio_vn_int <- round(suma_vn_int / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Regresión logística internet",
    exactitud = round(
      x = (promedio_vp_int + promedio_vn_int) /
        (promedio_vp_int + promedio_fn_int + promedio_fp_int +
           promedio_vn_int),
      digit = 4
    ),
    sensibilidad = round(
      x = promedio_vp_int / (promedio_vp_int + promedio_fn_int),
      digit = 4
    ),
    especificidad = round(
      x = promedio_vn_int / (promedio_fp_int + promedio_vn_int),
      digit = 4
    ),
    dif_sen_esp = abs(sensibilidad - especificidad),
    valor_F1 = round(
      x = (2 * promedio_vp_int) /
        (2 * promedio_vp_int + promedio_fn_int + promedio_fp_int),
      digit = 4
    ),
    verdaderos_positivos = promedio_vp_int,
    falsos_negativos = promedio_fn_int,
    falsos_positivos = promedio_fp_int,
    verdaderos_negativos = promedio_vn_int,
    observaciones = promedio_vp_int + promedio_fn_int + promedio_fp_int +
      promedio_vn_int
  )
)
view(comparador_modelos)

# Variables no significativas para todos los modelos de regresión logística
# Pr(> |z|) > 0.05
maximo_variables_no_significativas_int <- lapply(
  X = variables_no_significativas_int,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_no_significativas_int <- data.frame(lapply(
  X = variables_no_significativas_int,
  FUN = `length<-`,
  maximo_variables_no_significativas_int
))

# Variables significativas para todos los modelos de regresión logística
# Pr(> |z|) < 0.001
maximo_variables_significativas_int <- lapply(
  X = variables_significativas_int,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_significativas_int <- data.frame(lapply(
  X = variables_significativas_int,
  FUN = `length<-`,
  maximo_variables_significativas_int
))

##### Regresión logística ambos servicios #####
# Se declaran las variables que guardarán la suma
suma_vp_dos <- 0
suma_fn_dos <- 0
suma_fp_dos <- 0
suma_vn_dos <- 0

variables_no_significativas_dos <- list()
variables_significativas_dos <- list()

for (particion in 1:numero_partes) {
  # Se generan los datasets de entrenamiento y validación de cada iteración
  datos_entrenamiento_dos <- dataset_ambos_sin_fuga[
    -particiones_ambos[[particion]],
  ]
  datos_validacion_dos <- dataset_ambos_sin_fuga[
    particiones_ambos[[particion]],
  ]

  # Se entrena el modelo de la iteración
  modelo_auxiliar_dos <- glm(
    formula = Fugado ~ .,
    data = datos_entrenamiento_dos,
    family = binomial
  )

  # Se obtiene la importancia de las variables
  significancia_variables <- coef(summary(modelo_auxiliar_dos)) %>%
    as_tibble(rownames = "id") %>%
    arrange(`Pr(>|z|)`)

  # Se obtienen las variables que no aportan al modelo significativamente
  nombre_part <- paste("Fold", as.character(particion))
  variables_no_significativas_dos[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` > 0.05) %>%
    select(id) %>%
    unlist()

  # Se obtienen las variables más significativas del modelo
  variables_significativas_dos[[nombre_part]] <- significancia_variables %>%
    filter(`Pr(>|z|)` < 0.001) %>%
    arrange(desc(`Pr(>|z|)`)) %>%
    select(id) %>%
    unlist()

  # Se genera la predicción
  prediccion_auxiliar_dos <- predict(
    object = modelo_auxiliar_dos,
    newdata = datos_validacion_dos,
    type = "response"
  )
  prediccion_auxiliar_dos <- ifelse(prediccion_auxiliar_dos < 0.50, 0, 1)

  # Se genera la matriz de confusión
  matriz_auxiliar_dos <- confusionMatrix(
    data = as.factor(prediccion_auxiliar_dos),
    reference = as.factor(datos_validacion_dos$Fugado),
    positive = "1"
  )

  # Se suman VP, VN, FP y FN
  suma_vp_dos <- suma_vp_dos + matriz_auxiliar_dos$table[1]
  suma_fn_dos <- suma_fn_dos + matriz_auxiliar_dos$table[2]
  suma_fp_dos <- suma_fp_dos + matriz_auxiliar_dos$table[3]
  suma_vn_dos <- suma_vn_dos + matriz_auxiliar_dos$table[4]
}

# Se promedian los valores por el número de partes
promedio_vp_dos <- round(suma_vp_dos / numero_partes)
promedio_fn_dos <- round(suma_fn_dos / numero_partes)
promedio_fp_dos <- round(suma_fp_dos / numero_partes)
promedio_vn_dos <- round(suma_vn_dos / numero_partes)

# Se insertan en la tabla para comparar los modelos
comparador_modelos <- bind_rows(
  comparador_modelos,
  tibble(
    modelo = "Regresión logística ambos",
    exactitud = round(
      x = (promedio_vp_dos + promedio_vn_dos) /
        (promedio_vp_dos + promedio_fn_dos + promedio_fp_dos +
           promedio_vn_dos),
      digit = 4
    ),
    sensibilidad = round(
      x = promedio_vp_dos / (promedio_vp_dos + promedio_fn_dos),
      digit = 4
    ),
    especificidad = round(
      x = promedio_vn_dos / (promedio_fp_dos + promedio_vn_dos),
      digit = 4
    ),
    dif_sen_esp = abs(sensibilidad - especificidad),
    valor_F1 = round(
      x = (2 * promedio_vp_dos) /
        (2 * promedio_vp_dos + promedio_fn_dos + promedio_fp_dos),
      digit = 4
    ),
    verdaderos_positivos = promedio_vp_dos,
    falsos_negativos = promedio_fn_dos,
    falsos_positivos = promedio_fp_dos,
    verdaderos_negativos = promedio_vn_dos,
    observaciones = promedio_vp_dos + promedio_fn_dos + promedio_fp_dos +
      promedio_vn_dos
  )
)
view(comparador_modelos)

# Variables no significativas para todos los modelos de regresión logística
# Pr(> |z|) > 0.05
maximo_variables_no_significativas_dos <- lapply(
  X = variables_no_significativas_dos,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_no_significativas_dos <- data.frame(lapply(
  X = variables_no_significativas_dos,
  FUN = `length<-`,
  maximo_variables_no_significativas_dos
))

# Variables significativas para todos los modelos de regresión logística
# Pr(> |z|) < 0.001
maximo_variables_significativas_dos <- lapply(
  X = variables_significativas_dos,
  FUN = length
) %>%
  unlist() %>%
  max()
variables_significativas_dos <- data.frame(lapply(
  X = variables_significativas_dos,
  FUN = `length<-`,
  maximo_variables_significativas_dos
))


# Variables más importantes -----------------------------------------------

# Evaluación de las variables significativas de Regresión logística, Árboles de
#decisiones y Random forest

view(variables_significativas_fono)
variables_fono <- sort(variables_significativas_fono[4, 1])
view(variables_significativas_int)
variables_int <- sort(variables_significativas_int[c(1:3), 1])
view(variables_significativas_dos)
variables_dos <- sort(variables_significativas_dos[c(3:7, 9:15), 1])
maxima_variables_lg <- max(
  length(variables_fono),
  length(variables_int),
  length(variables_dos)
)

variables_fono <- c(
  variables_fono,
  rep(NA, maxima_variables_lg - length(variables_fono))
)
variables_int <- c(
  variables_int,
  rep(NA, maxima_variables_lg - length(variables_int))
)
variables_dos <- c(
  variables_dos,
  rep(NA, maxima_variables_lg - length(variables_dos))
)

variables_significativas_lg_df <- tibble(
  solo_telefono = variables_fono,
  solo_internet = variables_int,
  ambos_servicios = variables_dos
)
view(variables_significativas_lg_df)
