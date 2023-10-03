# Ciencia de datos y sus aplicaciones
# Proyecto 1

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "tidyr",
  "readxl",
  "xlsx",
  "caret",
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

dataset_original <- read_xls(
  path = "proyecto_1_datos_fuga.xls",
  range = cell_cols(2:28)
)

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


# Evaluación de los datasets ----------------------------------------------

##### Resumen de personas fugadas o no #####
numero_fugados <- tibble(
  Dataset = character(),
  Fugado_NO = integer(),
  Fugado_SI = integer(),
  Total_Dataset = integer(),
  Probab_Fugado = numeric()
) %>%
  add_row(
    Dataset = "Solo teléfono",
    Fugado_NO = summary(as.factor(dataset_telefono_sin_fuga$Fugado))[1],
    Fugado_SI = summary(as.factor(dataset_telefono_sin_fuga$Fugado))[2],
    Total_Dataset = Fugado_NO + Fugado_SI,
    Probab_Fugado = round(
      x = (Fugado_SI * 100) / (Fugado_SI + Fugado_NO),
      digit = 2
    )
  ) %>%
  add_row(
    Dataset = "Solo internet",
    Fugado_NO = summary(as.factor(dataset_internet_sin_fuga$Fugado))[1],
    Fugado_SI = summary(as.factor(dataset_internet_sin_fuga$Fugado))[2],
    Total_Dataset = Fugado_NO + Fugado_SI,
    Probab_Fugado = round(
      x = (Fugado_SI * 100) / (Fugado_SI + Fugado_NO),
      digit = 2
    )
  ) %>%
  add_row(
    Dataset = "Ambos servicios",
    Fugado_NO = summary(as.factor(dataset_ambos_sin_fuga$Fugado))[1],
    Fugado_SI = summary(as.factor(dataset_ambos_sin_fuga$Fugado))[2],
    Total_Dataset = Fugado_NO + Fugado_SI,
    Probab_Fugado = round(
      x = (Fugado_SI * 100) / (Fugado_SI + Fugado_NO),
      digit = 2
    )
  )

##### Dataset sólo teléfono #####
dataset_telefono_categoricas <- dataset_tratado %>%
  filter(`Servicio contratado` == 0) %>%
  mutate(
    Tipo_Contrato_más_largo = ifelse(
      test = Tipo_Contrato_mensual == 0 & Tipo_Contrato_anual == 0,
      yes = 1,
      no = 0
    ),
    Plan_Ninguno = ifelse(
      test = Plan_A == 0 & Plan_B == 0 & Plan_C == 0 & Plan_D == 0 &
        Plan_E == 0,
      yes = 1,
      no = 0
    )
  ) %>%
  select(
    Sexo,
    Casado,
    `Multiples lineas`,
    Plan_A,
    Plan_B,
    Plan_C,
    Plan_D,
    Plan_E,
    Plan_Ninguno,
    Tipo_Contrato_mensual,
    Tipo_Contrato_anual,
    Tipo_Contrato_más_largo,
    Fugado
  )

resumen_telefono_categorico <- tibble(
  Variable = names(dataset_telefono_categoricas[, -13]),
  Probabilidad_Fugado_NO = lapply(
    X = dataset_telefono_categoricas[
      dataset_telefono_categoricas$Fugado == 0,
      -13
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4),
  Probabilidad_Fugado_SI = lapply(
    X = dataset_telefono_categoricas[
      dataset_telefono_categoricas$Fugado == 1,
      -13
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4)
)

dataset_telefono_numericas <- dataset_categorizado %>%
  filter(`Servicio contratado` == 0) %>%
  select(
    Edad,
    `Numero Dependientes`,
    `Recomendaciones realizadas`,
    `Meses como Cliente`,
    `Cargo Mensual LLamadas`,
    `Cobro Mensual`,
    `Historico de Devoluciones`,
    `Historico Cargos Llamadas`,
    `Historico Cobro Acumulado`,
    Fugado
  )

resumen_telefono_numerico <- tibble(
  Variable = names(dataset_telefono_numericas[, -10]),
  Promedio_Fugado_NO = lapply(
    X = dataset_telefono_numericas[dataset_telefono_numericas$Fugado == 0, -10],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Promedio_Fugado_SI = lapply(
    X = dataset_telefono_numericas[dataset_telefono_numericas$Fugado == 1, -10],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Mediana_Fugado_NO = lapply(
    X = dataset_telefono_numericas[dataset_telefono_numericas$Fugado == 0, -10],
    FUN = median
  ) %>%
    unlist(),
  Mediana_Fugado_SI = lapply(
    X = dataset_telefono_numericas[dataset_telefono_numericas$Fugado == 1, -10],
    FUN = median
  ) %>%
    unlist()
)

resumen_telefono <- bind_rows(
  resumen_telefono_categorico,
  resumen_telefono_numerico
)

##### Dataset sólo internet #####
dataset_internet_categoricas <- dataset_tratado %>%
  filter(`Servicio contratado` == 1) %>%
  mutate(
    Tipo_Contrato_más_largo = ifelse(
      test = Tipo_Contrato_mensual == 0 & Tipo_Contrato_anual == 0,
      yes = 1,
      no = 0
    ),
    Plan_Ninguno = ifelse(
      test = Plan_A == 0 & Plan_B == 0 & Plan_C == 0 & Plan_D == 0 &
        Plan_E == 0,
      yes = 1,
      no = 0
    )
  ) %>%
  select(
    Sexo,
    Casado,
    `Servicio Adicional Antivirus`,
    `Servicio Respaldo en la Nube`,
    `Seguro Proteccion Equipo`,
    `Servicio Soporte Premium`,
    `Usa Streaming TV`,
    `Usa Streaming Peliculas`,
    `Usa Streaming Musica`,
    `Plan Ilimitado Datos`,
    Plan_A,
    Plan_B,
    Plan_C,
    Plan_D,
    Plan_E,
    Plan_Ninguno,
    Tipo_Contrato_mensual,
    Tipo_Contrato_anual,
    Tipo_Contrato_más_largo,
    Fugado
  )

resumen_internet_categorico <- tibble(
  Variable = names(dataset_internet_categoricas[, -20]),
  Probabilidad_Fugado_NO = lapply(
    X = dataset_internet_categoricas[
      dataset_internet_categoricas$Fugado == 0,
      -20
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4),
  Probabilidad_Fugado_SI = lapply(
    X = dataset_internet_categoricas[
      dataset_internet_categoricas$Fugado == 1,
      -20
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4)
)

dataset_internet_numericas <- dataset_categorizado %>%
  filter(`Servicio contratado` == 1) %>%
  select(
    Edad,
    `Numero Dependientes`,
    `Recomendaciones realizadas`,
    `Meses como Cliente`,
    `GB mensuales consumidos`,
    `Cobro Mensual`,
    `Historico de Devoluciones`,
    `Historico Cargos extra datos`,
    `Historico Cobro Acumulado`,
    Fugado
  )

resumen_internet_numerico <- tibble(
  Variable = names(dataset_internet_numericas[, -10]),
  Promedio_Fugado_NO = lapply(
    X = dataset_internet_numericas[dataset_internet_numericas$Fugado == 0, -10],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Promedio_Fugado_SI = lapply(
    X = dataset_internet_numericas[dataset_internet_numericas$Fugado == 1, -10],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Mediana_Fugado_NO = lapply(
    X = dataset_internet_numericas[dataset_internet_numericas$Fugado == 0, -10],
    FUN = median
  ) %>%
    unlist(),
  Mediana_Fugado_SI = lapply(
    X = dataset_internet_numericas[dataset_internet_numericas$Fugado == 1, -10],
    FUN = median
  ) %>%
    unlist()
)

resumen_internet <- bind_rows(
  resumen_internet_categorico,
  resumen_internet_numerico
)

##### Dataset ambos servicios #####
dataset_ambos_categoricas <- dataset_tratado %>%
  filter(`Servicio contratado` == 2) %>%
  mutate(
    Tipo_Contrato_más_largo = ifelse(
      test = Tipo_Contrato_mensual == 0 & Tipo_Contrato_anual == 0,
      yes = 1,
      no = 0
    ),
    Plan_Ninguno = ifelse(
      test = Plan_A == 0 & Plan_B == 0 & Plan_C == 0 & Plan_D == 0 &
        Plan_E == 0,
      yes = 1,
      no = 0
    )
  ) %>%
  select(
    Sexo,
    Casado,
    `Multiples lineas`,
    `Servicio Adicional Antivirus`,
    `Servicio Respaldo en la Nube`,
    `Seguro Proteccion Equipo`,
    `Servicio Soporte Premium`,
    `Usa Streaming TV`,
    `Usa Streaming Peliculas`,
    `Usa Streaming Musica`,
    `Plan Ilimitado Datos`,
    Plan_A,
    Plan_B,
    Plan_C,
    Plan_D,
    Plan_E,
    Plan_Ninguno,
    Tipo_Contrato_mensual,
    Tipo_Contrato_anual,
    Tipo_Contrato_más_largo,
    Fugado
  )

resumen_ambos_categorico <- tibble(
  Variable = names(dataset_ambos_categoricas[, -21]),
  Probabilidad_Fugado_NO = lapply(
    X = dataset_ambos_categoricas[
      dataset_ambos_categoricas$Fugado == 0,
      -21
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4),
  Probabilidad_Fugado_SI = lapply(
    X = dataset_ambos_categoricas[
      dataset_ambos_categoricas$Fugado == 1,
      -21
    ],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 4)
)

dataset_ambos_numericas <- dataset_categorizado %>%
  filter(`Servicio contratado` == 2) %>%
  select(
    Edad,
    `Numero Dependientes`,
    `Recomendaciones realizadas`,
    `Meses como Cliente`,
    `Cargo Mensual LLamadas`,
    `GB mensuales consumidos`,
    `Cobro Mensual`,
    `Historico de Devoluciones`,
    `Historico Cargos extra datos`,
    `Historico Cargos Llamadas`,
    `Historico Cobro Acumulado`,
    Fugado
  )

resumen_ambos_numerico <- tibble(
  Variable = names(dataset_ambos_numericas[, -12]),
  Promedio_Fugado_NO = lapply(
    X = dataset_ambos_numericas[dataset_ambos_numericas$Fugado == 0, -12],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Promedio_Fugado_SI = lapply(
    X = dataset_ambos_numericas[dataset_ambos_numericas$Fugado == 1, -12],
    FUN = mean
  ) %>%
    unlist() %>%
    round(digit = 2),
  Mediana_Fugado_NO = lapply(
    X = dataset_ambos_numericas[dataset_ambos_numericas$Fugado == 0, -12],
    FUN = median
  ) %>%
    unlist(),
  Mediana_Fugado_SI = lapply(
    X = dataset_ambos_numericas[dataset_ambos_numericas$Fugado == 1, -12],
    FUN = median
  ) %>%
    unlist()
)

resumen_ambos <- bind_rows(
  resumen_ambos_categorico,
  resumen_ambos_numerico
)


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

comparador_modelos_2 <- tibble(
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
  Observaciones = integer()
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
    formula = Fugado ~ Casado + Numero.Dependientes +
      Recomendaciones.realizadas + Meses.como.Cliente + Plan_B +
      Tipo_Contrato_mensual,
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
  prediccion_auxiliar_fono <- ifelse(prediccion_auxiliar_fono < 0.30, 0, 1)

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
comparador_modelos_2 <- bind_rows(
  comparador_modelos_2,
  tibble(
    Modelo = "Regresión logística teléfono",
    Exactitud = round(
      x = (promedio_vp_fono + promedio_vn_fono) /
        (promedio_vp_fono + promedio_fn_fono + promedio_fp_fono +
           promedio_vn_fono),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_fono / (promedio_vp_fono + promedio_fn_fono),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_fono / (promedio_fp_fono + promedio_vn_fono),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_fono) /
        (2 * promedio_vp_fono + promedio_fn_fono + promedio_fp_fono),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_fono,
    Falsos_Negativos = promedio_fn_fono,
    falsos_Positivos = promedio_fp_fono,
    Verdaderos_Negativos = promedio_vn_fono,
    Observaciones = promedio_vp_fono + promedio_fn_fono + promedio_fp_fono +
      promedio_vn_fono
  )
)

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
    formula = Fugado ~ Edad + Casado + Numero.Dependientes +
      Recomendaciones.realizadas + Meses.como.Cliente +
      Servicio.Adicional.Antivirus + Servicio.Soporte.Premium +
      Historico.Cobro.Acumulado + Plan_D + Tipo_Contrato_mensual +
      Tipo_Contrato_anual,
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
  prediccion_auxiliar_int <- ifelse(prediccion_auxiliar_int < 0.4, 0, 1)

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
comparador_modelos_2 <- bind_rows(
  comparador_modelos_2,
  tibble(
    Modelo = "Regresión logística internet",
    Exactitud = round(
      x = (promedio_vp_int + promedio_vn_int) /
        (promedio_vp_int + promedio_fn_int + promedio_fp_int +
           promedio_vn_int),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_int / (promedio_vp_int + promedio_fn_int),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_int / (promedio_fp_int + promedio_vn_int),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_int) /
        (2 * promedio_vp_int + promedio_fn_int + promedio_fp_int),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_int,
    Falsos_Negativos = promedio_fn_int,
    Falsos_Positivos = promedio_fp_int,
    Verdaderos_Negativos = promedio_vn_int,
    Observaciones = promedio_vp_int + promedio_fn_int + promedio_fp_int +
      promedio_vn_int
  )
)

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
modelos_dos <- list()

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
    formula = Fugado ~ Edad + Casado + Numero.Dependientes +
      Recomendaciones.realizadas + Meses.como.Cliente + Cargo.Mensual.LLamadas +
      Servicio.Adicional.Antivirus + Servicio.Respaldo.en.la.Nube +
      Seguro.Proteccion.Equipo + Servicio.Soporte.Premium +
      Usa.Streaming.Peliculas + Usa.Streaming.Musica + Cobro.Mensual +
      Historico.Cargos.Llamadas + Historico.Cobro.Acumulado + Plan_A + Plan_D +
      Plan_E + Tipo_Contrato_mensual + Tipo_Contrato_anual,
    data = datos_entrenamiento_dos,
    family = binomial
  )

  # Se obtiene la importancia de las variables
  significancia_variables <- coef(summary(modelo_auxiliar_dos)) %>%
    as_tibble(rownames = "id") %>%
    arrange(`Pr(>|z|)`)

  # Se guardan los modelos para evaluarlos después
  modelos_dos[[nombre_part]] <- modelo_auxiliar_dos

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
  prediccion_auxiliar_dos <- ifelse(prediccion_auxiliar_dos < 0.44, 0, 1)

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
comparador_modelos_2 <- bind_rows(
  comparador_modelos_2,
  tibble(
    Modelo = "Regresión logística ambos",
    Exactitud = round(
      x = (promedio_vp_dos + promedio_vn_dos) /
        (promedio_vp_dos + promedio_fn_dos + promedio_fp_dos +
           promedio_vn_dos),
      digit = 4
    ),
    Sensibilidad = round(
      x = promedio_vp_dos / (promedio_vp_dos + promedio_fn_dos),
      digit = 4
    ),
    Especificidad = round(
      x = promedio_vn_dos / (promedio_fp_dos + promedio_vn_dos),
      digit = 4
    ),
    Diferencia_SE = abs(Sensibilidad - Especificidad),
    Valor_F1 = round(
      x = (2 * promedio_vp_dos) /
        (2 * promedio_vp_dos + promedio_fn_dos + promedio_fp_dos),
      digit = 4
    ),
    Verdaderos_Positivos = promedio_vp_dos,
    Falsos_Negativos = promedio_fn_dos,
    Falsos_Positivos = promedio_fp_dos,
    Verdaderos_Negativos = promedio_vn_dos,
    Observaciones = promedio_vp_dos + promedio_fn_dos + promedio_fp_dos +
      promedio_vn_dos
  )
)

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
variables_fono <- sort(variables_significativas_fono[2:5, 3])
view(variables_significativas_int)
variables_int <- sort(variables_significativas_int[c(1, 3:6), 1])
view(variables_significativas_dos)
variables_dos <- sort(variables_significativas_dos[c(1:6, 8:13), 7])

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
  Solo_Telefono = variables_fono,
  Solo_Internet = variables_int,
  Ambos_Servicios = variables_dos
)


# Tablas importantes ------------------------------------------------------

##### Visualización de las tablas #####
view(numero_fugados)
view(comparador_modelos_2)
view(variables_significativas_lg_df)
view(resumen_telefono)
view(resumen_internet)
view(resumen_ambos)

##### Traspaso de las tablas a un archivo xlsx #####
write.xlsx(
  x = numero_fugados,
  file = "Datos y Modelos.xlsx",
  sheetName = "Cantidad_Fugados",
  append = FALSE, # Se crea el archivo
  showNA = FALSE
)
write.xlsx(
  x = comparador_modelos_2,
  file = "Datos y Modelos.xlsx",
  sheetName = "Rendimiento_Regresión_Logística",
  append = TRUE,
  showNA = FALSE
)
write.xlsx(
  x = variables_significativas_lg_df,
  file = "Datos y Modelos.xlsx",
  sheetName = "Variables_Significativas",
  append = TRUE,
  showNA = FALSE
)
write.xlsx(
  x = resumen_telefono,
  file = "Datos y Modelos.xlsx",
  sheetName = "Dataset_Teléfono",
  append = TRUE,
  showNA = FALSE
)
write.xlsx(
  x = resumen_internet,
  file = "Datos y Modelos.xlsx",
  sheetName = "Dataset_Internet",
  append = TRUE,
  showNA = FALSE
)
write.xlsx(
  x = resumen_ambos,
  file = "Datos y Modelos.xlsx",
  sheetName = "Dataset_Ambos_Servicios",
  append = TRUE,
  showNA = FALSE
)


# Experimentos ------------------------------------------------------------

dataset_telefono_categoricas2 <- tibble(
  Variable = character(),
  Probabilidad_Fuga = numeric()
)
for (variable in names(dataset_telefono_categoricas)) {
  if (variable != "Fugado") {
    for (valor in c(0, 1)) {
      vector <- dataset_telefono_categoricas[
        dataset_telefono_categoricas[, variable] == valor,
        "Fugado"
      ] %>%  unlist()
      probabilidad <- round(x = mean(vector) * 100, digit = 2)
      if (variable == "Sexo" & valor == 0) {
        texto <- "Mujer"
      } else if (variable == "Sexo" & valor == 1) {
        texto <- "Hombre"
      } else if (valor == 0) {
        texto <- paste("No", variable)
      } else {
        texto <- paste("Si", variable)
      }
      dataset_telefono_categoricas2 <- bind_rows(
        dataset_telefono_categoricas2,
        tibble(
          Variable = texto,
          Probabilidad_Fuga = probabilidad
        )
      )
    }
  }
}

dataset_internet_categoricas2 <- tibble(
  Variable = character(),
  Probabilidad_Fuga = numeric()
)
for (variable in names(dataset_internet_categoricas)) {
  if (variable != "Fugado") {
    for (valor in c(0, 1)) {
      vector <- dataset_internet_categoricas[
        dataset_internet_categoricas[, variable] == valor,
        "Fugado"
      ] %>%  unlist()
      probabilidad <- round(x = mean(vector) * 100, digit = 2)
      if (variable == "Sexo" & valor == 0) {
        texto <- "Mujer"
      } else if (variable == "Sexo" & valor == 1) {
        texto <- "Hombre"
      } else if (valor == 0) {
        texto <- paste("No", variable)
      } else {
        texto <- paste("Si", variable)
      }
      dataset_internet_categoricas2 <- bind_rows(
        dataset_internet_categoricas2,
        tibble(
          Variable = texto,
          Probabilidad_Fuga = probabilidad
        )
      )
    }
  }
}

dataset_ambos_categoricas2 <- tibble(
  Variable = character(),
  Probabilidad_Fuga = numeric()
)
for (variable in names(dataset_ambos_categoricas)) {
  if (variable != "Fugado") {
    for (valor in c(0, 1)) {
      vector <- dataset_ambos_categoricas[
        dataset_ambos_categoricas[, variable] == valor,
        "Fugado"
      ] %>%  unlist()
      probabilidad <- round(x = mean(vector) * 100, digit = 2)
      if (variable == "Sexo" & valor == 0) {
        texto <- "Mujer"
      } else if (variable == "Sexo" & valor == 1) {
        texto <- "Hombre"
      } else if (valor == 0) {
        texto <- paste("No", variable)
      } else {
        texto <- paste("Si", variable)
      }
      dataset_ambos_categoricas2 <- bind_rows(
        dataset_ambos_categoricas2,
        tibble(
          Variable = texto,
          Probabilidad_Fuga = probabilidad
        )
      )
    }
  }
}

view(dataset_telefono_categoricas2)
view(dataset_internet_categoricas2)
view(dataset_ambos_categoricas2)

# Se usa el modelo de la séptima iteración como estándar para presentar
modelos_dos$`Fold 7`
