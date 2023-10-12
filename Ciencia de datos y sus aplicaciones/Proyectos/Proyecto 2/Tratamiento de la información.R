# Ciencia de datos y sus aplicaciones
# Proyecto 2

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "readxl",
  "xlsx",
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
    "Ciencia de datos y sus aplicaciones\\Proyectos\\Proyecto 2"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Ciencia de ",
    "datos y sus aplicaciones/Proyectos/Proyecto 2/"
  ))
}

# Se ejecuta el linter de R para evaluar el estilo del script
lint(filename = "Tratamiento de la información.R")


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
  path = "Proyecto 2 - Base segmentacion.xls"
))

dataset_editado <- dataset_original %>%
  mutate(
    Edad = as.double(as.POSIXct("2010-01-01") - `Fecha Nacimiento`) / 365.25,
    Actividad_laboral = as.factor(`Actividad Laboral`),
    Estado_civil = as.factor(`Estado Civil`),
    Nivel_educacional = as.factor(`Nivel Educacional`),
    Tiene_mora = as.factor(`Tiene Mora`),
    Tiene_credito_hipotecario = as.factor(`Tiene Crédito Hipotecario`),
    Tiene_credito_de_consumo = as.factor(`Tiene Crédito de Consumo`),
    Medio_de_contacto_preferente = as.factor(`Medio de Contacto Preferente`),
    Tiene_inversiones = as.factor(`Tiene Inversiones`)
  ) %>%
  select(
    !c(
      `Actividad Laboral`, `Estado Civil`, `Nivel Educacional`, `Tiene Mora`,
      `Tiene Crédito Hipotecario`, `Tiene Crédito de Consumo`,
      `Medio de Contacto Preferente`, `Tiene Inversiones`, `Fecha Nacimiento`
    )
  )

dataset_categorizado <- dataset_editado %>%
  mutate(
    AL_empresario = ifelse(Actividad_laboral == "Empresario", 1, 0),
    AL_gerente = ifelse(Actividad_laboral == "Gerente", 1, 0),
    AL_obrero = ifelse(Actividad_laboral == "Obrero", 1, 0),
    AL_tecnico = ifelse(Actividad_laboral == "Técnico", 1, 0),
    AL_trab_dep = ifelse(Actividad_laboral == "Trabajador Dependiente", 1, 0),
    AL_trab_ind = ifelse(Actividad_laboral == "Trabajador Independiente", 1, 0),
    # Actividad_laboral == "Jubilado" queda por exclusión
    EC_casado = ifelse(Estado_civil == "Casado", 1, 0),
    EC_soltero = ifelse(Estado_civil == "Soltero", 1, 0),
    # Estado_civil == "Divorciado" queda por exclusión
    NE_ens_media = ifelse(Nivel_educacional == "Enseñanza Media", 1, 0),
    NE_tec_profe = ifelse(Nivel_educacional == "Técnico Profesional", 1, 0),
    NE_universit = ifelse(Nivel_educacional == "Universitario", 1, 0),
    # Nivel_educacional == "Enseñanza Básica" queda por exclusión
    MCP_celular = ifelse(Medio_de_contacto_preferente == "Celular", 1, 0),
    MCP_emailing = ifelse(Medio_de_contacto_preferente == "Emailing", 1, 0),
    # Medio_de_contacto_preferente == "Fono Particular" queda por exclusión
    Tiene_mora = ifelse(Tiene_mora == "Si", 1, 0),
    Tiene_credito_hipotecario = ifelse(Tiene_credito_hipotecario == "Si", 1, 0),
    Tiene_credito_de_consumo = ifelse(Tiene_credito_de_consumo == "Si", 1, 0),
    Tiene_inversiones = ifelse(Tiene_inversiones == "Si", 1, 0),
  ) %>%
  select(
    !c(
      IdCliente, Actividad_laboral, Estado_civil, Nivel_educacional,
      Medio_de_contacto_preferente
    )
  )

dataset_ponderado <- dataset_categorizado %>%
  lapply(FUN = normalizacion) %>%
  as.data.frame() %>%
  mutate(
    AL_empresario = AL_empresario/ 7,
    AL_gerente = AL_gerente / 7,
    AL_obrero = AL_obrero / 7,
    AL_tecnico = AL_tecnico / 7,
    AL_trab_dep = AL_trab_dep / 7,
    AL_trab_ind = AL_trab_ind / 7,
    EC_casado = EC_casado / 3,
    EC_soltero = EC_soltero / 3,
    NE_ens_media = NE_ens_media / 4,
    NE_tec_profe = NE_tec_profe / 4,
    NE_universit = NE_universit / 4,
    MCP_celular = MCP_celular / 3,
    MCP_emailing = MCP_emailing / 3,
  )


# Exploración de los datos ------------------------------------------------

str(dataset_original)
summary(dataset_original)

str(dataset_editado)
summary(dataset_editado)

str(dataset_ponderado)
summary(dataset_ponderado)


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


# Tablas importantes ------------------------------------------------------

##### Visualización de las tablas #####

##### Traspaso de las tablas a un archivo xlsx #####
write.xlsx(
  x = comparador_modelos_1,
  file = "Datos y Modelos.xlsx",
  sheetName = "Rendimiento_Modelos",
  append = TRUE, # Se crea el archivo
  showNA = FALSE
)

# Experimentos ------------------------------------------------------------
