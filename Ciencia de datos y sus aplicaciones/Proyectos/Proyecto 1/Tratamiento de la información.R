# Ciencia de datos y sus aplicaciones
# Proyecto 1

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "tibble",
  "patchwork",
  "readxl",
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

# Resumen de las variables según los distintos servicios contratados
dataset_categorizado %>% filter(`Servicio contratado` == 0) %>% summary()
dataset_categorizado %>% filter(`Servicio contratado` == 1) %>% summary()
dataset_categorizado %>% filter(`Servicio contratado` == 2) %>% summary()

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


# Evaluación de modelos ---------------------------------------------------
