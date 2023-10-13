# Ciencia de datos y sus aplicaciones
# Proyecto 2

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "readxl",
  "xlsx",
  "factoextra", # Visualización de PCA
  "clustercrit",
  #"cluster", # Cálculo de silueta
  #"caret",# Matriz de comparación
  #"mclust",# Mixture-Gaussian model
  #"dbscan", # DBSCAN  model
  #"dendextend",# Evaluar comparador "FM_index"
  #"clevr",# Evaluar comparador "v_measure" y "adj_rand_index"
  #"rgl", # Visualización en 3D
  #"dendextend", # Otro modelo que no recuerdo
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
#lint(filename = "Tratamiento de la información.R")


# Funciones ---------------------------------------------------------------

normalizacion <- function(valores) {
  # Realiza normalización de los datos
  minimo <-  min(valores, na.rm = TRUE)
  maximo <- max(valores, na.rm = TRUE)
  return((valores - minimo) / (maximo - minimo))
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
    AL_empresario = AL_empresario / 7,
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

# Dataset sin datos outliers según técnicas de visualización
dataset_sin_outliers <- dataset_ponderado %>%
  filter(
    Saldo.Medio.Anual < 0.8,
    Saldo.Medio.Anual > 0.02,
    Contactos.con.su.Ejecutivo < 0.18,
    Edad < 1,
    log(Edad) > -6
  )


# Exploración de los datos ------------------------------------------------

# Considerar: Edad, Actividad laboral, Mora, Saldo medio anual, Crédito
#hipotecario y Crèdito de consumo

str(dataset_original)
summary(dataset_original)

str(dataset_editado)
summary(dataset_editado)

str(dataset_ponderado)
summary(dataset_ponderado)

# Histogramas de variables numéricas
# Saldo medio anual
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = `Saldo Medio Anual`)) +
  labs(title = "Saldo medio anual")

# Saldo medio anual (logaritmo)
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = log(`Saldo Medio Anual`))) +
  labs(title = "Saldo medio anual (logaritmo)")

# Saldo medio anual sin valores menores o iguales a 0
ggplot(
  data = dataset_editado[dataset_editado$`Saldo Medio Anual` > 0, ]
) +
  geom_histogram(mapping = aes(x = `Saldo Medio Anual`)) +
  labs(title = "Saldo medio anual sin valores menores o iguales a 0")

# Saldo medio anual sin valores menores o iguales a 0 (logaritmo)
ggplot(
  data = dataset_editado[dataset_editado$`Saldo Medio Anual` > 0, ]
) +
  geom_histogram(mapping = aes(x = log(`Saldo Medio Anual`))) +
  labs(
    title = "Saldo medio anual sin valores menores o iguales a 0 (logaritmo)"
  )

# Edad
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = `Edad`)) +
  labs(title = "Edad")

# Edad
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = log(`Edad`))) +
  labs(title = "Edad (logaritmo)")

# Contactos con su ejecutivo
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = `Contactos con su Ejecutivo`)) +
  labs(title = "Contactos con su ejecutivo")

# Contactos con su ejecutivo (logaritmo)
ggplot(data = dataset_editado) +
  geom_histogram(mapping = aes(x = log(`Contactos con su Ejecutivo`))) +
  labs(title = "Contactos con su ejecutivo (logaritmo)")

# Contactos con su ejecutivo sin valores 0
ggplot(
  data = dataset_editado[dataset_editado$`Contactos con su Ejecutivo` != 0, ]
) +
  geom_histogram(mapping = aes(x = `Contactos con su Ejecutivo`)) +
  labs(title = "Contactos con su ejecutivo sin valores 0")

# Contactos con su ejecutivo sin valores 0 (logaritmo)
ggplot(
  data = dataset_editado[dataset_editado$`Contactos con su Ejecutivo` != 0, ]
) +
  geom_histogram(mapping = aes(x = log(`Contactos con su Ejecutivo`))) +
  labs(title = "Contactos con su ejecutivo sin valores 0 (logaritmo)")

# Boxplots de variables numéricas
# Saldo medio anual
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = `Saldo Medio Anual`)) +
  labs(title = "Saldo medio anual")

# Saldo medio anual (logaritmo)
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = log(`Saldo Medio Anual`))) +
  labs(title = "Saldo medio anual (logaritmo)")

# Saldo medio anual sin valores menores o iguales a 0
ggplot(
  data = dataset_editado[dataset_editado$`Saldo Medio Anual` > 0, ]
) +
  geom_boxplot(mapping = aes(x = `Saldo Medio Anual`)) +
  labs(title = "Saldo medio anual sin valores menores o iguales a 0")

# Saldo medio anual sin valores menores o iguales a 0 (logaritmo)
ggplot(
  data = dataset_editado[dataset_editado$`Saldo Medio Anual` > 0, ]
) +
  geom_boxplot(mapping = aes(x = log(`Saldo Medio Anual`))) +
  labs(
    title = "Saldo medio anual sin valores menores o iguales a 0 (logaritmo)"
  )

# Edad
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = `Edad`)) +
  labs(title = "Edad")

# Edad
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = log(`Edad`))) +
  labs(title = "Edad (logaritmo)")

# Contactos con su ejecutivo
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = `Contactos con su Ejecutivo`)) +
  labs(title = "Contactos con su ejecutivo")

# Contactos con su ejecutivo (logaritmo)
ggplot(data = dataset_editado) +
  geom_boxplot(mapping = aes(x = log(`Contactos con su Ejecutivo`))) +
  labs(title = "Contactos con su ejecutivo (logaritmo)")

# Contactos con su ejecutivo sin valores 0
ggplot(
  data = dataset_editado[dataset_editado$`Contactos con su Ejecutivo` != 0, ]
) +
  geom_boxplot(mapping = aes(x = `Contactos con su Ejecutivo`)) +
  labs(title = "Contactos con su ejecutivo sin valores 0")

# Contactos con su ejecutivo sin valores 0 (logaritmo)
ggplot(
  data = dataset_editado[dataset_editado$`Contactos con su Ejecutivo` != 0, ]
) +
  geom_boxplot(mapping = aes(x = log(`Contactos con su Ejecutivo`))) +
  labs(title = "Contactos con su ejecutivo sin valores 0 (logaritmo)")

# Comparativa de gráfico de puntos de las variables numéricas
# Variables en escala lineal sin eliminar outliers
plot(
  x = dataset_ponderado[
    ,
    c("Saldo.Medio.Anual", "Contactos.con.su.Ejecutivo", "Edad")
  ]
)

# Variables en escala lineal con outliers eliminados
plot(
  x = dataset_ponderado[
    dataset_ponderado$Contactos.con.su.Ejecutivo < 0.18 &
      dataset_ponderado$Saldo.Medio.Anual < 0.8 &
      dataset_ponderado$Saldo.Medio.Anual > 0.02 &
      dataset_ponderado$Edad < 1 &
      log(dataset_ponderado$Edad) > -6,
    c("Saldo.Medio.Anual", "Contactos.con.su.Ejecutivo", "Edad")
  ]
)

# Variables en escala logaritmica sin eliminar outliers
plot(
  x = log(dataset_ponderado[
    ,
    c("Saldo.Medio.Anual", "Contactos.con.su.Ejecutivo", "Edad")
  ])
)

# Variables en escala lineal con outliers eliminados
plot(
  x = log(dataset_ponderado[
    dataset_ponderado$Contactos.con.su.Ejecutivo < 0.18 &
      dataset_ponderado$Saldo.Medio.Anual < 0.8 &
      dataset_ponderado$Saldo.Medio.Anual > 0.02 &
      dataset_ponderado$Edad < 1 &
      log(dataset_ponderado$Edad) > -6,
    c("Saldo.Medio.Anual", "Contactos.con.su.Ejecutivo", "Edad")
  ])
)


# Evaluación de modelos ---------------------------------------------------

##### K medios #####
# Se declaran las variables que guardarán la suma

numero_clusteres <- 2:10
# Requiere de 7,5 GB libres para ejecutarse
distancia <- dist(dataset_ponderado)

# Dataset ponderado con registros outliers
modelo_km1 <- kmeans(
  x = dataset_ponderado,
  centers = 4,
  iter.max = 10,
  nstart = 1,
  algorithm = "Hartigan-Wong",
  trace = FALSE
)

silhouette(x = modelo_km1$cluster, dist = distancia)


modelo_mixgauss1 <- Mclust(
  data = dataset_ponderado,
  G = 4
)

modelo_km1_viz <- fviz_cluster(modelo_km1, dataset_ponderado, ellipse = FALSE, geom = "point")
modelo_mixgauss1_viz <- fviz_cluster(modelo_mixgauss1, dataset_ponderado, ellipse = FALSE, geom = "point")
fm_score1 <- FM_index(modelo_km1$cluster, modelo_mixgauss1$classification)
v_measure1 <- v_measure(true = modelo_km1$cluster, pred =  modelo_mixgauss1$classification)
ar_index1 <- adj_rand_index(cl1 = modelo_km1$cluster, cl2 = modelo_mixgauss1$classification)

# Dataset ponderado con registros outliers
modelo_km2 <- kmeans(
  x = dataset_sin_outliers,
  centers = 5,
  iter.max = 10,
  nstart = 1,
  algorithm = "Hartigan-Wong",
  trace = FALSE
)

modelo_mixgauss2 <- Mclust(
  data = dataset_sin_outliers,
  G = 5
)

modelo_km2_viz <- fviz_cluster(modelo_km2, dataset_sin_outliers, ellipse = FALSE, geom = "point")
modelo_mixgauss2_viz <- fviz_cluster(modelo_mixgauss2, dataset_sin_outliers, ellipse = FALSE, geom = "point")
fm_score2 <- FM_index(modelo_km2$cluster, modelo_mixgauss2$classification)
v_measure2 <- v_measure(true = modelo_km2$cluster, pred = modelo_mixgauss2$classification)
ar_index2 <- adj_rand_index(cl1 = modelo_km2$cluster, cl2 = modelo_mixgauss2$classification)

# Comparador de comparadores
fm_score1
v_measure1
ar_index1

fm_score2
v_measure2
ar_index2

# PCA
modelo1_pca <- princomp(x = dataset_sin_outliers)
var_pca_porc1 <- round(
  x = modelo1_pca$sdev^2 / sum(modelo1_pca$sdev^2) * 100,
  digit = 2
)
modelo1_2d <- fviz_pca_ind(X = modelo1_pca, habillage = modelo_km1$cluster)
modelo1_3d <- plot3d(modelo1_pca$scores[, 1:3], col = modelo_km1$cluster)

modelo2_pca <- princomp(x = dataset_sin_outliers)
var_pca_porc2 <- round(
  x = modelo2_pca$sdev^2 / sum(modelo2_pca$sdev^2) * 100,
  digit = 2
)
modelo2_2d <- fviz_pca_ind(X = modelo2_pca, habillage = modelo_km2$cluster)
modelo2_3d <- plot3d(modelo2_pca$scores[, 1:3], col = modelo_km2$cluster)


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
