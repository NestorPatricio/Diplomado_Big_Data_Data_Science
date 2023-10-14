# Ciencia de datos y sus aplicaciones
# Proyecto 2

# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "readxl",
  #"xlsx", # Paquete para escirbir archivos xlsx
  "factoextra", # Visualización de PCA
  "cluster", # Cálculo de silueta
  "caret", # Matriz de comparación
  "mclust", # Mixture-Gaussian model
  "dbscan", # DBSCAN  model
  "dendextend", # Evaluar comparador "FM_index"
  "clevr", # Evaluar comparador "v_measure" y "adj_rand_index"
  "rgl", # Visualización en 3D
  "Rtsne", # Análisis t-SNE
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

# Dataset con peso de variables clave x2
dataset_pesado <- dataset_sin_outliers %>%
  mutate(
    Edad = Edad * 2,
    Tiene_mora = Tiene_mora * 2,
    Saldo.Medio.Anual = Saldo.Medio.Anual * 2,
    Tiene_credito_hipotecario = Tiene_credito_hipotecario * 2,
    Tiene_credito_de_consumo = Tiene_credito_de_consumo * 2,
    AL_empresario = AL_empresario * 2,
    AL_gerente = AL_gerente * 2,
    AL_obrero = AL_obrero * 2,
    AL_tecnico = AL_tecnico * 2,
    AL_trab_dep = AL_trab_dep * 2,
    AL_trab_ind = AL_trab_ind * 2
  )

# Dataset con peso de variables clave x2 y eliminación de algunas homogéneas
dataset_podado_pesado <- dataset_pesado %>%
  select(
    Edad, Tiene_mora, Saldo.Medio.Anual, Tiene_credito_hipotecario,
    Tiene_credito_de_consumo, AL_empresario, AL_gerente, AL_obrero, AL_tecnico,
    AL_trab_dep, AL_trab_ind, NE_universit, MCP_celular
  )

# Dataset con peso de variables clave x2 y eliminación de todas las homogéneas
dataset_podado_pesado2 <- dataset_pesado %>%
  select(
    Edad, Saldo.Medio.Anual, Tiene_credito_hipotecario, AL_trab_dep,
    NE_universit, MCP_celular
  )

# Dataset con eliminación de variables muy homogéneas, excepto las clave
dataset_podado <- dataset_sin_outliers %>%
  select(
    Edad, Tiene_mora, Saldo.Medio.Anual, Tiene_credito_hipotecario,
    Tiene_credito_de_consumo, AL_empresario, AL_gerente, AL_obrero, AL_tecnico,
    AL_trab_dep, AL_trab_ind, NE_universit, MCP_celular
  )

# Dataset con eliminación de todas las variables muy homogéneas
dataset_podado2 <- dataset_sin_outliers %>%
  select(
    Edad, Saldo.Medio.Anual, Tiene_credito_hipotecario, AL_trab_dep,
    NE_universit, MCP_celular
  )
#summary(dataset_podado_pesado2)


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

##### Evaluación de la variación con K-medios de 4 clústeres #####
comparador <- tibble(
  Cluster1 = integer(),
  Cluster2 = integer(),
  Cluster3 = integer(),
  Cluster4 = integer(),
)

# Dataset ponderado con registros outliers
for (i in 1:20) {
  modelo_km1 <- kmeans(
    x = dataset_ponderado,
    centers = 3,
    iter.max = 20,
    nstart = 30,
    algorithm = "Hartigan-Wong",
    trace = FALSE
  )

  comparador <- bind_rows(
    comparador,
    tibble(
      Cluster1 = sum(modelo_km1$cluster == 1),
      Cluster2 = sum(modelo_km1$cluster == 2),
      Cluster3 = sum(modelo_km1$cluster == 3),
      Cluster4 = sum(modelo_km1$cluster == 4),
    )
  )
}


##### Homologación de K-medios y Mezcla Gaussiana para 4 clústeres #####
# K-medios
modelo_km1 <- kmeans(
  x = dataset_pesado,
  centers = 4,
  iter.max = 20,
  nstart = 30,
  algorithm = "Hartigan-Wong",
  trace = FALSE
)
clusters_km1 <- modelo_km1$cluster

# Mezcla Gaussiana
modelo_mixgauss1 <- Mclust(
  data = dataset_pesado,
  G = 4
)
clusters_mg1 <- modelo_mixgauss1$classification

# Matriz de confusión previo a la edición
comp_matriz1 <- confusionMatrix(
  data = as.factor(clusters_km1),
  reference = as.factor(clusters_mg1),
  dnn = c("K-medios", "Mezcla Gaussian")
)

# Cálculo del Índice de Fowlkes-Mallows previo a la edición
comp_fmi1 <- FM_index(
  A1_clusters = clusters_km1,
  A2_clusters = clusters_mg1
)

# Cálculo de V-measure previo a la edición
comp_vmeasure1 <- v_measure(
  true = clusters_km1,
  pred = clusters_mg1
)

# Cálculo del Índice de Rand ajustado previo a la edición
comp_arandi1 <- adj_rand_index(
  true = clusters_km1,
  pred = clusters_mg1
)

# Comparador de valores previo a la edición
comparacion_comparadores <- tibble(
  Exactitud = round(x = comp_matriz1$overall[1], digit = 4),
  Fowlkes_Mallows = round(x = comp_fmi1[1], digit = 4),
  V_measure = round(x = comp_vmeasure1, digit = 4),
  Rand_ajustado = round(x = comp_arandi1, digit = 4)
)

# Identificador de clústeres
clusteres <- tibble(
  K_medios_pre = clusters_km1,
  M_gaussi_pre = clusters_mg1
)

clusteres %>%
  filter(K_medios_pre == 1) %>%
  group_by(M_gaussi_pre) %>%
  summarise("cantidad" = n())
clusteres %>%
  filter(K_medios_pre == 2) %>%
  group_by(M_gaussi_pre) %>%
  summarise("cantidad" = n())
clusteres %>%
  filter(K_medios_pre == 3) %>%
  group_by(M_gaussi_pre) %>%
  summarise("cantidad" = n())
clusteres %>%
  filter(K_medios_pre == 4) %>%
  group_by(M_gaussi_pre) %>%
  summarise("cantidad" = n())



# El cálculo de la distancia y la silueta los debe hacer Wladimir
#distancia <- dist(dataset_pca)
# silhouette(x = modelo_km1$cluster, dist = distancia)

# Propuesta de Ramiro (Chat-GPT) para el cálculo de silueta
#mean(modelo_km$betweenss / modelo_km$tot.withinss




##### Análisis de los componentes principales (PCA) #####
# Dataset sin outliers
modelo1_pca <- princomp(x = dataset_sin_outliers)
round(
  x = modelo1_pca$sdev^2 / sum(modelo1_pca$sdev^2) * 100,
  digit = 2
)
fviz_pca_ind(X = modelo1_pca, habillage = modelo_km_final$cluster)
plot3d(modelo1_pca$scores[, 1:3], col = modelo_km_final$cluster)

# Dataset con análisis PCA del 85% de información
dataset_pca <- modelo1_pca$scores[,
  c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5")
]

# Dataset pesado
modelo2_pca <- princomp(x = dataset_pesado)
round(
  x = modelo2_pca$sdev^2 / sum(modelo2_pca$sdev^2) * 100,
  digit = 2
)
fviz_pca_ind(X = modelo2_pca, habillage = modelo_km_final$cluster)
plot3d(modelo2_pca$scores[, 1:3], col = modelo_km_final$cluster)

# Dataset PCA
modelo3_pca <- princomp(x = dataset_sin_outliers)
round(
  x = modelo3_pca$sdev^2 / sum(modelo3_pca$sdev^2) * 100,
  digit = 2
)
fviz_pca_ind(X = modelo3_pca, habillage = modelo_km_final$cluster)
plot3d(modelo3_pca$scores[, 1:3], col = modelo_km_final$cluster)

##### Análisis tSNE #####
modelo1_tsne <- Rtsne(X = unique(dataset_sin_outliers))
ggplot(data = as_tibble(modelo1_tsne$Y)) +
  geom_point(mapping = aes(x = V1, y = V2))


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

# Comparador de distintas ejecuciones de K-medios
df_experimental <- data.frame(
  variables = character(),
  valor_k = integer(),
  iteración = integer(),
  silueta_media = numeric(),
  suma_cuadratica = numeric()
)

# Se comparan 20 ejecuciones para valores K de 2 a 10
for (k in 1:10) {
  for (iteracion in 1:30) {
    modelo_km <- kmeans(
      #x = dataset_ponderado,
      #x= dataset_sin_outliers,
      #x = dataset_pesado,
      #x = dataset_podado_pesado,
      #x = dataset_podado_pesado2,
      x = dataset_pca,
      centers = k,
      iter.max = 20,
      nstart = 30,
      algorithm = "Hartigan-Wong",
      trace = FALSE
    )

    # Acá podría ponerse el cálculo de silueta

    # Se genera la nueva fila de la iteración
    nueva_fila <- data.frame(
      #variables = "Todas",
      #variables = "Sin outliers",
      #variables = "Pesadas x2",
      #variables = "Podado x2",
      #variables = "Liviano x2",
      variables = "PCA",
      valor_k = k,
      iteración = iteracion,
      silueta_media = NA,
      suma_cuadratica = modelo_km$tot.withinss
    )

    # Agrega una fila al DataFrame df_experimental
    df_experimental <- rbind(df_experimental, nueva_fila)
  }
}

for (k in 1:10) {
  valores <- unique(
    x = df_experimental[
      df_experimental$valor_k == k & df_experimental$variables == "PCA",
      "suma_cuadratica"
    ]
  )

  cat(
    paste0(
      "\n\nPara ", k, " clústeres, en 30 iteraciones, existen ",
      length(valores), " valores de suma cuadrática distintos, con promedio ",
      round(mean(valores), 2), ":\n"
    )
  )
  print(sort(unique(valores)))
}

# Gráficos de codo
grafico_codo1 <- df_experimental[1:300, ] %>%
  filter(variables == "Todas") %>%
  group_by(valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  geom_point(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Todas las variables.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())

grafico_codo2 <- df_experimental[301:600, ] %>%
  filter(variables == "Sin outliers") %>%
  group_by(valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  geom_point(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Todas las variables sin valores outliers.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())

grafico_codo3 <- df_experimental %>%
  filter(variables == "Pesadas x2") %>%
  group_by(valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  geom_point(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Variables claves con peso x2.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())

grafico_codo4 <- df_experimental %>%
  filter(variables == "Podado x2") %>%
  group_by(valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  geom_point(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Variables claves con peso x2 y seleccionado.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())

grafico_codo5 <- df_experimental %>%
  filter(variables == "Liviano x2") %>%
  group_by(valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  geom_point(mapping = aes(x = valor_k, y = suma_cuadratica)) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Variables claves con peso x2 y ultra seleccionado.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank())

# Gráfico comparado con distintas facetas
df_experimental %>%
  mutate(variables = as.factor(variables)) %>%
  group_by(variables, valor_k) %>%
  summarise(suma_cuadratica = mean(suma_cuadratica)) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = valor_k, y = suma_cuadratica, colour = variables)
  ) +
  geom_point(
    mapping = aes(x = valor_k, y = suma_cuadratica, colour = variables)
  ) +
  labs(
    title = "Suma cuadrática de distancias por número de clústeres.",
    subtitle = "Variables claves con peso x2.",
    x = "Número de clústeres",
    y = "Suma cuadrática"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  facet_wrap(facets = vars(variables), ncol = 5, scales = "free_y") +
  theme_light() +
  theme(panel.grid = element_blank())


# Evaluación de clústeres -------------------------------------------------

modelo_km_final <- kmeans(
  x = dataset_pca,
  centers = 3,
  iter.max = 20,
  nstart = 30,
  algorithm = "Hartigan-Wong",
  trace = FALSE
)

dataset_con_clusteres <- dataset_categorizado[
  dataset_ponderado$Saldo.Medio.Anual < 0.8 &
    dataset_ponderado$Saldo.Medio.Anual > 0.02 &
    dataset_ponderado$Contactos.con.su.Ejecutivo < 0.18 &
    dataset_ponderado$Edad < 1 & log(dataset_ponderado$Edad) > -6,
] %>%
  mutate(Clúster = modelo_km_final$cluster)

media_sin_cluster <- dataset_con_clusteres %>%
  summarise_all(mean) %>%
  mutate(Clúster = "Total", Tamaño = nrow(dataset_con_clusteres))

medias_con_clusteres <- dataset_con_clusteres %>%
  mutate(Clúster = as.factor(Clúster)) %>%
  group_by(Clúster) %>%
  summarise_all(mean) %>%
  mutate(Tamaño = modelo_km_final$size) %>%
  bind_rows(media_sin_cluster)

mediana_sin_cluster <- dataset_con_clusteres %>%
  summarise_all(median) %>%
  mutate(Clúster = "Total", Tamaño = nrow(dataset_con_clusteres))

medianas_con_clusteres <- dataset_con_clusteres %>%
  mutate(Clúster = as.factor(Clúster)) %>%
  group_by(Clúster) %>%
  summarise_all(median) %>%
  mutate(Tamaño = modelo_km_final$size) %>%
  bind_rows(mediana_sin_cluster)

ggplot(data = dataset_con_clusteres) +
  geom_boxplot(mapping = aes(x = Clúster, y = `Saldo Medio Anual`))
