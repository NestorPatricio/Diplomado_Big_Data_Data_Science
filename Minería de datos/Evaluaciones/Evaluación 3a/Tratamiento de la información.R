# Evaluación 3a
# Wladimir Richard Parada Rebolledo
# Néstor Patricio Rojas Ríos

############################## Prolegómenos ##############################
# Configuración del directorio de trabajo
if (Sys.info()["sysname"] == "Windows") {
  setwd(paste0(
    "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\",
    "\\Minería de datos\\Evaluaciones\\Evaluación 3a\\"
  ))
} else if (Sys.info()["sysname"] == "Linux") {
  setwd(paste0(
    "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de ",
    "datos/Evaluaciones/Evaluación 3a/"
  ))
}

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "patchwork",
  "ggsci",
  "grid",
  "gridExtra",
  "lintr",
  "cluster",
  "factoextra"
)
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Se activa el linter para R
lint("Tratamiento de la información.R")

# Carga de datos
datos_banco <- read.csv(
  file = "../Evaluación 2/SouthGermanCredit.csv",
  header = TRUE,
  sep = ";"
)


############################## Funciones ##############################
valores_kmeans_df <- function(
  datos,
  clusteres = 10,
  intentos_por_k = 1,
  iteraciones_max = 20,
  algoritmo = "Hartigan-Wong"
) {
  # algortimo puede ser "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"
  # Facilita la generación de múltiples clústeres, junto con su valor de silueta
  #y de suma cuadrática de distancia total
  lista_final <- list()
  dataframe_final <- data.frame(
    N_clusters = 1:clusteres,
    Suma_cuadratica = NA,
    Silueta = NA
  )

  # Acá se genra la información que saldrá
  for (k in 1:clusteres) {
    kmedio_aux <- kmeans(
      x = datos,
      centers = k,
      nstart = intentos_por_k,
      iter.max = iteraciones_max,
      algorithm = algoritmo
    )

    if (k > 1) {
      silueta_aux <- silhouette(x = kmedio_aux$cluster, dist = dist(datos))
      dataframe_final[k, "Silueta"] <- mean(silueta_aux[, "sil_width"])
    } else {
      dataframe_final[k, "Silueta"] <- 0
    }
    dataframe_final[k, "Suma_cuadratica"] <- kmedio_aux$tot.withinss

    dataframe_aux <- data.frame(kmedio_aux$centers)
    dataframe_aux["Tamano"] <- kmedio_aux$size
    lista_final[[k]] <- list(
      "Dataframe" = dataframe_aux,
      "Clasificacion" = kmedio_aux$cluster
    )
  }

  # Se arma la lista final
  nombre_listas <- paste0("Lista_K_", 1:clusteres)
  names(lista_final) <- nombre_listas
  lista_final[["Dataframe_final"]] <- dataframe_final

  return(lista_final)
}

normalizacion <- function(valores) {
  # Realiza normalización de los datos
  return((valores - min(valores)) / (max(valores) - min(valores)))
}

formateador <- function(numero) {
  # Define el punto para separación de miles y la coma para separación decimal
  return(
    format(x = numero, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  )
}

es_outlier <- function(variable) {
  # Identifica los valores outliers en un boxplot
  return(
    variable < quantile(variable, 0.25) - (1.5 * IQR(variable)) |
      variable > quantile(variable, 0.75) + (1.5 * IQR(variable))
  )
}


############################## Desarrollo ##############################

############################## Pregunta 1 ##############################
# Usando todas las variables de la base de datos sin tratamiento ----------
modelo_total_crudo <- valores_kmeans_df(
  datos = datos_banco,
  clusteres = 50,
  intentos_por_k = 20,
  iteraciones_max = 50
)

codo_total_crudo <- ggplot(
  data = modelo_total_crudo[[length(modelo_total_crudo)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Todas las variables sin tratamiento.",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()

silueta_total_crudo <- ggplot(
  data = modelo_total_crudo[[length(modelo_total_crudo)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Todas las variables sin tratamiento.",
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  theme_light()

codo_total_crudo + silueta_total_crudo


# Tratamiento de datos ----------------------------------------------------
# Se tratan los datos
datos_banco_tratados <- datos_banco %>%
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
    history_4 = ifelse(history == 4, 1, 0),
    personal_1 = ifelse(personal == 1, 1, 0),
    personal_2 = ifelse(personal == 2, 1, 0),
    personal_3 = ifelse(personal == 3, 1, 0),
    housing_1 = ifelse(housing == 1, 1, 0),
    housing_2 = ifelse(housing == 2, 1, 0)
  ) %>%
  select(!c(status, history, personal, housing))

datos_banco_normalizados <- as.data.frame(
  lapply(X = datos_banco_tratados, FUN = normalizacion)
)

datos_banco_ponderados <- datos_banco_normalizados %>%
  mutate(
    status_1 = status_1 / 4,
    status_2 = status_2 / 4,
    status_3 = status_3 / 4,
    history_1 = history_1 / 4,
    history_2 = history_2 / 4,
    history_3 = history_3 / 4,
    personal_1 = personal_1 / 4,
    personal_2 = personal_2 / 4,
    personal_3 = personal_3 / 4,
    housing_1 = housing_1 / 3,
    housing_2 = housing_2 / 3
  )


# Usando todas las variables de la base de datos con tratamiento ----------
# Se procede con los cálculos y gráficos
modelo_total_tratado <- valores_kmeans_df(
  datos = datos_banco_ponderados,
  clusteres = 200,
  intentos_por_k = 50,
  iteraciones_max = 50
)

codo_total_tratado <- ggplot(
  data = modelo_total_tratado[[length(modelo_total_tratado)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Todas las variables con tratamiento.",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()

silueta_total_tratado <- ggplot(
  data = modelo_total_tratado[[length(modelo_total_tratado)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Todas las variables con tratamiento.",
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  theme_light()

codo_total_tratado + silueta_total_tratado

# Se compara la información para evaluar el peso de las variables
total_tratado_k4 <- kmeans(
  x = datos_banco_ponderados,
  centers = 4,
  nstart = 20,
  iter.max = 50
)

total_tratado_k4$size
total_tratado_k4_df <- datos_banco_tratados %>%
  mutate(k_4 = total_tratado_k4$cluster) %>%
  group_by(k_4) %>%
  summarise_all(mean)
View(total_tratado_k4_df)

total_tratado_k5 <- kmeans(
  x = datos_banco_ponderados,
  centers = 5,
  nstart = 20,
  iter.max = 50
)

total_tratado_k5$size
total_tratado_k5_df <- datos_banco_tratados %>%
  mutate(k_5 = total_tratado_k5$cluster) %>%
  group_by(k_5) %>%
  summarise_all(mean)
View(total_tratado_k5_df)


# Datos tratados: amount, savings, telephone, credit, status_1 ------------
# Se editan los datos del modelo
datos_banco_ponderado_modelo_3 <- datos_banco_normalizados %>%
  select(
    amount, savings, telephone, credit, status_1
  )

# Se editan los datos para comparación
datos_banco_tratados_modelo_3 <- datos_banco_tratados %>%
  select(
    amount, savings, telephone, credit, status_1
  )

# Se procede con los cálculos y gráficos
modelo_3 <- valores_kmeans_df(
  datos = datos_banco_ponderado_modelo_3,
  clusteres = 50,
  intentos_por_k = 50,
  iteraciones_max = 50
)

codo_modelo_3 <- ggplot(
  data = modelo_3[[length(modelo_3)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Variables del modelo 3.",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()

silueta_modelo_3 <- ggplot(
  data = modelo_3[[length(modelo_3)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Variables del modelo 3.",
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  theme_light()

codo_modelo_3 + silueta_modelo_3

# Se compara la información para evaluar el peso de las variables
# K 12
moldeo_3_k12 <- kmeans(
  x = datos_banco_ponderado_modelo_3,
  centers = 12,
  nstart = 50,
  iter.max = 50
)

moldeo_3_k12$size
moldeo_3_k12_df <- datos_banco_tratados_modelo_3 %>%
  mutate(k_12 = moldeo_3_k12$cluster) %>%
  group_by(k_12) %>%
  summarise_all(mean)
View(moldeo_3_k12_df)

# K 10
moldeo_3_k10 <- kmeans( # Candidato probable
  x = datos_banco_ponderado_modelo_3,
  centers = 10,
  nstart = 50,
  iter.max = 50
)

moldeo_3_k10$size
moldeo_3_k10_df <- datos_banco_tratados_modelo_3 %>%
  mutate(k_10 = moldeo_3_k10$cluster) %>%
  group_by(k_10) %>%
  summarise_all(mean)
View(moldeo_3_k10_df)

# K 5
moldeo_3_k5 <- kmeans( # Candidato probable
  x = datos_banco_ponderado_modelo_3,
  centers = 5,
  nstart = 50,
  iter.max = 50
)

moldeo_3_k5$size
moldeo_3_k5_df <- datos_banco_tratados_modelo_3 %>%
  mutate(k_5 = moldeo_3_k5$cluster) %>%
  group_by(k_5) %>%
  summarise_all(mean)
View(moldeo_3_k5_df)


# Datos tratados, sólo variables binarias: telephone, credit, stat --------
# Se deja de ejemplo, pero no hay que seguir esta vía
# Se editan los datos para comparación
datos_banco_tratados_modelo_4 <- datos_banco_tratados %>%
  select(
    telephone, credit, status_1
  )

# Se editan los datos del modelo
datos_banco_ponderado_modelo_4 <- datos_banco_normalizados %>%
  select(
    telephone, credit, status_1
  )

# Se procede con los cálculos y gráficos
modelo_4 <- valores_kmeans_df(
  datos = datos_banco_ponderado_modelo_4,
  clusteres = 8,
  intentos_por_k = 20,
  iteraciones_max = 50
)

codo_modelo_4 <- ggplot(
  data = modelo_4[[length(modelo_4)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Variables del modelo 4.",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()

silueta_modelo_4 <- ggplot(
  data = modelo_4[[length(modelo_4)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Variables del modelo 4.",
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  theme_light()

codo_modelo_4 + silueta_modelo_4

# Se compara la información para evaluar el peso de las variables
moldeo_4_k8 <- kmeans(
  x = datos_banco_ponderado_modelo_4,
  centers = 8,
  nstart = 20,
  iter.max = 50
)

moldeo_4_k8$size
moldeo_4_k8_df <- datos_banco_tratados_modelo_4 %>%
  mutate(k_8 = moldeo_4_k8$cluster) %>%
  group_by(k_8) %>%
  summarise_all(mean)
View(moldeo_4_k8_df)


# Datos tratados, seleccionados según evaluación del dataset
# Se editan los datos para comparación
datos_banco_tratados_modelo_5 <- datos_banco_tratados %>%
  select(
    !c(foreign, persons, job, credits, age, rate, residence, savings,
      status_1, status_2, status_3, history_1, history_2, history_3,
      personal_1, housing_1, housing_2, employed
    )
  )

# Se editan los datos del modelo
datos_banco_ponderado_modelo_5 <- datos_banco_normalizados %>%
  select(
    !c(foreign, persons, job, credits, age, rate, residence, savings,
      status_1, status_2, status_3, history_1, history_2, history_3,
      personal_1, housing_1, housing_2, employed
    )
  )

# Se procede con los cálculos y gráficos
modelo_5 <- valores_kmeans_df(
  datos = datos_banco_ponderado_modelo_5,
  clusteres = 20,
  intentos_por_k = 50,
  iteraciones_max = 50
)

codo_modelo_5 <- ggplot(
  data = modelo_5[[length(modelo_5)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Variables del modelo 5.",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()

silueta_modelo_5 <- ggplot(
  data = modelo_5[[length(modelo_5)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Variables del modelo 5.",
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  theme_light()

codo_modelo_5 + silueta_modelo_5

# Se compara la información para evaluar el peso de las variables
# K 10
moldeo_5_k10 <- kmeans(
  x = datos_banco_ponderado_modelo_5,
  centers = 10,
  nstart = 50,
  iter.max = 50
)

moldeo_5_k10$size
moldeo_5_k10_df <- datos_banco_tratados_modelo_5 %>%
  mutate(k_10 = moldeo_5_k10$cluster) %>%
  group_by(k_10) %>%
  summarise_all(mean)
View(moldeo_5_k10_df)

# K 9
moldeo_5_k9 <- kmeans(
  x = datos_banco_ponderado_modelo_5,
  centers = 9,
  nstart = 50,
  iter.max = 50
)

moldeo_5_k9$size
moldeo_5_k9_df <- datos_banco_tratados_modelo_5 %>%
  mutate(k_9 = moldeo_5_k9$cluster) %>%
  group_by(k_9) %>%
  summarise_all(mean)
View(moldeo_5_k9_df)

# K 12
moldeo_5_k12 <- kmeans(
  x = datos_banco_ponderado_modelo_5,
  centers = 12,
  nstart = 50,
  iter.max = 50
)

moldeo_5_k12$size
moldeo_5_k12_df <- datos_banco_tratados_modelo_5 %>%
  mutate(k_12 = moldeo_5_k12$cluster) %>%
  group_by(k_12) %>%
  summarise_all(mean)
View(moldeo_5_k12_df)


# Datos tratados, seleccionados desde los datos numéricos no ordin --------
# Se editan los datos para comparación
# Candidato 1: amount, property. K: 5, 6, 7 ,8
# Candidato 2: amount, status_1, status_2, personal_2, personal_3. K: 9
# Candidato 3: amount, status_1, status_2, personal_2. K: 6
# Se probó history, status, amount, rate, personal, age, employed, property,
#savings, telephone, credit, residence
datos_banco_tratados_modelo_6 <- datos_banco_tratados %>%
  select(
    amount, status_1, status_2, personal_2
  )

# Se editan los datos del modelo
datos_banco_ponderado_modelo_6 <- datos_banco_normalizados %>%
  select(
    amount, status_1, status_2, personal_2
  )

# Se procede con los cálculos y gráficos
modelo_6 <- valores_kmeans_df(
  datos = datos_banco_ponderado_modelo_6,
  clusteres = 50,
  intentos_por_k = 50,
  iteraciones_max = 50
)

codo_modelo_6 <- ggplot(
  data = modelo_6[[length(modelo_6)]],
  aes(x = N_clusters, y = Suma_cuadratica)
) +
  geom_line() +
  geom_point() +
  geom_segment(
    mapping = aes(x = 7.5, y = 150, xend = 6.3, yend = 50),
    arrow = arrow(length = unit(0.5, "cm")),
    linewidth = 1,
    color = "#ED0000FF"
  ) +
  labs(
    title = "Valores de K por suma cuadrática de distancias.",
    subtitle = "Variables: amount, status_1, status_2 & personal_2",
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  theme_light()
codo_modelo_6

silueta_modelo_6 <- ggplot(
  data = modelo_6[[length(modelo_6)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  geom_segment(
    mapping = aes(x = 14, y = 0.85, xend = 7.0, yend = 0.855),
    arrow = arrow(length = unit(0.5, "cm")),
    linewidth = 1,
    color = "#00468BFF"
  ) +
  labs(
    title = "Valores de K por cohesión y separación de clústeres.",
    subtitle = "Variables: amount, status_1, status_2 & personal_2",
    x = "Número de clústers",
    y = "Valor de silueta promedio"
  ) +
  theme_light()
silueta_modelo_6

codo_modelo_6 + silueta_modelo_6

# Se compara la información para evaluar el peso de las variables
# K 2
modelo_6_k2 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 2,
  nstart = 50,
  iter.max = 50
)

modelo_6_k2$size
modelo_6_k2_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_2 = modelo_6_k2$cluster) %>%
  group_by(k_2) %>%
  summarise_all(mean)
View(modelo_6_k2_df)

# K 3
modelo_6_k3 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 3,
  nstart = 50,
  iter.max = 50
)

modelo_6_k3$size
modelo_6_k3_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_3 = modelo_6_k3$cluster) %>%
  group_by(k_3) %>%
  summarise_all(mean)
View(modelo_6_k3_df)

# K 4
modelo_6_k4 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 4,
  nstart = 50,
  iter.max = 50
)

modelo_6_k4$size
modelo_6_k4_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_4 = modelo_6_k4$cluster) %>%
  group_by(k_4) %>%
  summarise_all(mean)
View(modelo_6_k4_df)

# K 6
# Este es el valor de K que finalmente se usará en el informe.
modelo_6_k6 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 6,
  nstart = 50,
  iter.max = 50
)

# Los valores de mediana de las variables del modelo 6 para cada clúster
modelo_6_k6$size
modelo_6_k6_df <- datos_banco_tratados_modelo_6 %>%
  mutate(Clúster = as.factor(modelo_6_k6$cluster)) %>%
  group_by(Clúster) %>%
  summarise_all(median)
View(modelo_6_k6_df)

# Se hace la tabla de una forma más visual
grid.table(
  d = modelo_6_k6_df,
  row = NULL,
  theme = ttheme_default(
    core = list(
      bg_params = list(fill = c("lightblue1", "azure"), col = "black")
    ),
    colhead = list(
      bg_params = list(fill = "deepskyblue2", col = "black"),
      fg_params = list(col = "white")
    )
  )
)

# Se arregla el dataframe para el gráfico de boxplots
modelo_6_k6_grafico_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_6 = as.factor(modelo_6_k6$cluster)) %>%
  group_by(k_6) %>%
  mutate(outlier = es_outlier(amount)) %>%
  ungroup()

# Gráfico comparando "amount" en los distintos clústeres
modelo_6_k6_grafico <- ggplot(data = modelo_6_k6_grafico_df) +
  geom_boxplot(
    mapping = aes(x = k_6, y = amount, fill = k_6),
    colour = "#000000",
    outlier.alpha = 0
  ) +
  geom_point(
    data = modelo_6_k6_grafico_df[modelo_6_k6_grafico_df$outlier, ],
    mapping = aes(x = k_6, y = amount, colour = k_6, alpha = 0.2)
  ) +
  geom_hline(
    yintercept = 2320, # Marca la mediana de amount
    linetype = "dashed",
    linewidth = 1,
    color = "#AD002AFF"
  ) +
  theme_light() +
  labs(
    y = "Monto del crédito (DEM)",
    x = "Clústeres"
  ) +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0, 20000, by = 2000),
    labels = formateador,
  ) +
  scale_fill_lancet() +
  scale_colour_lancet()
modelo_6_k6_grafico

# Gráfico de la silueta de 6 clústeres para el modelo 6
modelo_6_k6_siluetas <- silhouette(
  x = modelo_6_k6$cluster,
  dist = dist(datos_banco_ponderado_modelo_6)
)
summary(modelo_6_k6_siluetas)
modelo_6_k6_grafico_silueta <- fviz_silhouette(sil.obj = modelo_6_k6_siluetas) +
  scale_colour_lancet() +
  scale_fill_lancet() +
  labs(
    title = "Distribución de las siluetas para K = 6.",
    subtitle = "Variables: amount, status_1, status_2 & personal_2",
    y = "Valor de silueta",
    fill = "Clústeres",
    colour = "Clústeres"
  ) +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )
modelo_6_k6_grafico_silueta$layers[[2]]$aes_params$colour <- "#AD002AFF"
modelo_6_k6_grafico_silueta$layers[[2]]$aes_params$linewidth <- 1
modelo_6_k6_grafico_silueta

# K 7
modelo_6_k7 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 7,
  nstart = 50,
  iter.max = 50
)

modelo_6_k7$size
modelo_6_k7_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_7 = modelo_6_k7$cluster) %>%
  group_by(k_7) %>%
  summarise_all(mean)
View(modelo_6_k7_df)

# K 8
modelo_6_k8 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 8,
  nstart = 50,
  iter.max = 50
)

modelo_6_k8$size
modelo_6_k8_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_8 = modelo_6_k8$cluster) %>%
  group_by(k_8) %>%
  summarise_all(mean)
View(modelo_6_k8_df)

# K 9
modelo_6_k9 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 9,
  nstart = 50,
  iter.max = 50
)

modelo_6_k9$size
modelo_6_k9_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_9 = modelo_6_k9$cluster) %>%
  group_by(k_9) %>%
  summarise_all(mean)
View(modelo_6_k9_df)

# K 10
modelo_6_k10 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 10,
  nstart = 50,
  iter.max = 50
)

modelo_6_k10$size
modelo_6_k10_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_10 = modelo_6_k10$cluster) %>%
  group_by(k_10) %>%
  summarise_all(mean)
View(modelo_6_k10_df)

# K 12
modelo_6_k12 <- kmeans(
  x = datos_banco_ponderado_modelo_6,
  centers = 12,
  nstart = 50,
  iter.max = 50
)

modelo_6_k12$size
modelo_6_k12_df <- datos_banco_tratados_modelo_6 %>%
  mutate(k_12 = modelo_6_k12$cluster) %>%
  group_by(k_12) %>%
  summarise_all(mean)
View(modelo_6_k12_df)
