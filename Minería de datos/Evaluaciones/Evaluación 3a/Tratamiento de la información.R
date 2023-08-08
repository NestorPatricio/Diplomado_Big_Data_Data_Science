# Evaluación 3
# Néstor Patricio Rojas Ríos

############################## Prolegómenos ##############################
# Configuración del espacio de trabajo
# Linux
setwd(paste0(
  "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/",
  "Minería de datos/Evaluaciones/Evaluación 3"
))

# Windows
setwd(paste0(
  "C:\\Users\\nproj\\Documents\\Diplomado_Big_Data_Data_Science\\Minería de ",
  "datos\\Evaluaciones\\Evaluación 3"
))

# Instalación de librerías


# Carga de librerías
library(lintr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cluster)

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
  lista_final <- list()
  dataframe_final <- data.frame(
    N_clusters = 1:clusteres,
    Suma_cuadratica = NA,
    Silueta = NA
  )

  # Acá se genra la información que saldrá
  for (k in 1:clusteres) {
    print(k)
    kmedio_aux <- kmeans(
      x = datos,
      centers = k,
      nstart = intentos_por_k,
      iter.max = iteraciones_max,
      algorithm = algoritmo
    )
    print(kmedio_aux$centers)

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
  return((valores - min(valores)) / (max(valores) - min(valores)))
}


############################## Desarrollo ##############################

############################## Pregunta 1 ##############################
# Usando todas las variables de la base de datos sin tratamiento
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


# Usando todas las variables de la base de datos con tratamiento
# Se tratan los datos
datos_banco_tratados <- datos_banco %>%
  mutate(
    status_1 = ifelse(status == 1, 1, 0),
    status_2 = ifelse(status == 2, 1, 0),
    status_3 = ifelse(status == 3, 1, 0),
    history_1 = ifelse(history == 1, 1, 0),
    history_2 = ifelse(history == 2, 1, 0),
    history_3 = ifelse(history == 3, 1, 0),
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

# Se procede con los cálculos y gráficos
modelo_total_tratado <- valores_kmeans_df(
  datos = datos_banco_ponderados,
  clusteres = 50,
  intentos_por_k = 20,
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


# Usando todas las variables de la base de datos con tratamiento
# Se editan los datos
datos_banco_tratados_modelo_3 <- datos_banco_tratados %>%
  select(
    amount, savings, telephone, credit, status_1
  )

datos_banco_ponderado_modelo_3 <- datos_banco_normalizados %>%
  select(
    amount, savings, telephone, credit, status_1
  )

# Se procede con los cálculos y gráficos
modelo_3 <- valores_kmeans_df(
  datos = datos_banco_ponderado_modelo_3,
  clusteres = 16,
  intentos_por_k = 20,
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
moldeo_3_k12 <- kmeans(
  x = datos_banco_ponderado_modelo_3,
  centers = 12,
  nstart = 20,
  iter.max = 50
)

moldeo_3_k12$size
moldeo_3_k12_df <- datos_banco_tratados_modelo_3 %>%
  mutate(k_12 = moldeo_3_k12$cluster) %>%
  group_by(k_12) %>%
  summarise_all(mean)
View(moldeo_3_k12_df)

moldeo_3_k16 <- kmeans(
  x = datos_banco_ponderado_modelo_3,
  centers = 16,
  nstart = 20,
  iter.max = 50
)

moldeo_3_k16$size
moldeo_3_k16_df <- datos_banco_tratados_modelo_3 %>%
  mutate(k_16 = moldeo_3_k16$cluster) %>%
  group_by(k_16) %>%
  summarise_all(mean)
View(moldeo_3_k16_df)


# Usando todas las variables de la base de datos con tratamiento
# Se editan los datos
datos_banco_tratados_modelo_4 <- datos_banco_tratados %>%
  select(
    telephone, credit, status_1
  )

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
