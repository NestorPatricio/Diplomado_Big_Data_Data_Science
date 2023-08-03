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
  return(valores - min(valores) / max(valores) - min(valores))
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
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  #scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  #scale_y_continuous() +
  theme_light()

silueta_total_crudo <- ggplot(
  data = modelo_total_crudo[[length(modelo_total_crudo)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  #scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  #scale_y_continuous() +
  theme_light()

codo_total_crudo + silueta_total_crudo


# Usando todas las variables de la base de datos con tratamiento
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
  lapply(X =datos_banco_tratados, FUN = normalizacion)
)

# Pendiente ponderar las variables
modelo_total_tratado <- valores_kmeans_df(
  datos = datos_banco,
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
    x = "Número de clústers",
    y = "Suma cuadrática"
  ) +
  #scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  #scale_y_continuous() +
  theme_light()

silueta_total_tratado <- ggplot(
  data = modelo_total_tratado[[length(modelo_total_tratado)]],
  aes(x = N_clusters, y = Silueta)
) +
  geom_line() +
  geom_point() +
  labs(
    x = "Número de clústers",
    y = "Valor de silueta"
  ) +
  #scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  #scale_y_continuous() +
  theme_light()

codo_total_tratado + silueta_total_tratado
