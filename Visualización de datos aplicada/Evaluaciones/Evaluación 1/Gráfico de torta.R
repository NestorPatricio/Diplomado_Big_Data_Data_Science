# Gráfico de torta o pie chart

# Este gráfico no quedó en la evaluación 2, pero me parece bueno conservarlo
# para el futuro


# Prolegómenos ------------------------------------------------------------

# Carga de librerías
librerias <- c(
  "dplyr",
  "ggplot2",
  "lintr",
  "datos"
)
for (libreria in librerias) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria)
    library(libreria, character.only = TRUE)
  }
}

# Se configura el directorio de trabajo
setwd(paste0(
  "/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Visualización de",
  " datos aplicada/Evaluaciones/Evaluación 1/"
))

# Se ejecuta el linter para evaluar el estilo del script
lint("Gráfico de torta.R")


# Construcción del gráfico ------------------------------------------------

colores <- c("#0015BC", "#DE0100")
presidencial_arreglado <- presidencial %>%
  mutate(
    anos_ejercicio = as.double(fin - inicio) / 365.25,
    anos_ejercicio = ifelse(
      partido == "Demócrata",
      anos_ejercicio * -1,
      anos_ejercicio
    )
  )

partidos <- presidencial_arreglado %>%
  mutate(anos_ejercicio = abs(anos_ejercicio)) %>%
  group_by(partido) %>%
  summarise(anos_ejercicio = sum(anos_ejercicio))


grafico_partidos <- ggplot(
  data = partidos,
  mapping = aes(x = "", y = anos_ejercicio, fill = partido)
) +
  geom_bar(width = 1, stat = "identity", colour = "#FFFFFF") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colores) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    line = element_blank(),
    legend.position = c(0.5, 0),
    legend.direction = "horizontal"
  ) +
  labs(
    title = "Proporción de tiempo de presidencia por partido",
    subtitle = "Desde 1953 hasta 2021",
    x = element_blank(),
    y = element_blank(),
    fill = "Partidos"
  )
grafico_partidos
