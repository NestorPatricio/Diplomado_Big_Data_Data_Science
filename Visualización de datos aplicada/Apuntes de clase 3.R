# Apuntes de la clase 3 de Visualización de datos aplicada

setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Visualización ',
  'de datos aplicada'
))
# install.packages(c('datos', 'ggstream'))
install.packages(c('patchwork', 'ggparty'))
library(ggplot2)
library(dplyr)
library(datos)

# Opcionales
library(ggstream)
library(scales)
library(patchwork)
library(ggparty)

# Se seleccionan los datos del último año
paises2 <- paises %>% filter(anio == max(anio))

#################### Script 1 ####################
paises2 %>% # Datos
  ggplot(
    aes(x = pib_per_capita, y = esperanza_de_vida) # Estética
  ) +
  geom_point( # Esta capa le agrega la comparación con el total de los datos
    data = paises2 %>% select(-continente), # Esta selección es por la faceta
    colour = 'gray80'
    ) +
  geom_point( # Geometría
    aes(size = poblacion, colour = continente)
  ) +
  facet_wrap( # Faceta
    vars(continente)
  ) +
  geom_smooth( # Estadísticos
    method = 'lm',
    se = FALSE,
    colour = '#FF0000'
  ) +
  coord_cartesian() + # Coordenadas
  scale_x_log10( # Se cambia a escala logarítmica sólo el eje X
    labels = scales::dollar
  ) +
  theme_light() + # Tema
  labs(
    x = 'PIB per cápita',
    y = 'Esperanza de vida'
  )

#################### Script 2 ####################
grafico <- paises2 %>% 
  ggplot() +
  geom_point(
    aes( x = pib_per_capita, y = esperanza_de_vida),
    colour = 'red',
    fill = 'gray80',
    size = 3,
    shape = 21
  )
grafico

p <- paises2 %>% 
  ggplot() +
  geom_point(
    aes(
      x = pib_per_capita,
      y = esperanza_de_vida,
      colour = continente,
      size = poblacion
    ),
  )

colores_continente <- c( # Se puede setear una paleta
  'África' = '#442288',
  'Américas' = '#6CA2EA',
  'Asia' = '#B5D33D',
  'Europa' = '#FED23F',
  'Oceanía' = '#EB7D5B'
)

p1 <- p +
  scale_colour_manual(
    values = colores_continente
  ) + # Agrega la paleta seteada
  scale_x_continuous(
    labels = scales::dollar
  ) +
  labs(
    x = 'PIB per cápita',
    y = 'Esperanza de vida',
    size = 'Leyenda de\nPoblación',
    colour = 'Continente',
    title = 'Título',
    subtitle = 'Sutítulo',
    caption = 'Caption'
  )
p1

p2 <- p1 +
  scale_x_log10(
    labels = scales::dollar
  ) +
  geom_smooth(
    aes(x = pib_per_capita, y = esperanza_de_vida),
    method = 'lm'
  ) +
  facet_wrap(
    vars(continente)
  ) +
  theme_minimal()
p2
