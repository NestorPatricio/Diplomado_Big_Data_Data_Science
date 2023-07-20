# Apuntes Clase 4

# Se configura el directorio de trabajo
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/',
  'Visualización de datos aplicada/'
))

# Se carga el archivo
legos <- read.csv('legosets.csv')
summary(legos)
glimpse(legos)

# Se cargan las librerias
library(dplyr)
library(ggplot2)
library(ggforce)

lego_modificado <- legos %>% 
  mutate(Decade = floor(Year/10) * 10)

# Precio en USD por número de piezas
grafico_1 <- lego_modificado %>% 
  ggplot(aes(x = Pieces, y = USD_MSRP)) +
  geom_point(colour = 'grey45') +
  theme_minimal()
grafico_1

# Use de geom_mark_circle de la librería ggforce
# Marca los valores que cumplen los criterios de filter
grafico_2 <- grafico_1 +
  geom_mark_circle(
    aes(
      filter = coalesce(Pieces, 0) > 4000 | coalesce(USD_MSRP, 0) > 600,
      label = Name,
      decription = Availability
    ),
    colour = 'grey50',
    fill = 'grey90'
  ) +
  theme_minimal()
grafico_2

# Barplot
grafico_3 <- lego_modificado %>%
  count(Theme) %>%
  arrange(n) %>% 
  mutate(Theme_Factor = as.factor(Theme)) %>%
  arrange(Theme_Factor) %>%
  ggplot() +
  geom_bar(
    aes(x = Theme)
  )
grafico_3
