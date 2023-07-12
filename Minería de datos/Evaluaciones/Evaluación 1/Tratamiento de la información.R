# Evaluación 1.

# Configurando área de trabajo e importando datos.
library(tidyverse)
setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Evaluaciones/Evaluación 1'
))
datos_paises <- read.csv('DatosPaises.csv', sep = ';')
summary(datos_paises)
head(datos_paises, n = 10)

# Búsqueda de valores NA.
hay_na <- c()
for(dato in names(datos_paises)) {
  datos_na = sum(is.na(datos_paises[, dato]))
  if(datos_na > 0) {
    hay_na <- c(hay_na, dato)
  }
}
hay_na

# Un primer vistazo a los datos:
# Población en millones de personas.
plot(datos_paises$POB, datos_paises$PIB)
cor(datos_paises$POB, datos_paises$PIB)

# Índice de desarrollo humano.
plot(datos_paises$IDH, datos_paises$PIB)
cor(datos_paises$IDH, datos_paises$PIB)

# Coeficiente de GINI.
plot(datos_paises$GINI, datos_paises$PIB)
cor(datos_paises$GINI, datos_paises$PIB)

# Índice de precio al consumidor.
plot(datos_paises$IPC, datos_paises$PIB)
cor(datos_paises$IPC, datos_paises$PIB)

# Índice de precios alimenticios de la FAO.
plot(datos_paises$FAO, datos_paises$PIB)
cor(datos_paises$FAO, datos_paises$PIB)

# Índice de desigualdad de género.
plot(datos_paises$GENERO, datos_paises$PIB)
cor(datos_paises$GENERO, datos_paises$PIB)

# Tasa de electrificación.
plot(datos_paises$ELECTRICIDAD, datos_paises$PIB)
cor(datos_paises$ELECTRICIDAD, datos_paises$PIB)

# Años de escolaridad promedio.
plot(datos_paises$ESCOLARIDAD, datos_paises$PIB)
cor(datos_paises$ESCOLARIDAD, datos_paises$PIB)

# Tasa de suicidios femeninos cada 100.000 personas.
plot(datos_paises$SUICIDIOFEM, datos_paises$PIB)
cor(datos_paises$SUICIDIOFEM, datos_paises$PIB)

# Tasa de suicidios masculinos cada 100.000 personas.
plot(datos_paises$SUICIDIOMAS, datos_paises$PIB)
cor(datos_paises$SUICIDIOMAS, datos_paises$PIB)

# Porcentaje de superficie que corresponde a bosques.
plot(datos_paises$BOSQUE, datos_paises$PIB)
cor(datos_paises$BOSQUE, datos_paises$PIB)

# Porcentaje de uso de combustibles fósiles.
plot(datos_paises$FOSIL, datos_paises$PIB)
cor(datos_paises$FOSIL, datos_paises$PIB)

# Emisiones de dióxido de carbono en toneladas per cápita.
plot(datos_paises$DIOXIDO, datos_paises$PIB)
cor(datos_paises$DIOXIDO, datos_paises$PIB)

# Población afectada por desastres naturales en miles.
plot(datos_paises$DESASTRE, datos_paises$PIB)
cor(datos_paises$DESASTRE, datos_paises$PIB)

# Población sin hogar por desastres naturales.
plot(datos_paises$AFECTADOS, datos_paises$PIB)
cor(datos_paises$AFECTADOS, datos_paises$PIB)

# Tasa de homicidios cada 100.000 personas.
plot(datos_paises$HOMICIDIO, datos_paises$PIB)
cor(datos_paises$HOMICIDIO, datos_paises$PIB)

# Mortalidad de niños menores de 5 años en miles.
plot(datos_paises$MORTINF, datos_paises$PIB)
cor(datos_paises$MORTINF, datos_paises$PIB)

# Tasa de mortalidad maternal cada 100 nacimientos.
plot(datos_paises$MORTMAT, datos_paises$PIB)
cor(datos_paises$MORTMAT, datos_paises$PIB)

# Turistas internacionales en millones.
plot(datos_paises$TURISMO, datos_paises$PIB)
cor(datos_paises$TURISMO, datos_paises$PIB)

# Porcentaje de uso de internet.
plot(datos_paises$INTERNET, datos_paises$PIB)
cor(datos_paises$INTERNET, datos_paises$PIB)

# Porcentaje de vćtima de violencia entre parejas.
plot(datos_paises$VIOLENCIA, datos_paises$PIB)
cor(datos_paises$VIOLENCIA, datos_paises$PIB)

# Expectativa de vida en años.
plot(datos_paises$VIDA, datos_paises$PIB)
cor(datos_paises$VIDA, datos_paises$PIB)

# Subscripciones a telefonía celular cada 1.000 personas.
plot(datos_paises$CELULAR, datos_paises$PIB)
cor(datos_paises$CELULAR, datos_paises$PIB)

# Tasa de deserción escolar primaria.
plot(datos_paises$DESERCION, datos_paises$PIB)
cor(datos_paises$DESERCION, datos_paises$PIB)

# Tasa de encarcelamiento cada 100.000 personas.
plot(datos_paises$PRISION, datos_paises$PIB)
cor(datos_paises$PRISION, datos_paises$PIB)

# Porcentaje de uso de energía renovable.
plot(datos_paises$RENOVABLE, datos_paises$PIB)
cor(datos_paises$RENOVABLE, datos_paises$PIB)

# Porcentaje de escaños parlamentarios ocupados por mujeres.
plot(datos_paises$PARLAMENTO, datos_paises$PIB)
cor(datos_paises$PARLAMENTO, datos_paises$PIB)

# Porcentaje de población que es inmigrante.
plot(datos_paises$INMIGRANTES, datos_paises$PIB)
cor(datos_paises$INMIGRANTES, datos_paises$PIB)

############### Identificación de candidatos ouliers ###############

# IDH
respuesta1_a <- datos_paises %>%
  ggplot() +
  aes(x = IDH) +
  geom_histogram() +
  labs(
    x = 'Índice de Desarrollo Humano',
    y = 'Cantidad'
  )
respuesta1_a

media_idh <- mean(datos_paises$IDH)
de_idh <- sd(datos_paises$IDH)
cuantiles_idh <- quantile(datos_paises$IDH, c(0.01, 0.99))
lim_inf_idh <- cuantiles_idh[1]
lim_sup_idh <- cuantiles_idh[2]

# Percentiles 1% y 99%
candidatos_percentil_idh <- datos_paises %>% 
  arrange(IDH) %>% 
  filter(IDH < lim_inf_idh | IDH > lim_sup_idh)

respuesta1_a_percentil <- datos_paises %>%
  ggplot() +
  aes(x = IDH, fill = PAIS %in% candidatos_percentil_idh$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice de Desarrollo Humano',
    y = 'Cantidad',
    title = 'Candidatos a outliers por percentiles 1% a 99%'
  ) +
  theme(legend.position = 'none')
respuesta1_a_percentil

# Intervalo de variabilidad con delta 2
candidatos_intervalo_idh <- datos_paises %>% 
  arrange(IDH) %>% 
  filter(IDH < (media_idh - (2 * de_idh)) | IDH > (media_idh + (2 * de_idh)))

respuesta1_a_intervalo <- datos_paises %>%
  ggplot() +
  aes(x = IDH, fill = PAIS %in% candidatos_intervalo_idh$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice de Desarrollo Humano',
    y = 'Cantidad',
    title = 'Candidatos a outliers por intervalo de variabilidad con delta 2'
  ) +
  theme(legend.position = 'none')
respuesta1_a_intervalo

# Valor-Z robusto con delta 3
candidatos_z_idh <- datos_paises %>% 
  mutate(
    Error_IDH = abs(IDH - median(datos_paises$IDH)),
    Valor_Z_IDH = Error_IDH / median(Error_IDH)
  ) %>% 
  arrange(Valor_Z_IDH) %>% 
  filter(Valor_Z_IDH > 3)

respuesta1_a_z <- datos_paises %>%
  ggplot() +
  aes(x = IDH, fill = PAIS %in% candidatos_z_idh$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice de Desarrollo Humano',
    y = 'Cantidad',
    title = 'Candidatos a outliers por Valor-Z robusto con delta 3'
  ) +
  theme(legend.position = 'none')
respuesta1_a_z

# Comparación de promedio y desviación estándar
parametros_percentil_idh <- datos_paises %>% 
  filter(IDH > lim_inf_idh & IDH < lim_sup_idh) %>% 
  summarise(prom = mean(IDH), desest = sd(IDH), corr = cor(IDH, PIB))

parametros_intervalo_idh <- datos_paises %>% 
  filter(IDH > (media_idh - (2 * de_idh)) & IDH < (media_idh + (2 * de_idh))) %>% 
  summarise(prom = mean(IDH), desest = sd(IDH), corr = cor(IDH, PIB))

parametros_z_idh <- datos_paises %>% 
  mutate(
    Error_IDH = abs(IDH - median(datos_paises$IDH)),
    Valor_Z_IDH = Error_IDH / median(Error_IDH)
  ) %>%
  filter(Valor_Z_IDH < 3) %>% 
  summarise(prom = mean(IDH), desest = sd(IDH), corr = cor(IDH, PIB))

parametros1a <- data.frame(
  'Grupo' = c(
    'IDH Original', 
    'Percentiles 1% al 99%', 
    'Intervalo de variabilidad delta 2', 
    'Valor-Z robusto con delta 3'
  ),
  'Promedio' = c(
    media_idh,
    parametros_percentil_idh$prom,
    parametros_intervalo_idh$prom,
    parametros_z_idh$prom
  ),
  'Desviación_estándar' = c(
    de_idh,
    parametros_percentil_idh$desest,
    parametros_intervalo_idh$desest,
    parametros_z_idh$desest
  ),
  'Correlación_con_PIB' = c(
    cor(datos_paises$IDH, datos_paises$PIB),
    parametros_percentil_idh$corr,
    parametros_intervalo_idh$corr,
    parametros_z_idh$corr
  )
)


# FAO
respuesta1_b <- datos_paises %>%
  ggplot() +
  aes(x = FAO) +
  geom_histogram() +
  labs(
    x = 'Índice del precio de alimentos FAO',
    y = 'Cantidad'
  )
respuesta1_b

media_fao <- mean(datos_paises$FAO)
de_fao <- sd(datos_paises$FAO)
cuantiles_fao <- quantile(datos_paises$FAO, c(0.01, 0.99))
lim_inf_fao <- cuantiles_fao[1]
lim_sup_fao <- cuantiles_fao[2]

# Percentiles 1% y 99%
candidatos_percentil_fao <- datos_paises %>% 
  arrange(FAO) %>% 
  filter(FAO < lim_inf_fao | FAO > lim_sup_fao)

respuesta1_b_percentil <- datos_paises %>%
  ggplot() +
  aes(x = FAO, fill = PAIS %in% candidatos_percentil_fao$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice del precio de alimentos FAO',
    y = 'Cantidad',
    title = 'Candidatos a outliers por percentiles 1% a 99%'
  ) +
  theme(legend.position = 'none')
respuesta1_b_percentil

# Intervalo de variabilidad con delta 3
candidatos_intervalo_fao <- datos_paises %>% 
  arrange(FAO) %>% 
  filter(FAO < (media_fao - (3 * de_fao)) | FAO > (media_fao + (3 * de_fao)))

respuesta1_b_intervalo <- datos_paises %>%
  ggplot() +
  aes(x = FAO, fill = PAIS %in% candidatos_intervalo_fao$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice del precio de alimentos FAO',
    y = 'Cantidad',
    title = 'Candidatos a outliers por intervalo de variabilidad con delta 3'
  ) +
  theme(legend.position = 'none')
respuesta1_b_intervalo

# Valor-Z robusto con delta 4
candidatos_z_fao <- datos_paises %>% 
  mutate(
    Error_FAO = abs(FAO - median(datos_paises$FAO)),
    Valor_Z_FAO = Error_FAO / median(Error_FAO)
  ) %>% 
  arrange(Valor_Z_FAO) %>% 
  filter(Valor_Z_FAO > 4)

respuesta1_b_z <- datos_paises %>%
  ggplot() +
  aes(x = FAO, fill = PAIS %in% candidatos_z_fao$PAIS) +
  geom_histogram() +
  labs(
    x = 'Índice del precio de alimentos FAO',
    y = 'Cantidad',
    title = 'Candidatos a outliers por Valor-Z robusto con delta 4'
  ) +
  theme(legend.position = 'none')
respuesta1_b_z

# Comparación de promedio y desviación estándar
parametros_percentil_fao <- datos_paises %>% 
  filter(FAO > lim_inf_fao & FAO < lim_sup_fao) %>% 
  summarise(prom = mean(FAO), desest = sd(FAO), corr = cor(FAO, PIB))

parametros_intervalo_fao <- datos_paises %>% 
  filter(FAO > (media_fao - (3 * de_fao)) & FAO < (media_fao + (3 * de_fao))) %>% 
  summarise(prom = mean(FAO), desest = sd(FAO), corr = cor(FAO, PIB))

parametros_z_fao <- datos_paises %>% 
  mutate(
    Error_FAO = abs(FAO - median(datos_paises$FAO)),
    Valor_Z_FAO = Error_FAO / median(Error_FAO)
  ) %>%
  filter(Valor_Z_FAO < 4) %>% 
  summarise(prom = mean(FAO), desest = sd(FAO), corr = cor(FAO, PIB))

parametros1b <- data.frame(
  'Grupo' = c(
    'FAO Original', 
    'Percentiles 1% al 99%', 
    'Intervalo de variabilidad delta 3', 
    'Valor-Z robusto con delta 4'
  ),
  'Promedio' = c(
    media_fao,
    parametros_percentil_fao$prom,
    parametros_intervalo_fao$prom,
    parametros_z_fao$prom
  ),
  'Desviación_estándar' = c(
    de_fao,
    parametros_percentil_fao$desest,
    parametros_intervalo_fao$desest,
    parametros_z_fao$desest
  ),
  'Correlación_con_PIB' = c(
    cor(datos_paises$FAO, datos_paises$PIB),
    parametros_percentil_fao$corr,
    parametros_intervalo_fao$corr,
    parametros_z_fao$corr
  )
)


# INTERNET
respuesta1_a <- datos_paises %>%
  ggplot() +
  aes(x = INTERNET) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de uso de internet',
    y = 'Cantidad'
  )
respuesta1_a

media_internet <- mean(datos_paises$INTERNET)
de_internet <- sd(datos_paises$INTERNET)
cuantiles_internet <- quantile(datos_paises$INTERNET, c(0.01, 0.99))
lim_inf_internet <- cuantiles_internet[1]
lim_sup_internet <- cuantiles_internet[2]

# Percentiles 1% y 99%
candidatos_percentil_internet <- datos_paises %>% 
  arrange(INTERNET) %>% 
  filter(INTERNET < lim_inf_internet | INTERNET > lim_sup_internet)

respuesta1_c_percentil <- datos_paises %>%
  ggplot() +
  aes(x = INTERNET, fill = PAIS %in% candidatos_percentil_internet$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de uso de internet',
    y = 'Cantidad',
    title = 'Candidatos a outliers por percentiles 1% a 99%'
  ) +
  theme(legend.position = 'none')
respuesta1_c_percentil

# Intervalo de variabilidad con delta 1.8
candidatos_intervalo_internet <- datos_paises %>% 
  arrange(INTERNET) %>% 
  filter(INTERNET < (media_internet - (1.8 * de_internet)) | INTERNET > (media_internet + (1.8 * de_internet)))

respuesta1_c_intervalo <- datos_paises %>%
  ggplot() +
  aes(x = INTERNET, fill = PAIS %in% candidatos_intervalo_internet$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de uso de internet',
    y = 'Cantidad',
    title = 'Candidatos a outliers por intervalo de variabilidad con delta 1,8'
  ) +
  theme(legend.position = 'none')
respuesta1_c_intervalo

# Valor-Z robusto con delta 2
candidatos_z_internet <- datos_paises %>% 
  mutate(
    Error_INTERNET = abs(INTERNET - median(datos_paises$INTERNET)),
    Valor_Z_INTERNET = Error_INTERNET / median(Error_INTERNET)
  ) %>% 
  arrange(Valor_Z_INTERNET) %>% 
  filter(Valor_Z_INTERNET > 2)

respuesta1_c_z <- datos_paises %>%
  ggplot() +
  aes(x = INTERNET, fill = PAIS %in% candidatos_z_internet$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de uso de internet',
    y = 'Cantidad',
    title = 'Candidatos a outliers por Valor-Z robusto con delta 2'
  ) +
  theme(legend.position = 'none')
respuesta1_c_z

# Comparación de promedio y desviación estándar
parametros_percentil_internet <- datos_paises %>% 
  filter(INTERNET > lim_inf_internet & INTERNET < lim_sup_internet) %>% 
  summarise(prom = mean(INTERNET), desest = sd(INTERNET), corr = cor(INTERNET, PIB))

parametros_intervalo_internet <- datos_paises %>% 
  filter(INTERNET > (media_internet - (1.8 * de_internet)) & INTERNET < (media_internet + (1.8 * de_internet))) %>% 
  summarise(prom = mean(INTERNET), desest = sd(INTERNET), corr = cor(INTERNET, PIB))

parametros_z_internet <- datos_paises %>% 
  mutate(
    Error_INTERNET = abs(INTERNET - median(datos_paises$INTERNET)),
    Valor_Z_INTERNET = Error_INTERNET / median(Error_INTERNET)
  ) %>%
  filter(Valor_Z_INTERNET < 2) %>% 
  summarise(prom = mean(INTERNET), desest = sd(INTERNET), corr = cor(INTERNET, PIB))

parametros1c <- data.frame(
  'Grupo' = c(
    'INTERNET Original', 
    'Percentiles 1% al 99%', 
    'Intervalo de variabilidad delta 1.8', 
    'Valor-Z robusto con delta 2'
  ),
  'Promedio' = c(
    media_internet,
    parametros_percentil_internet$prom,
    parametros_intervalo_internet$prom,
    parametros_z_internet$prom
  ),
  'Desviación_estándar' = c(
    de_internet,
    parametros_percentil_internet$desest,
    parametros_intervalo_internet$desest,
    parametros_z_internet$desest
  ),
  'Correlación_con_PIB' = c(
    cor(datos_paises$INTERNET, datos_paises$PIB),
    parametros_percentil_internet$corr,
    parametros_intervalo_internet$corr,
    parametros_z_internet$corr
  )
)


# INMIGRANTES
respuesta1_d <- datos_paises %>%
  ggplot() +
  aes(x = INMIGRANTES) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de población inmigrante',
    y = 'Cantidad'
  )
respuesta1_d

media_inmigrantes <- mean(datos_paises$INMIGRANTES)
de_inmigrantes <- sd(datos_paises$INMIGRANTES)
cuantiles_inmigrantes <- quantile(datos_paises$INMIGRANTES, c(0.01, 0.99))
lim_inf_inmigrantes <- cuantiles_inmigrantes[1]
lim_sup_inmigrantes <- cuantiles_inmigrantes[2]

# Percentiles 1% y 99%
candidatos_percentil_inmigrantes <- datos_paises %>% 
  arrange(INMIGRANTES) %>% 
  filter(INMIGRANTES < lim_inf_inmigrantes | INMIGRANTES > lim_sup_inmigrantes)

respuesta1_d_percentil <- datos_paises %>%
  ggplot() +
  aes(x = INMIGRANTES, fill = PAIS %in% candidatos_percentil_inmigrantes$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de población inmigrante',
    y = 'Cantidad',
    title = 'Candidatos a outliers por percentiles 1% a 99%'
  ) +
  theme(legend.position = 'none')
respuesta1_d_percentil

# Intervalo de variabilidad con delta 4
candidatos_intervalo_inmigrantes <- datos_paises %>% 
  arrange(INMIGRANTES) %>% 
  filter(INMIGRANTES < (media_inmigrantes - (4 * de_inmigrantes)) | INMIGRANTES > (media_inmigrantes + (4 * de_inmigrantes)))

respuesta1_d_intervalo <- datos_paises %>%
  ggplot() +
  aes(x = INMIGRANTES, fill = PAIS %in% candidatos_intervalo_inmigrantes$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de población inmigrante',
    y = 'Cantidad',
    title = 'Candidatos a outliers por intervalo de variabilidad con delta 4'
  ) +
  theme(legend.position = 'none')
respuesta1_d_intervalo

# Valor-Z robusto con delta 6
candidatos_z_inmigrantes <- datos_paises %>% 
  mutate(
    Error_INMIGRANTES = abs(INMIGRANTES - median(datos_paises$INMIGRANTES)),
    Valor_Z_INMIGRANTES = Error_INMIGRANTES / median(Error_INMIGRANTES)
  ) %>% 
  arrange(Valor_Z_INMIGRANTES) %>% 
  filter(Valor_Z_INMIGRANTES > 6)

respuesta1_d_z <- datos_paises %>%
  ggplot() +
  aes(x = INMIGRANTES, fill = PAIS %in% candidatos_z_inmigrantes$PAIS) +
  geom_histogram() +
  labs(
    x = 'Porcentaje de población inmigrante',
    y = 'Cantidad',
    title = 'Candidatos a outliers por Valor-Z robusto con delta 6'
  ) +
  theme(legend.position = 'none')
respuesta1_d_z

# Comparación de promedio y desviación estándar
parametros_percentil_inmigrantes <- datos_paises %>% 
  filter(INMIGRANTES > lim_inf_inmigrantes & INMIGRANTES < lim_sup_inmigrantes) %>% 
  summarise(prom = mean(INMIGRANTES), desest = sd(INMIGRANTES), corr = cor(INMIGRANTES, PIB))

parametros_intervalo_inmigrantes <- datos_paises %>% 
  filter(INMIGRANTES > (media_inmigrantes - (4 * de_inmigrantes)) & INMIGRANTES < (media_inmigrantes + (4 * de_inmigrantes))) %>% 
  summarise(prom = mean(INMIGRANTES), desest = sd(INMIGRANTES), corr = cor(INMIGRANTES, PIB))

parametros_z_inmigrantes <- datos_paises %>% 
  mutate(
    Error_INMIGRANTES = abs(INMIGRANTES - median(datos_paises$INMIGRANTES)),
    Valor_Z_INMIGRANTES = Error_INMIGRANTES / median(Error_INMIGRANTES)
  ) %>%
  filter(Valor_Z_INMIGRANTES < 6) %>% 
  summarise(prom = mean(INMIGRANTES), desest = sd(INMIGRANTES), corr = cor(INMIGRANTES, PIB))

parametros1d <- data.frame(
  'Grupo' = c(
    'INMIGRANTES Original', 
    'Percentiles 1% al 99%', 
    'Intervalo de variabilidad delta 4', 
    'Valor-Z robusto con delta 6'
  ),
  'Promedio' = c(
    media_inmigrantes,
    parametros_percentil_inmigrantes$prom,
    parametros_intervalo_inmigrantes$prom,
    parametros_z_inmigrantes$prom
  ),
  'Desviación_estándar' = c(
    de_inmigrantes,
    parametros_percentil_inmigrantes$desest,
    parametros_intervalo_inmigrantes$desest,
    parametros_z_inmigrantes$desest
  ),
  'Correlación_con_PIB' = c(
    cor(datos_paises$INMIGRANTES, datos_paises$PIB),
    parametros_percentil_inmigrantes$corr,
    parametros_intervalo_inmigrantes$corr,
    parametros_z_inmigrantes$corr
  )
)

