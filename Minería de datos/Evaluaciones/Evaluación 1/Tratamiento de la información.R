# Evaluación 1.

########## Configuración de área de trabajo e importación de datos ##########
library(dplyr)
library(ggplot2)
library(patchwork)

setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Evaluaciones/Evaluación 1'
))
datos_paises <- read.csv('DatosPaises.csv', sep = ';')
summary(datos_paises)
head(datos_paises, n = 10)


############################## FUNCIONES ##############################

######## Gráficos de variables contra PIB normal y en forma logarítmica ########
graficos_PIB <- function(
  datos,
  variable,
  log_variable = FALSE,
  titulo = '',
  color_smooth
) {
  if(log_variable) {
    valor_x = log(datos[, variable])
    texto_x = paste('Logaritmo de', variable)
  } else {
    valor_x = datos[, variable]
    texto_x = variable
  }
  
  grafico_nolog = datos %>% 
    ggplot(
      aes(x = valor_x, y = PIB)
    ) +
    geom_point() +
    geom_smooth(method = 'lm', colour = color_smooth) +
    labs(
      title = titulo,
      x = texto_x,
      y = 'PIB'
    ) + theme_minimal()
  
  grafico_log = datos %>% 
    ggplot(
      aes(x = valor_x, y = log(PIB))
    ) +
    geom_point() +
    geom_smooth(method = 'lm', colour = color_smooth) +
    labs(
      title = titulo,
      x = texto_x,
      y = 'Logaritmo de PIB'
    ) +
    theme_minimal()
  return(list(grafico_nolog, grafico_log))
}


######################### Histograma univariable #########################
histograma <- function(
  variable,
  datos_originales,
  candidatos,
  log_variable = FALSE,
  titulo = ''
) {
  if(log_variable) {
    datos_originales[variable] = log(datos_originales[variable])
    nombre_x = paste('Logaritmo de', variable)
  } else {
    nombre_x = variable
  }
  return(
    datos_originales %>%
      ggplot(aes(x = datos_originales[, variable])) +
      geom_histogram(
        aes(fill = PAIS %in% candidatos$PAIS)
      ) +
      labs(
        title = titulo,
        x = nombre_x,
        y = 'Registros'
      ) +
      theme_minimal() +
      theme(legend.position = 'none')
  )
}


#################### Identificación de candidatos ouliers ####################
# Por percentiles 1% y 99%
por_percentil <- function(
  variable,
  log_variable = FALSE,
  candidatos = TRUE
) {
  if(log_variable) {
    nombre_variable = paste0(variable, '_log')
    dataframe = datos_paises
    dataframe[nombre_variable] = log(datos_paises[, variable])
    dataframe = dataframe %>% arrange(dataframe[, nombre_variable])
  } else {
    nombre_variable = variable
    dataframe = datos_paises %>% arrange(datos_paises[, nombre_variable])
  }
  
  cuantiles = quantile(dataframe[, nombre_variable], c(0.01, 0.99))
  lim_inf = cuantiles[1]
  lim_sup = cuantiles[2]

  if(candidatos) {
    dataframe = dataframe %>% 
      filter(
        dataframe[, nombre_variable] < lim_inf
        | dataframe[, nombre_variable] > lim_sup
      )
  } else {
    dataframe = dataframe %>% 
      filter(
        dataframe[, nombre_variable] > lim_inf
        & dataframe[, nombre_variable] < lim_sup
      )
  }
  return(dataframe)
}

# Por intervalo
por_intervalo <- function(
  variable,
  delta,
  log_variable = FALSE,
  candidatos = TRUE
) {
  if(log_variable) {
    nombre_variable = paste0(variable, '_log')
    dataframe = datos_paises
    dataframe[nombre_variable] = log(datos_paises[, variable])
    dataframe = dataframe %>% arrange(dataframe[, nombre_variable])
  } else {
    nombre_variable = variable
    dataframe = datos_paises %>% arrange(datos_paises[, nombre_variable])
  }
  
  promedio <- mean(dataframe[, nombre_variable])
  desviacion <- sd(dataframe[, nombre_variable])
  
  if(candidatos) {
    dataframe = dataframe %>% 
      filter(
        dataframe[, nombre_variable] < (promedio - (delta * desviacion))
        | dataframe[, nombre_variable] > (promedio + (delta * desviacion))
      )
  } else {
    dataframe = dataframe %>% 
      filter(
        dataframe[, nombre_variable] > (promedio - (delta * desviacion))
        & dataframe[, nombre_variable] < (promedio + (delta * desviacion))
      )
  }
  return(dataframe)
}

# Por valor-Z robusto
por_z_robusto <- function(
  variable,
  delta,
  log_variable = FALSE,
  candidatos = TRUE
) {
  if(log_variable) {
    nombre_variable = paste0(variable, '_log')
    dataframe = datos_paises
    dataframe[nombre_variable] = log(datos_paises[, variable])
    dataframe = dataframe %>% arrange(dataframe[, nombre_variable])
  } else {
    nombre_variable = variable
    dataframe = datos_paises %>% arrange(datos_paises[, nombre_variable])
  }
  
  if(candidatos) {
    dataframe['Error'] = abs(dataframe[, nombre_variable] - median(dataframe[, nombre_variable]))
    dataframe['Valor_Z'] = dataframe$Error / median(dataframe$Error)
    dataframe = dataframe %>% 
      filter(Valor_Z > delta)
  } else {
    dataframe['Error'] = abs(dataframe[, nombre_variable] - median(dataframe[, nombre_variable]))
    dataframe['Valor_Z'] = dataframe$Error / median(dataframe$Error)
    dataframe = dataframe %>% 
      filter(Valor_Z < delta)
  }
  return(dataframe)
}  


######################### Comparación de estadísticos ######################### 
comparacion_estadisticos <- function(
  variable,
  delta_intervalo,
  delta_z,
  log_variable = FALSE,
  log_PIB = FALSE
) {
  if(log_variable) {
    nombre_variable = paste0(variable, '_log')
    dataframe = datos_paises
    dataframe[nombre_variable] = log(datos_paises[, variable])
    dataframe = dataframe %>% arrange(dataframe[, nombre_variable])
    original = paste('Logaritmo de', variable, 'original')
  } else {
    nombre_variable = variable
    dataframe = datos_paises %>% arrange(datos_paises[, nombre_variable])
    original = paste(variable, 'original')
  }
  
  percentiles <- por_percentil(
    variable,
    log_variable,
    FALSE
  )
  intervalo <- por_intervalo(
    variable,
    delta_intervalo,
    log_variable,
    FALSE
  )
  valor_z <- por_z_robusto(
    variable,
    delta_z,
    log_variable,
    FALSE
  )
  
  if(log_PIB){
    correlacion = cor(dataframe[, nombre_variable], log(dataframe[, 'PIB']))
    cor_percentil = cor(percentiles[, nombre_variable], log(percentiles[, 'PIB']))
    cor_intervalo = cor(intervalo[, nombre_variable], log(intervalo[, 'PIB']))
    cor_z = cor(valor_z[, nombre_variable], log(valor_z[, 'PIB']))
    nombre_correlacion = 'Correlación_con_log_PIB'
    numero_grafico = 2
  } else {
    correlacion = cor(dataframe[, nombre_variable], dataframe[, 'PIB'])
    cor_percentil = cor(percentiles[, nombre_variable], percentiles[, 'PIB'])
    cor_intervalo = cor(intervalo[, nombre_variable], intervalo[, 'PIB'])
    cor_z = cor(valor_z[, nombre_variable], valor_z[, 'PIB'])
    nombre_correlacion = 'Correlación_con_PIB'
    numero_grafico = 1
  }
  resumen_df <- data.frame(
    'Grupo' = c(
      original, 
      'Percentiles 1% al 99%', 
      paste('Intervalo de variabilidad delta', delta_intervalo), 
      paste('Valor-Z robusto con delta', delta_z)
    ),
    'Promedio' = c(
      mean(dataframe[, nombre_variable], na.rm = TRUE),
      mean(percentiles[, nombre_variable], na.rm = TRUE),
      mean(intervalo[, nombre_variable], na.rm = TRUE),
      mean(valor_z[, nombre_variable], na.rm = TRUE)
    ),
    'Desviación_estándar' = c(
      sd(dataframe[, nombre_variable], na.rm = TRUE),
      sd(percentiles[, nombre_variable], na.rm = TRUE),
      sd(intervalo[, nombre_variable], na.rm = TRUE),
      sd(valor_z[, nombre_variable], na.rm = TRUE)
    )
  )
    
  resumen_df[nombre_correlacion] = c(
    correlacion,
    cor_percentil,
    cor_intervalo,
    cor_z
  )
  
  dataframe_graf <- graficos_PIB(dataframe, nombre_variable, FALSE, 'Datos originales', 'blue')[[numero_grafico]]
  percentiles_graf <- graficos_PIB(percentiles, nombre_variable, FALSE, 'Percentiles 1% y 99%', 'red')[[numero_grafico]]
  intervalo_graf <- graficos_PIB(intervalo, nombre_variable, FALSE, 'Intervalo de variabilidad', 'darkgreen')[[numero_grafico]]
  valor_z_graf <- graficos_PIB(valor_z, nombre_variable, FALSE, 'Valor-Z robusto', 'orange')[[numero_grafico]]
  
  return(list(
    resumen_df,
    dataframe_graf,
    percentiles_graf,
    intervalo_graf,
    valor_z_graf
  ))
}


############################## EVALUACIONES ##############################

######################### Búsqueda de valores NA #########################
hay_na <- function(dataframe) {
  lista_auxiliar = c()
  for(dato in names(datos_paises)) {
    datos_na = sum(is.na(datos_paises[, dato]))
    if(datos_na > 0) {
      lista_auxiliar <- c(lista_auxiliar, dato)
    }
  }
  return(lista_auxiliar)
}
hay_na(datos_paises)


############### Comparador de correlaciones con y sin logaritmo ###############
comparador_correlaciones <- function(dataframe) {
  variables = c()
  correlaciones = c()
  abs_correlaciones = c()
  correlaciones_PIB_log = c()
  abs_correlaciones_PIB_log = c()
  
  for(variable in names(dataframe)) {
    if(!(variable %in% c('PAIS', 'PIB'))) {
      correlacion = cor(dataframe[, variable], dataframe$PIB)
      correlacion_log = cor(log(dataframe[, variable]), dataframe$PIB)
      correlacion_PIB_log = cor(dataframe[, variable], log(dataframe$PIB))
      correlacion_log_PIB_log = cor(log(dataframe[, variable]), log(dataframe$PIB))
      
      variables = c(variables, variable, paste0(variable, '_log'))
      correlaciones = c(correlaciones, correlacion, correlacion_log)
      abs_correlaciones = c(abs_correlaciones, abs(correlacion), abs(correlacion_log))
      correlaciones_PIB_log = c(correlaciones_PIB_log, correlacion_PIB_log, correlacion_log_PIB_log)
      abs_correlaciones_PIB_log = c(abs_correlaciones_PIB_log, abs(correlacion_PIB_log), abs(correlacion_log_PIB_log))
    }
  }
  
  correlaciones_df <- data.frame(
    variables,
    correlaciones,
    abs_correlaciones,
    correlaciones_PIB_log,
    abs_correlaciones_PIB_log
  ) %>% 
    mutate(mejora_PIB_log = abs_correlaciones < abs_correlaciones_PIB_log)
  return(correlaciones_df)
}
correlaciones_df <- comparador_correlaciones(datos_paises)


############################## Pregunta 1 ##############################
# IDH
histograma_IDH <- histograma('IDH', datos_paises, datos_paises, FALSE, 'Histograma base')
histograma_IDH

candidatos_percentil_IDH <- por_percentil('IDH', FALSE, TRUE)
histograma_IDH_percentil <- histograma('IDH', datos_paises, candidatos_percentil_IDH, FALSE, 'Outliers por percentiles')

candidatos_intervalo_IDH <- por_intervalo('IDH', 2, FALSE, TRUE)
histograma_IDH_intervalo <- histograma('IDH', datos_paises, candidatos_intervalo_IDH, FALSE, 'Outliers por intervalo de variabilidad delta 2')

candidatos_valor_z_IDH <- por_z_robusto('IDH', 3, FALSE, TRUE)
histograma_IDH_valor_z <- histograma('IDH', datos_paises, candidatos_valor_z_IDH, FALSE, 'Outliers por valor-Z robusto delta 3')

# Gráficos con datos candidatos a outliers
histograma_IDH_percentil + histograma_IDH_intervalo + histograma_IDH_valor_z

estadisticos_IDH <- comparacion_estadisticos('IDH', 2, 3, FALSE, TRUE)
resumen_IDH <- estadisticos_IDH[[1]]


# Logaritmo de DIOXIDO
histograma_DIOXIDO <- histograma('DIOXIDO', datos_paises, datos_paises, TRUE, 'Histograma base')
histograma_DIOXIDO

candidatos_percentil_DIOXIDO <- por_percentil('DIOXIDO', TRUE, TRUE)
histograma_DIOXIDO_percentil <- histograma('DIOXIDO', datos_paises, candidatos_percentil_DIOXIDO, TRUE, 'Outliers por percentiles')

candidatos_intervalo_DIOXIDO <- por_intervalo('DIOXIDO', 2, TRUE, TRUE)
histograma_DIOXIDO_intervalo <- histograma('DIOXIDO', datos_paises, candidatos_intervalo_DIOXIDO, TRUE, 'Outliers por intervalo de variabilidad delta 2')

candidatos_valor_z_DIOXIDO <- por_z_robusto('DIOXIDO', 3.5, TRUE, TRUE)
histograma_DIOXIDO_valor_z <- histograma('DIOXIDO', datos_paises, candidatos_valor_z_DIOXIDO, TRUE, 'Outliers por valor-Z robusto delta 3,5')

# Gráficos con datos candidatos a outliers
histograma_DIOXIDO_percentil + histograma_DIOXIDO_intervalo + histograma_DIOXIDO_valor_z

estadisticos_DIOXIDO <- comparacion_estadisticos('DIOXIDO', 2, 3.5, TRUE, TRUE)
resumen_DIOXIDO <- estadisticos_DIOXIDO[[1]]


# INTERNET
histograma_INTERNET <- histograma('INTERNET', datos_paises, datos_paises, FALSE, 'Histograma base')
histograma_INTERNET

candidatos_percentil_INTERNET <- por_percentil('INTERNET', FALSE, TRUE)
histograma_INTERNET_percentil <- histograma('INTERNET', datos_paises, candidatos_percentil_INTERNET, FALSE, 'Outliers por percentiles')

candidatos_intervalo_INTERNET <- por_intervalo('INTERNET', 1.75, FALSE, TRUE)
histograma_INTERNET_intervalo <- histograma('INTERNET', datos_paises, candidatos_intervalo_INTERNET, FALSE, 'Outliers por intervalo de variabilidad delta 1,75')

candidatos_valor_z_INTERNET <- por_z_robusto('INTERNET', 2, FALSE, TRUE)
histograma_INTERNET_valor_z <- histograma('INTERNET', datos_paises, candidatos_valor_z_INTERNET, FALSE, 'Outliers por valor-Z robusto delta 2')

# Gráficos con datos candidatos a outliers
histograma_INTERNET_percentil + histograma_INTERNET_intervalo + histograma_INTERNET_valor_z

estadisticos_INTERNET <- comparacion_estadisticos('INTERNET', 1.75, 2, FALSE, TRUE)
resumen_INTERNET <- estadisticos_INTERNET[[1]]


# Logaritmo de MORTMAT
histograma_MORTMAT <- histograma('MORTMAT', datos_paises, datos_paises, TRUE, 'Histograma base')
histograma_MORTMAT

candidatos_percentil_MORTMAT <- por_percentil('MORTMAT', TRUE, TRUE)
histograma_MORTMAT_percentil <- histograma('MORTMAT', datos_paises, candidatos_percentil_MORTMAT, TRUE, 'Outliers por percentiles')

candidatos_intervalo_MORTMAT <- por_intervalo('MORTMAT', 2, TRUE, TRUE)
histograma_MORTMAT_intervalo <- histograma('MORTMAT', datos_paises, candidatos_intervalo_MORTMAT, TRUE, 'Outliers por intervalo de variabilidad delta 2')

candidatos_valor_z_MORTMAT <- por_z_robusto('MORTMAT', 2, TRUE, TRUE)
histograma_MORTMAT_valor_z <- histograma('MORTMAT', datos_paises, candidatos_valor_z_MORTMAT, TRUE, 'Outliers por valor-Z robusto delta 2')

# Gráficos con datos candidatos a outliers
histograma_MORTMAT_percentil + histograma_MORTMAT_intervalo + histograma_MORTMAT_valor_z

estadisticos_MORTMAT <- comparacion_estadisticos('MORTMAT', 2, 2, TRUE, TRUE)
resumen_MORTMAT <- estadisticos_MORTMAT[[1]]


############################## Pregunta 2 ##############################
cor(log(datos_paises$PIB), datos_paises$IDH)
mean(datos_paises$IDH)
sd(datos_paises$IDH)
median(datos_paises$IDH)
grafico_IDH <- graficos_PIB(datos_paises, 'IDH', FALSE, color_smooth = 'blue')[[2]]
grafico_IDH

cor(log(datos_paises$PIB), log(datos_paises$DIOXIDO))
mean(log(datos_paises$DIOXIDO))
sd(log(datos_paises$DIOXIDO))
median(log(datos_paises$DIOXIDO))
grafico_DIOXIDO_log <- graficos_PIB(datos_paises, 'DIOXIDO', TRUE, color_smooth = 'blue')[[2]]
grafico_DIOXIDO_log

cor(log(datos_paises$PIB), datos_paises$INTERNET)
mean(datos_paises$INTERNET)
sd(datos_paises$INTERNET)
median(datos_paises$INTERNET)
grafico_INTERNET <- graficos_PIB(datos_paises, 'INTERNET', FALSE, color_smooth = 'blue')[[2]]
grafico_INTERNET

cor(log(datos_paises$PIB), log(datos_paises$MORTMAT))
mean(log(datos_paises$MORTMAT))
sd(log(datos_paises$MORTMAT))
median(log(datos_paises$MORTMAT))
grafico_MORTMAT_log <- graficos_PIB(datos_paises, 'MORTMAT', TRUE, color_smooth = 'red')[[2]]
grafico_MORTMAT_log

cor(log(datos_paises$PIB), log(datos_paises$MORTINF))
mean(log(datos_paises$MORTINF))
sd(log(datos_paises$MORTINF))
median(log(datos_paises$MORTINF))
grafico_MORTINF_log <- graficos_PIB(datos_paises, 'MORTINF', TRUE, color_smooth = 'red')[[2]]
grafico_MORTINF_log

cor(log(datos_paises$PIB), log(datos_paises$FAO))
mean(log(datos_paises$FAO))
sd(log(datos_paises$FAO))
median(log(datos_paises$FAO))
grafico_FAO_log <- graficos_PIB(datos_paises, 'FAO', TRUE, color_smooth = 'red')[[2]]
grafico_FAO_log

cor(log(datos_paises$PIB), datos_paises$ESCOLARIDAD)
mean(datos_paises$ESCOLARIDAD)
sd(datos_paises$ESCOLARIDAD)
median(datos_paises$ESCOLARIDAD)
grafico_ESCOLARIDAD <- graficos_PIB(datos_paises, 'ESCOLARIDAD', FALSE, color_smooth = 'blue')[[2]]
grafico_ESCOLARIDAD

cor(log(datos_paises$PIB), datos_paises$ELECTRICIDAD)
mean(datos_paises$ELECTRICIDAD)
sd(datos_paises$ELECTRICIDAD)
median(datos_paises$ELECTRICIDAD)
grafico_ELECTRICIDAD <- graficos_PIB(datos_paises, 'ELECTRICIDAD', FALSE, color_smooth = 'blue')[[2]]
grafico_ELECTRICIDAD

cor(log(datos_paises$PIB), datos_paises$VIDA)
mean(datos_paises$VIDA)
sd(datos_paises$VIDA)
median(datos_paises$VIDA)
grafico_VIDA <- graficos_PIB(datos_paises, 'VIDA', FALSE, color_smooth = 'blue')[[2]]
grafico_VIDA

cor(log(datos_paises$PIB), datos_paises$GENERO)
mean(datos_paises$GENERO)
sd(datos_paises$GENERO)
median(datos_paises$GENERO)
grafico_GENERO <- graficos_PIB(datos_paises, 'GENERO', FALSE, color_smooth = 'red')[[2]]
grafico_GENERO

cor(log(datos_paises$PIB), datos_paises$DESERCION)
mean(datos_paises$DESERCION)
sd(datos_paises$DESERCION)
median(datos_paises$DESERCION)
grafico_DESERCION <- graficos_PIB(datos_paises, 'DESERCION', FALSE, color_smooth = 'red')[[2]]
grafico_DESERCION


# Estructura de gráficos con  signo positivo
(grafico_IDH + grafico_DIOXIDO_log + grafico_INTERNET) / (grafico_ESCOLARIDAD + grafico_ELECTRICIDAD + grafico_VIDA)

# Estructura de gráficos con  signo negativo
(grafico_MORTMAT_log + grafico_MORTINF_log + grafico_FAO_log) / (grafico_GENERO + grafico_DESERCION)


############################## Pregunta 3 ##############################
# Logaritmo del PIB
PIB_log <- log(datos_paises$PIB)

# Variables en escala normal con signo positivo
IDH <- datos_paises$IDH
INTERNET <- datos_paises$INTERNET
ESCOLARIDAD <- datos_paises$ESCOLARIDAD
ELECTRICIDAD <- datos_paises$ELECTRICIDAD
VIDA <- datos_paises$VIDA
CELULAR <- datos_paises$CELULAR
INMIGRANTES <- datos_paises$INMIGRANTES
FOSIL <- datos_paises$FOSIL
TURISMO <- datos_paises$TURISMO#
PARLAMENTO <- datos_paises$PARLAMENTO
BOSQUE <- datos_paises$BOSQUE

# Variables en escala normal con signo negativo
GENERO <- datos_paises$GENERO
DESERCION <- datos_paises$DESERCION
RENOVABLE <- datos_paises$RENOVABLE
VIOLENCIA <- datos_paises$VIOLENCIA#
SUICIDIOFEM <- datos_paises$SUICIDIOFEM
HOMICIDIO <- datos_paises$HOMICIDIO
AFECTADOS <- datos_paises$AFECTADOS
DESASTRE <- datos_paises$DESASTRE

# Variables en escala logarítmica con signo positivo
DIOXIDO_log <- log(datos_paises$DIOXIDO)
PRISION_log <- log(datos_paises$PRISION)

# Variables en escala logarítmica con signo negativo
MORTMAT_log <- log(datos_paises$MORTMAT)
MORTINF_log <- log(datos_paises$MORTINF)
FAO_log <- log(datos_paises$FAO)
GINI_log <- log(datos_paises$GINI)
IPC_log <- log(datos_paises$IPC)
SUICIDIOMAS_log <- log(datos_paises$SUICIDIOMAS)
POB_log <- log(datos_paises$POB)

datos_paises_nuevo <- data.frame(
  PIB_log,
  IDH, INTERNET, ESCOLARIDAD, ELECTRICIDAD, VIDA, CELULAR, INMIGRANTES, FOSIL, TURISMO, PARLAMENTO, BOSQUE,
  GENERO, DESERCION, RENOVABLE, VIOLENCIA, SUICIDIOFEM, HOMICIDIO, AFECTADOS, DESASTRE,
  DIOXIDO_log, PRISION_log,
  MORTMAT_log, MORTINF_log, FAO_log, GINI_log, IPC_log, SUICIDIOMAS_log, POB_log
)
correlacion_datos_nuevos <- cor(datos_paises_nuevo)

# Revisión de las variables que fueron mencionadas en la hipótesis
datos_hipotesis_df <- data.frame(
  IDH, DIOXIDO_log, INTERNET, ESCOLARIDAD, ELECTRICIDAD, VIDA,
  MORTMAT_log, MORTINF_log, FAO_log, GENERO, DESERCION
)

correlaciones_hipotesis <- cor(datos_hipotesis_df)
graficos_hipotesis <- pairs(datos_hipotesis_df)


# Se genera el modelo con todas las variables
modelo_final_todos_1 <- lm(PIB_log ~ 
  IDH + INTERNET + ESCOLARIDAD + ELECTRICIDAD + VIDA + CELULAR + INMIGRANTES + FOSIL + TURISMO + PARLAMENTO + BOSQUE +
  GENERO + DESERCION + RENOVABLE + VIOLENCIA + SUICIDIOFEM + HOMICIDIO + AFECTADOS + DESASTRE +
  DIOXIDO_log + PRISION_log +
  MORTMAT_log + MORTINF_log + FAO_log + GINI_log + IPC_log + SUICIDIOMAS_log + POB_log
)
summary(modelo_final_todos_1)
# R2: 0.953
# R2 ajustado: 0.9447
grafico_todos_1 <- plot(predict(modelo_final_todos_1), PIB_log)
cor(predict(modelo_final_todos_1), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.1
modelo_final_todos_2 <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA + CELULAR + INMIGRANTES +
  DESERCION +
  DIOXIDO_log +
  FAO_log + SUICIDIOMAS_log
)
summary(modelo_final_todos_2)
# R2: 0.9504
# R2 ajustado: 0.9479
grafico_todos_2 <- plot(predict(modelo_final_todos_2), PIB_log)
grafico_todos_2_nolog <-  plot(exp(predict(modelo_final_todos_2)), datos_paises$PIB)


############################## Modelo escogido ##############################
# Se sacan todas las variables con una probabilidad(t-test) > 0.05
modelo_final_todos_2b <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA + CELULAR + INMIGRANTES +
  DESERCION +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_todos_2b)
# R2: 0.9492
# R2 ajustado: 0.9469
valores_predichos <- predict(modelo_final_todos_2b)
valores_predichos_exp <- exp(predict(modelo_final_todos_2b))
dataframe_comparacion_resultados <- data.frame(
  valores_predichos = valores_predichos,
  valores_predichos_exp = valores_predichos_exp,
  PIB_log = PIB_log,
  PIB = datos_paises$PIB
)

cor(valores_predichos_exp, datos_paises$PIB)
cor(valores_predichos, PIB_log)

grafico_todos_2b <- dataframe_comparacion_resultados %>% 
  ggplot(
    aes(x = valores_predichos, y = PIB_log)
  ) +
  geom_smooth(
    method = 'lm',
    colour = 'cyan4',
    se = FALSE
  ) +
  geom_point(
    colour = 'red',
    alpha = 0.4
  ) +
  theme_bw() +
  labs(
    x = 'Valores predichos del modelo',
    y = 'Logaritmo de los valores observados del PIB',
    title = 'Logaritmo del PIB vs predicción del modelo',
    caption = 'Correlación = 0,9742693'
  ) +
  theme(
    title = element_text(size = 16),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 16)
  )

grafico_todos_2b_nolog <- dataframe_comparacion_resultados %>% 
  ggplot(
    aes(x = valores_predichos_exp, y = PIB)
  ) +
  geom_smooth(
    method = 'lm',
    colour = 'green4',
    se = FALSE
  ) +
  geom_point(
    colour = 'hotpink',
    alpha = 0.4
  ) +
  theme_bw() +
  labs(
    x = 'Exponencial de los valores predichos del modelo',
    y = 'Valores observados del PIB',
    title = 'PIB vs la exponencial de la predicción del modelo',
    caption = 'Correlación = 0,9455411'
  ) +
  theme(
    title = element_text(size = 16),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 16)
  )

# Visualización de los gráficos del modelo
grafico_todos_2b + grafico_todos_2b_nolog
############################## Modelo escogido ##############################


# Se sacan todas las variables con una probabilidad(t-test) > 0.05 y se sacan los logaritmos
modelo_final_todos_2b2 <- lm(log(PIB) ~ 
  IDH + ESCOLARIDAD + VIDA + CELULAR + INMIGRANTES +
  DESERCION +
  DIOXIDO +
  FAO,
  data = datos_paises
)
summary(modelo_final_todos_2b2)
# R2: 0.9445
# R2 ajustado: 0.9420
grafico_todos_2b2 <- plot(predict(modelo_final_todos_2b2), PIB_log)

# Se sacan todas las variables con un signo del parámetro calculado contrario a lo esperado
modelo_final_todos_2c <- lm(PIB_log ~ 
  IDH + CELULAR + INMIGRANTES +
  DIOXIDO_log +
  FAO_log + SUICIDIOMAS_log
)
summary(modelo_final_todos_2c)
# R2: 0.9356
# R2 ajustado: 0.9335
grafico_todos_2c <- plot(predict(modelo_final_todos_2c), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.1
modelo_final_todos_2c2 <- lm(PIB_log ~ 
  IDH + CELULAR + INMIGRANTES +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_todos_2c2)
# R2: 0.9355
# R2 ajustado: 0.9337
grafico_todos_2c2 <- plot(predict(modelo_final_todos_2c2), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.01
modelo_final_todos_3 <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA + CELULAR +
  DESERCION +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_todos_3)
# R2: 0.9467
# R2 ajustado: 0.9446
grafico_todos_3 <- plot(predict(modelo_final_todos_3), PIB_log)
grafico_todos_3_nolog <-  plot(exp(predict(modelo_final_todos_3)), datos_paises$PIB)

# Se sacan todas las variables con una probabilidad(t-test) > 0.001
modelo_final_todos_4 <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA + CELULAR +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_todos_4)
# R2: 0.9441
# R2 ajustado: 0.9422
grafico_todos_4 <- plot(predict(modelo_final_todos_4), PIB_log)

# Se sacan todas las variables con un signo del parámetro calculado contrario a lo esperado
modelo_final_todos_5 <- lm(PIB_log ~ 
  IDH + CELULAR +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_todos_5)
# R2: 0.9301
# R2 ajustado: 0.9286
grafico_todos_5 <- plot(predict(modelo_final_todos_5), PIB_log)

# Destilado final
modelo_final_todos_6 <- lm(PIB_log ~ 
  IDH +
  DIOXIDO_log
)
summary(modelo_final_todos_6)
# R2: 0.9182
# R2 ajustado: 0.9173
grafico_todos_6 <- plot(predict(modelo_final_todos_6), PIB_log)


# Se genera el modelo con las variables mencionadas en la hipótesis
modelo_final_hipotesis_1 <- lm(PIB_log ~ 
  IDH + INTERNET + ESCOLARIDAD + ELECTRICIDAD + VIDA +
  GENERO + DESERCION +
  DIOXIDO_log +
  MORTMAT_log + MORTINF_log + FAO_log
)
summary(modelo_final_hipotesis_1)
# R2: 0.9427
# R2 ajustado: 0.9392
grafico_hipotesis_1 <- plot(predict(modelo_final_hipotesis_1), PIB_log)
cor(predict(modelo_final_hipotesis_1), PIB_log)

# Se genera el modelo con las variables mencionadas en la hipótesis menos ELECTRICIDAD y DESERCION
modelo_final_hipotesis_1b <- lm(PIB_log ~ 
  IDH + INTERNET + ESCOLARIDAD + VIDA +
  GENERO +
  DIOXIDO_log +
  MORTMAT_log + MORTINF_log + FAO_log
)
summary(modelo_final_hipotesis_1b)
# R2: 0.9405
# R2 ajustado: 0.9375
grafico_hipotesis_1b <- plot(predict(modelo_final_hipotesis_1b), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.05
modelo_final_hipotesis_2 <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA +
  DESERCION +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_hipotesis_2)
# R2: 0.9417
# R2 ajustado: 0.9398
grafico_hipotesis_2 <- plot(predict(modelo_final_hipotesis_2), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.05 + DESERCION
modelo_final_hipotesis_2b <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_hipotesis_2b)
# R2: 0.9395
# R2 ajustado: 0.9378
grafico_hipotesis_2b <- plot(predict(modelo_final_hipotesis_2b), PIB_log)

# Se sacan todas las variables con un signo del parámetro calculado contrario a lo esperado
modelo_final_hipotesis_2c <- lm(PIB_log ~ 
  IDH +
  DIOXIDO_log +
  FAO_log
)
summary(modelo_final_hipotesis_2c)
# R2: 0.9250
# R2 ajustado: 0.9238
grafico_hipotesis_2c <- plot(predict(modelo_final_hipotesis_2c), PIB_log)

# Se sacan todas las variables con una probabilidad(t-test) > 0.001
modelo_final_hipotesis_3 <- lm(PIB_log ~ 
  IDH + ESCOLARIDAD + VIDA +
  DIOXIDO_log
)
summary(modelo_final_hipotesis_3)
# R2: 0.9355
# R2 ajustado: 0.9341
grafico_hipotesis_3 <- plot(predict(modelo_final_hipotesis_3), PIB_log)


# Se genera el modelo con todas las variables pero con PIB sin logaritmo
modelo_final_todos_sin_log_1 <- lm(PIB ~ 
  IDH + INTERNET + ESCOLARIDAD + ELECTRICIDAD + VIDA + CELULAR + INMIGRANTES + FOSIL + TURISMO + PARLAMENTO + BOSQUE +
  GENERO + DESERCION + RENOVABLE + VIOLENCIA + SUICIDIOFEM + HOMICIDIO + AFECTADOS + DESASTRE +
  DIOXIDO + PRISION +
  MORTMAT + MORTINF + FAO + GINI + IPC + SUICIDIOMAS + POB,
  data = datos_paises
)
summary(modelo_final_todos_sin_log_1)
# R2: 0.8880
# R2 ajustado: 0.8683
grafico_todos_sin_log_1 <- plot(predict(modelo_final_todos_sin_log_1), datos_paises$PIB)
cor(predict(modelo_final_todos_sin_log_1), datos_paises$PIB)

# Se sacan todas las variables con una probabilidad(t-test) > 0.1
modelo_final_todos_sin_log_2 <- lm(PIB ~ 
  IDH + ESCOLARIDAD + ELECTRICIDAD + VIDA + INMIGRANTES +
  RENOVABLE +
  DIOXIDO + PRISION +
  MORTINF + FAO,
  data = datos_paises
)
summary(modelo_final_todos_sin_log_2)
# R2: 0.8806 
# R2 ajustado: 0.8738
grafico_todos_sin_log_2 <- plot(predict(modelo_final_todos_sin_log_2), datos_paises$PIB)
cor(predict(modelo_final_todos_sin_log_2), datos_paises$PIB)

# Se sacan todas las variables con una probabilidad(t-test) > 0.05
modelo_final_todos_sin_log_3 <- lm(PIB ~ 
  IDH + ELECTRICIDAD + VIDA + INMIGRANTES +
  RENOVABLE +
  DIOXIDO + PRISION +
  MORTINF + FAO,
  data = datos_paises
)
summary(modelo_final_todos_sin_log_3)
# R2: 0.8785 
# R2 ajustado: 0.8724
grafico_todos_sin_log_3 <- plot(predict(modelo_final_todos_sin_log_3), datos_paises$PIB)
cor(predict(modelo_final_todos_sin_log_3), datos_paises$PIB)

# Se sacan todas las variables con una probabilidad(t-test) > 0.01
modelo_final_todos_sin_log_4 <- lm(PIB ~ 
  IDH + VIDA + INMIGRANTES +
  DIOXIDO +
  MORTINF + FAO,
  data = datos_paises
)
summary(modelo_final_todos_sin_log_4)
# R2: 0.8621
# R2 ajustado: 0.8575
grafico_todos_sin_log_4 <- plot(predict(modelo_final_todos_sin_log_4), datos_paises$PIB)
