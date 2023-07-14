# Evaluación 1.

########## Configuración de área de trabajo e importación de datos ##########
library(dplyr)
library(ggplot2)

setwd(paste0(
  '/home/nestorprr/Documentos/Diplomado_Big_Data_Data_Science/Minería de datos',
  '/Evaluaciones/Evaluación 1'
))
datos_paises <- read.csv('DatosPaises.csv', sep = ';')
summary(datos_paises)
head(datos_paises, n = 10)


############################## FUNCIONES ##############################

######## Gráficos de variables contra PIB normal y en forma logarítmica ########
graficos <- function(datos, variable, log_variable = FALSE) {
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
    geom_smooth(method = 'lm') +
    labs(
      x = texto_x, y = 'PIB'
    )
  
  grafico_log = datos %>% 
    ggplot(
      aes(x = valor_x, y = log(PIB))
    ) +
    geom_point() +
    geom_smooth(method = 'lm') +
    labs(
      x = texto_x, y = 'Logaritmo de PIB'
    )
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
  } else {
    correlacion = cor(dataframe[, nombre_variable], dataframe[, 'PIB'])
    cor_percentil = cor(percentiles[, nombre_variable], percentiles[, 'PIB'])
    cor_intervalo = cor(intervalo[, nombre_variable], intervalo[, 'PIB'])
    cor_z = cor(valor_z[, nombre_variable], valor_z[, 'PIB'])
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
    ),
    'Correlación_con_PIB' = c(
      correlacion,
      cor_percentil,
      cor_intervalo,
      cor_z
    )
  )
  
  if(log_PIB){
    numero_grafico = 2
  } else {
    numero_grafico = 1
  }
  dataframe_graf <- graficos(dataframe, nombre_variable, FALSE)[numero_grafico]
  percentiles_graf <- graficos(percentiles, nombre_variable, FALSE)[numero_grafico]
  intervalo_graf <- graficos(intervalo, nombre_variable, FALSE)[numero_grafico]
  valor_z_graf <- graficos(valor_z, nombre_variable, FALSE)[numero_grafico]
  
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
histograma_IDH_percentil

candidatos_intervalo_IDH <- por_intervalo('IDH', 2, FALSE, TRUE)
histograma_IDH_intervalo <- histograma('IDH', datos_paises, candidatos_intervalo_IDH, FALSE, 'Outliers por intervalo de variabilidad delta 2')
histograma_IDH_intervalo

candidatos_valor_z_IDH <- por_z_robusto('IDH', 3, FALSE, TRUE)
histograma_IDH_valor_z <- histograma('IDH', datos_paises, candidatos_valor_z_IDH, FALSE, 'Outliers por valor-Z robusto delta 3')
histograma_IDH_valor_z

estadisticos_IDH <- comparacion_estadisticos('IDH', 2, 3, FALSE, TRUE)
resumen_IDH <- estadisticos_IDH[[1]]
resumen_IDH


# Logaritmo de DIOXIDO
histograma_DIOXIDO <- histograma('DIOXIDO', datos_paises, datos_paises, TRUE, 'Histograma base')
histograma_DIOXIDO

candidatos_percentil_DIOXIDO <- por_percentil('DIOXIDO', TRUE, TRUE)
histograma_DIOXIDO_percentil <- histograma('DIOXIDO', datos_paises, candidatos_percentil_DIOXIDO, TRUE, 'Outliers por percentiles')
histograma_DIOXIDO_percentil

candidatos_intervalo_DIOXIDO <- por_intervalo('DIOXIDO', 2, TRUE, TRUE)
histograma_DIOXIDO_intervalo <- histograma('DIOXIDO', datos_paises, candidatos_intervalo_DIOXIDO, TRUE, 'Outliers por intervalo de variabilidad delta 2')
histograma_DIOXIDO_intervalo

candidatos_valor_z_DIOXIDO <- por_z_robusto('DIOXIDO', 3.5, TRUE, TRUE)
histograma_DIOXIDO_valor_z <- histograma('DIOXIDO', datos_paises, candidatos_valor_z_DIOXIDO, TRUE, 'Outliers por valor-Z robusto delta 3,5')
histograma_DIOXIDO_valor_z

estadisticos_DIOXIDO <- comparacion_estadisticos('DIOXIDO', 2, 3.5, TRUE, TRUE)
resumen_DIOXIDO <- estadisticos_DIOXIDO[[1]]
resumen_DIOXIDO


# INTERNET
histograma_INTERNET <- histograma('INTERNET', datos_paises, datos_paises, FALSE, 'Histograma base')
histograma_INTERNET

candidatos_percentil_INTERNET <- por_percentil('INTERNET', FALSE, TRUE)
histograma_INTERNET_percentil <- histograma('INTERNET', datos_paises, candidatos_percentil_INTERNET, FALSE, 'Outliers por percentiles')
histograma_INTERNET_percentil

candidatos_intervalo_INTERNET <- por_intervalo('INTERNET', 1.75, FALSE, TRUE)
histograma_INTERNET_intervalo <- histograma('INTERNET', datos_paises, candidatos_intervalo_INTERNET, FALSE, 'Outliers por intervalo de variabilidad delta 1,75')
histograma_INTERNET_intervalo

candidatos_valor_z_INTERNET <- por_z_robusto('INTERNET', 2, FALSE, TRUE)
histograma_INTERNET_valor_z <- histograma('INTERNET', datos_paises, candidatos_valor_z_INTERNET, FALSE, 'Outliers por valor-Z robusto delta 2')
histograma_INTERNET_valor_z

estadisticos_INTERNET <- comparacion_estadisticos('INTERNET', 1.75, 2, FALSE, TRUE)
resumen_INTERNET <- estadisticos_INTERNET[[1]]
resumen_INTERNET


# Logaritmo de MORTMAT
histograma_MORTMAT <- histograma('MORTMAT', datos_paises, datos_paises, TRUE, 'Histograma base')
histograma_MORTMAT

candidatos_percentil_MORTMAT <- por_percentil('MORTMAT', TRUE, TRUE)
histograma_MORTMAT_percentil <- histograma('MORTMAT', datos_paises, candidatos_percentil_MORTMAT, TRUE, 'Outliers por percentiles')
histograma_MORTMAT_percentil

candidatos_intervalo_MORTMAT <- por_intervalo('MORTMAT', 2, TRUE, TRUE)
histograma_MORTMAT_intervalo <- histograma('MORTMAT', datos_paises, candidatos_intervalo_MORTMAT, TRUE, 'Outliers por intervalo de variabilidad delta 2')
histograma_MORTMAT_intervalo

candidatos_valor_z_MORTMAT <- por_z_robusto('MORTMAT', 2, TRUE, TRUE)
histograma_MORTMAT_valor_z <- histograma('MORTMAT', datos_paises, candidatos_valor_z_MORTMAT, TRUE, 'Outliers por valor-Z robusto delta 2')
histograma_MORTMAT_valor_z

estadisticos_MORTMAT <- comparacion_estadisticos('MORTMAT', 2, 2, TRUE, TRUE)
resumen_MORTMAT <- estadisticos_MORTMAT[[1]]
resumen_MORTMAT
