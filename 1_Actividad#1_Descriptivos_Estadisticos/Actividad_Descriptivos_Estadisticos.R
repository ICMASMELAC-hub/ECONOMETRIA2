##########################################################
#                                                        #
####              UNIVERSIDAD DEL QUINDIO             ####
#                       ECONONOMIA 			                 #
#                                                        #
#                     ECONOMETRIA  II                    #
####                                                  ####
##############                              ##############
             #         ACTIVIDAD 1          #
##############		                          ##############
#							                                           #
##############  DESCRIPTIVOS_ESTADISTICOS   ##############


# By: LIZED CRISTINA BLANDON PALADINES - lcblandonp@uqvirtual.edu.co
# By: IVAN CRISTOBAL MASMELA CASTRO    - icmasmelac@uqvirtual.edu.co
# By: JUAN JACOBO OBANDO FRANCO        - juanj.obandof@uqvirtual.edu.co

getwd()
options( 'scipen' = 100 , 'digits' = 4 )
rm(list = ls())

# Librerias -----
library("skimr")
library("readxl")
library("stringr")
library("stringi")
library("haven")
library("tidyverse")
library("plyr")
library("rstatix")
library("e1071")
library("tidyr")
library("moments")

## Cargamos los datos ------
DATA_INGRESOS = read.csv(file ="C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/DATA_INGRESOS/DATA_INGRESOS.csv" , sep = ",")

## Descriptivos -----
head(DATA_INGRESOS)
str(DATA_INGRESOS)
skimr::skim(DATA_INGRESOS)

# Tabla de frecuencias ------
table(DATA_INGRESOS$FFT, useNA = "always")          # Frecuencia absoluta de FFT (incluye NA)
prop.table(table(na.omit(DATA_INGRESOS$FFT)))       # Frecuencia relativa de FFT (sin NA, proporciones)

summary(DATA_INGRESOS$TTF)

## Indicadores de posición y centro ----
DATA_INGRESOS |>
  dplyr::group_by(1) |>
  mutate(FFT = replace_na(FFT, 0)) |>
  dplyr::summarise(
    mean_FFT   = mean(as.numeric(FFT)),       # proporción fuera de la fuerza laboral  
    median_FFT = median(as.numeric(FFT)),     # valor central (0 o 1)
    min_FFT    = min(as.numeric(FFT)),        # mínimo
    max_FFT    = max(as.numeric(FFT)),        # máximo
    sum_FFT    = sum(as.numeric(FFT))         # Total fuera de la fuerza laboral
  )              

names(DATA_INGRESOS)

## Indicadores de dispersión ----
DATA_INGRESOS |>
  dplyr::group_by(1) |>
  mutate(FFT = replace_na(FFT, 0)) |>        
  dplyr::summarise(
    rango_FFT     = max(FFT) - min(FFT),       # siempre será 1     
    sd_FFT        = sd(as.numeric(FFT)),       # desviación estandar
    varianza_FFT  = var(as.numeric(FFT)),      # varianza
    curtosis_FFT  = kurtosis(as.numeric(FFT)), # curtosis
    asimetria_FFT = skewness(as.numeric(FFT))  # asimetría
  )

