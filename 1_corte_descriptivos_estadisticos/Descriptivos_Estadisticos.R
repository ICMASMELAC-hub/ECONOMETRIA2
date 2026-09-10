###############################################################################
#########                                                             #########
#                        UNIVERSIDAD DEL QUINDIO                              #
#                         PROGRAMA DE ECONOMIA                                #
#                             ECONOMETRIA                                     #
###############################################################################

# By: IVAN CRISTOBAL MASMELA CASTRO
# icmasmelac@uqvirtual.edu.co
# +57 3108206970

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

## Cargamos los datos ------
DATA_INGRESOS = read.csv(file ="C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/DATA_INGRESOS/DATA_INGRESOS.csv" , sep = ",")

## Descriptivos -----
head(DATA_INGRESOS)
str(DATA_INGRESOS)
skimr::skim(DATA_INGRESOS)

# Tabla de frecuencias
table(DATA_INGRESOS$OCI , useNA = "always")
table(DATA_INGRESOS$AREA , useNA = "always")

summary(DATA_INGRESOS$INGLABO)

# Analisis descriptivos

## Indicadores de posicion y centro ----
DATA_INGRESOS |> dplyr::group_by(1) |> mutate(
  INGLABO = replace_na( INGLABO , replace = 0)
) |>  dplyr::summarise(
  median_INGLABO = median(INGLABO) ,
  mean_INGLABO = mean(INGLABO),
  rango_medio_INGLABO = ( (max(INGLABO) - min(INGLABO) )/2 ) , 
  min_INGLABO = min(INGLABO) ,
  Q1_INGLABO = quantile(INGLABO , c(0.25) ) ,
  Q3_INGLABO = quantile(INGLABO , c(0.75) ) ,
  rando_Q_INGLABO = IQR(INGLABO) ,
  max_INGLABO = max(INGLABO)
)

## Indicadores de dispersión
DATA_INGRESOS |> dplyr::group_by(1) |> mutate(
  INGLABO = replace_na( INGLABO , replace = 0)
) |>  dplyr::summarise(
  rango_INGLABO = (max(INGLABO) - min(INGLABO)) ,
  sd_INGLABO = sd(INGLABO) ,
  varianza_INGLABO = var(INGLABO) ,
  c_varianza_INGLABO = ( (sd(INGLABO)/mean(INGLABO) )*100 ),
  c_curtosis_INGLABO = kurtosis(INGLABO) ,
  c_asimetria_INGLABO = skewness(INGLABO)
)




