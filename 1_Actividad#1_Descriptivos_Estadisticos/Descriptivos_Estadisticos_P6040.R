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

# Punto 1: Seleccione una variable de interes y extraiga los diferentes
# tipos de estadisticas descriptivas, y explíquelas.

# Variable seleccionada: P6040 (Edad de la persona)

getwd()
options( 'scipen' = 100 , 'digits' = 7 )
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
DATA_INGRESOS = read.csv(file = "C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/DATA_INGRESOS/DATA_INGRESOS.csv", sep = ",")

## Descriptivos generales -----
head(DATA_INGRESOS)
str(DATA_INGRESOS)
skimr::skim(DATA_INGRESOS$P6040)

# Tabla de frecuencias (referencia rapida de la distribucion por edades)
table(DATA_INGRESOS$P6040 , useNA = "always")

summary(DATA_INGRESOS$P6040)


# Analisis descriptivos de P6040 (Edad)

## Indicadores de posicion y centro ----
as.data.frame(DATA_INGRESOS |> dplyr::group_by(1) |> dplyr::summarise(
  n_P6040             = sum(!is.na(P6040)) ,
  media_P6040         = mean(P6040 , na.rm = TRUE) ,
  mediana_P6040       = median(P6040 , na.rm = TRUE) ,
  rango_medio_P6040   = ( (max(P6040 , na.rm = TRUE) - min(P6040 , na.rm = TRUE)) / 2 ) ,
  min_P6040           = min(P6040 , na.rm = TRUE) ,
  Q1_P6040            = quantile(P6040 , 0.25 , na.rm = TRUE) ,
  Q3_P6040            = quantile(P6040 , 0.75 , na.rm = TRUE) ,
  rango_Q_P6040       = IQR(P6040 , na.rm = TRUE) ,
  max_P6040           = max(P6040 , na.rm = TRUE)
))

## Indicadores de dispersion y forma ----
DATA_INGRESOS |> dplyr::group_by(1) |> dplyr::summarise(
  rango_P6040         = (max(P6040 , na.rm = TRUE) - min(P6040 , na.rm = TRUE)) ,
  sd_P6040            = sd(P6040 , na.rm = TRUE) ,
  varianza_P6040      = var(P6040 , na.rm = TRUE) ,
  c_varianza_P6040    = ( (sd(P6040 , na.rm = TRUE) / mean(P6040 , na.rm = TRUE)) * 100 ) ,
  c_curtosis_P6040    = kurtosis(P6040 , na.rm = TRUE) ,
  c_asimetria_P6040   = skewness(P6040 , na.rm = TRUE)
)


############################
# Estadisticos ADICIONALES #
############################

## Moda ----
# R no trae una funcion nativa para la moda, hay que crearla
moda = function(x) {
  x = x[!is.na(x)]
  tabla_freq = table(x)
  as.numeric(names(tabla_freq)[tabla_freq == max(tabla_freq)])
}
moda_P6040 = moda(DATA_INGRESOS$P6040)
moda_P6040

# La moda es el valor que mas se repite. En P6040 la edad mas frecuente es 24
# anios, lo que tiene sentido dado que la GEIH tiene mucha poblacion en edad
# economicamente activa.

## Media geometrica y media armonica ----
# Ambas son alternativas a la media aritmetica, mas apropiadas cuando los
# datos representan tasas o crecimientos, o cuando hay valores muy dispersos.
# No se pueden calcular con valores de 0, asi que se excluyen esos casos.
P6040_sin_ceros = DATA_INGRESOS$P6040[DATA_INGRESOS$P6040 > 0 & !is.na(DATA_INGRESOS$P6040)]

media_geometrica_P6040 = exp(mean(log(P6040_sin_ceros)))
media_geometrica_P6040

media_armonica_P6040 = length(P6040_sin_ceros) / sum(1 / P6040_sin_ceros)
media_armonica_P6040

# Ambas medias (~32.6 y ~20.0) son menores que la media aritmetica (~40.7),
# lo cual es normal: media aritmetica >= media geometrica >= media armonica.
# Esto confirma que hay observaciones de valores bajos (ninos) que "pesan"
# mas fuerte en estas medias alternativas.

## MAD - Desviacion absoluta mediana ----
# Es una medida de dispersion mas robusta que la desviacion estandar, ya que
# no se ve tan afectada por valores extremos.
mad_P6040 = mad(DATA_INGRESOS$P6040 , na.rm = TRUE)
mad_P6040

# El MAD (~26.7, escalado para ser comparable con sd) es cercano a la
# desviacion estandar (~22.4), lo que sugiere que no hay una fuerte
# influencia de valores atipicos en la dispersion de la edad.

## Percentiles y deciles adicionales ----
quantile(DATA_INGRESOS$P6040 , probs = seq(0.1, 0.9, by = 0.1) , na.rm = TRUE)

# Los deciles permiten ver la distribucion con mas detalle que los cuartiles.
# Por ejemplo, el decil 1 (P10 = 11) indica que el 10% de la poblacion tiene
# 11 anios o menos, mientras que el decil 9 (P90 = 71) indica que el 90% de
# la poblacion tiene 71 anios o menos.

## Coeficiente de asimetria de Pearson ----
# Formula alternativa y mas intuitiva que el skewness() de e1071:
# CAP = (media - mediana) / desviacion estandar
CAP_P6040 = (mean(DATA_INGRESOS$P6040 , na.rm = TRUE) - median(DATA_INGRESOS$P6040 , na.rm = TRUE)) /
  sd(DATA_INGRESOS$P6040 , na.rm = TRUE)
CAP_P6040

# Un valor cercano a 0 (~0.03) confirma nuevamente que la distribucion de la
# edad es practicamente simetrica.

## Deteccion de valores atipicos (regla del boxplot) ----
# Un valor se considera atipico si esta por fuera de:
# [Q1 - 1.5*IQR ; Q3 + 1.5*IQR]
Q1_P6040 = quantile(DATA_INGRESOS$P6040 , 0.25 , na.rm = TRUE)
Q3_P6040 = quantile(DATA_INGRESOS$P6040 , 0.75 , na.rm = TRUE)
IQR_P6040 = IQR(DATA_INGRESOS$P6040 , na.rm = TRUE)

limite_inferior_P6040 = Q1_P6040 - 1.5 * IQR_P6040
limite_superior_P6040 = Q3_P6040 + 1.5 * IQR_P6040
limite_inferior_P6040
limite_superior_P6040

outliers_P6040 = DATA_INGRESOS$P6040[DATA_INGRESOS$P6040 < limite_inferior_P6040 |
                                    DATA_INGRESOS$P6040 > limite_superior_P6040]
length(outliers_P6040)

# El limite inferior da negativo (-31) y el superior 113, y como la edad esta
# entre 0 y 106, NO se detectan valores atipicos bajo esta regla. Esto
# refuerza que la variable P6040 tiene un comportamiento "bien portado", sin
# datos extremos que distorsionen los estadisticos de centro y dispersion.

## Boxplot (resumen visual de varios estadisticos) ----
boxplot(DATA_INGRESOS$P6040 ,
        main = "Boxplot de la Edad (P6040)" ,
        ylab = "Edad" ,
        col  = "lightblue")

# Histograma ----
hist(DATA_INGRESOS$P6040,
     main   = "Histograma de la Edad (P6040)" ,
     xlab   = "Edad" ,
     ylab   = "Frecuencia" ,
     col    = "lightblue" ,
     border = "white" ,
     breaks = seq(0, 110, by = 10))

## Histograma con curva de densidad normal superpuesta ----
# Este grafico compara la distribucion real de los datos (barras) contra la
# forma que tendria una distribucion normal con la misma media y desviacion
# estandar (linea roja). Sirve para evaluar visualmente que tan cerca o lejos
# esta la variable de comportarse como una normal.
hist(DATA_INGRESOS$P6040,
     main   = "Histograma de la Edad (P6040) con curva normal" ,
     xlab   = "Edad" ,
     ylab   = "Densidad" ,
     col    = "lightblue" ,
     border = "white" ,
     breaks = seq(0, 110, by = 5) ,
     freq   = FALSE ,
     ylim   = c(0, 0.02))

curve(dnorm(x,
            mean = mean(DATA_INGRESOS$P6040, na.rm = TRUE) ,
            sd   = sd(DATA_INGRESOS$P6040 , na.rm = TRUE)) ,
      col = "purple" ,
      lwd = 3 ,
      add = TRUE)

# Interpretacion:

# - Si las barras siguen de cerca la linea morada, la variable se comporta de
#   forma similar a una normal.

# - En el caso de la Edad, se espera ver un histograma bastante simetrico y
#   parecido a la campana de Gauss, aunque un poco mas "aplanado" (curtosis
#   negativa, ~ -0.93), es decir, con menos concentracion alrededor de la
#   media de la que tendria una normal perfecta, y colas un poco mas cortas.

par(mfrow = c(2, 2)) # divide la ventana grafica en 2 filas x 2 columnas

# 1. Boxplot
boxplot(DATA_INGRESOS$P6040 ,
        main = "Boxplot de la Edad (P6040)" ,
        ylab = "Edad" ,
        col  = "steelblue")

# 2. Histograma simple
hist(DATA_INGRESOS$P6040,
     main   = "Histograma de la Edad (P6040)" ,
     xlab   = "Edad" ,
     ylab   = "Frecuencia" ,
     col    = "lightgreen" ,
     border = "white" ,
     breaks = seq(0, 110, by = 10))

# 3. Histograma con curva normal
hist(DATA_INGRESOS$P6040,
     main   = "Histograma de la Edad (P6040) con curva normal" ,
     xlab   = "Edad" ,
     ylab   = "Densidad" ,
     col    = "khaki" ,
     border = "white" ,
     breaks = seq(0, 110, by = 5) ,
     freq   = FALSE ,
     ylim   = c(0, 0.02) ,
     cex.main = 0.9)          

curve(dnorm(x,
            mean = mean(DATA_INGRESOS$P6040, na.rm = TRUE) ,
            sd   = sd(DATA_INGRESOS$P6040 , na.rm = TRUE)) ,
      col = "purple" ,
      lwd = 3 ,
      add = TRUE)

# 4. Grafico de densidad (Extra)
plot(density(DATA_INGRESOS$P6040, na.rm = TRUE),
     main = "Densidad de la Edad (P6040)" ,
     xlab = "Edad" ,
     ylab = "Densidad" ,
     col  = "darkblue" ,
     lwd  = 2)

par(mfrow = c(1, 1)) # vuelve a la configuracion normal (1 sola grafica por vez)

## Verificacion del "tope" de 99 anios segun ficha tecnica del DANE ----

# Cuantas personas tienen edad reportada por encima de 99
sum(DATA_INGRESOS$P6040 > 99, na.rm = TRUE)

# Cuales son esos valores
table(DATA_INGRESOS$P6040[DATA_INGRESOS$P6040 > 99])

## Crear una version "topcodeada" de la variable (ajustada a 99, segun DANE) ----
DATA_INGRESOS$P6040_ajustada = ifelse(DATA_INGRESOS$P6040 > 99, 99, DATA_INGRESOS$P6040)

# Comparar estadisticos: original vs ajustada
summary(DATA_INGRESOS$P6040)
summary(DATA_INGRESOS$P6040_ajustada)

sd(DATA_INGRESOS$P6040 , na.rm = TRUE)
sd(DATA_INGRESOS$P6040_ajustada , na.rm = TRUE)