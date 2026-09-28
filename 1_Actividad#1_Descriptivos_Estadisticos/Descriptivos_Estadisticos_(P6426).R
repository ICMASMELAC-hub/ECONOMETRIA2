##########################################################
#                                                        #
#                UNIVERSIDAD DEL QUINDIO                 #
#                        ECONOMIA                        #
#                                                        #
#                     ECONOMETRIA II                     #
##########################################################
#                      ACTIVIDAD 1                       #
##########################################################
#               DESCRIPTIVOS_ESTADISTICOS                #
##########################################################

# By: LIZED CRISTINA BLANDON PALADINES - lcblandonp@uqvirtual.edu.co
# By: IVAN CRISTOBAL MASMELA CASTRO    - icmasmelac@uqvirtual.edu.co
# By: JUAN JACOBO OBANDO FRANCO        - juanj.obandof@uqvirtual.edu.co

# Punto 1: Seleccione una variable de interes y extraiga los diferentes
# tipos de estadisticas descriptivas, y explíquelas.
#
# Variable seleccionada: P6426 (Tiempo -en meses- que la persona lleva
# trabajando en su empleo/negocio actual. Se usa como proxy de "antiguedad
# laboral", NO como experiencia laboral total de la persona).

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
skimr::skim(DATA_INGRESOS$P6426)
summary(DATA_INGRESOS$P6426)

## Tablas de frecuencia de diagnostico ----
table(DATA_INGRESOS$OCI  , useNA = "always")
table(DATA_INGRESOS$AREA , useNA = "always")

## Filtro explicito por ocupados (OCI == 1) ----
#  Se crea un subconjunto de datos solo con las personas ocupadas.
DATA_OCUPADOS = DATA_INGRESOS |> dplyr::filter(OCI == 1)

# Verificacion: el numero de filas de DATA_OCUPADOS debe coincidir con la
# cantidad de datos validos (no NA) que ya tenia P6426 en la base completa.
nrow(DATA_OCUPADOS)
sum(!is.na(DATA_INGRESOS$P6426))

# Ambos dan 38154: esto confirma que P6426 ya solo se reporta para personas
# ocupadas, y que filtrar explicitamente por OCI == 1 no cambia el resultado,
# pero deja documentado en el codigo el criterio de seleccion de la muestra.

# IMPORTANTE: P6426 solo aplica a las personas ocupadas, por lo que tiene
# ~53% de datos faltantes (NA) frente al total de la base. Estos NA NO se
# deben reemplazar por 0, ya que un NA aqui significa "no aplica" (no esta
# trabajando actualmente), no que la persona lleve 0 meses. Por eso se usa
# na.rm = TRUE en todos los calculos, y de aqui en adelante se trabaja sobre
# DATA_OCUPADOS en lugar de DATA_INGRESOS.

n_total_P6426 = nrow(DATA_INGRESOS)
n_validos_P6426 = sum(!is.na(DATA_OCUPADOS$P6426))
n_validos_P6426
round( (n_validos_P6426 / n_total_P6426) * 100 , 2)     # % de datos validos
round( (1 - n_validos_P6426 / n_total_P6426) * 100 , 2) # % de datos faltantes (NA)


# Analisis descriptivos de P6426 (Antiguedad laboral en meses)

## Indicadores de posicion y centro ----
DATA_OCUPADOS |> dplyr::group_by(1) |> dplyr::summarise(
  n_P6426             = sum(!is.na(P6426)) ,
  media_P6426         = mean(P6426 , na.rm = TRUE) ,
  mediana_P6426       = median(P6426 , na.rm = TRUE) ,
  rango_medio_P6426   = ( (max(P6426 , na.rm = TRUE) - min(P6426 , na.rm = TRUE)) / 2 ) ,
  min_P6426           = min(P6426 , na.rm = TRUE) ,
  Q1_P6426            = quantile(P6426 , 0.25 , na.rm = TRUE) ,
  Q3_P6426            = quantile(P6426 , 0.75 , na.rm = TRUE) ,
  rango_Q_P6426       = IQR(P6426 , na.rm = TRUE) ,
  max_P6426           = max(P6426 , na.rm = TRUE)
)

as.data.frame(DATA_OCUPADOS |> dplyr::group_by(1) |> dplyr::summarise(
  n_P6426             = sum(!is.na(P6426)) ,
  media_P6426         = mean(P6426 , na.rm = TRUE) ,
  mediana_P6426       = median(P6426 , na.rm = TRUE) ,
  rango_medio_P6426   = ( (max(P6426 , na.rm = TRUE) - min(P6426 , na.rm = TRUE)) / 2 ) ,
  min_P6426           = min(P6426 , na.rm = TRUE) ,
  Q1_P6426            = quantile(P6426 , 0.25 , na.rm = TRUE) ,
  Q3_P6426            = quantile(P6426 , 0.75 , na.rm = TRUE) ,
  rango_Q_P6426       = IQR(P6426 , na.rm = TRUE) ,
  max_P6426           = max(P6426 , na.rm = TRUE)
))

## Indicadores de dispersion y forma ----
DATA_OCUPADOS |> dplyr::group_by(1) |> dplyr::summarise(
  rango_P6426         = (max(P6426 , na.rm = TRUE) - min(P6426 , na.rm = TRUE)) ,
  sd_P6426            = sd(P6426 , na.rm = TRUE) ,
  varianza_P6426      = var(P6426 , na.rm = TRUE) ,
  c_varianza_P6426    = ( (sd(P6426 , na.rm = TRUE) / mean(P6426 , na.rm = TRUE)) * 100 ) ,
  c_curtosis_P6426    = kurtosis(P6426 , na.rm = TRUE) ,
  c_asimetria_P6426   = skewness(P6426 , na.rm = TRUE)
)


############################
# Estadisticos ADICIONALES #
############################

## Moda ----
moda = function(x) {
  x = x[!is.na(x)]
  tabla_freq = table(x)
  as.numeric(names(tabla_freq)[tabla_freq == max(tabla_freq)])
}
moda_P6426 = moda(DATA_OCUPADOS$P6426)
moda_P6426
# La antiguedad laboral que mas se repite es 24 meses (2 anios).

## Media geometrica y media armonica ----
P6426_sin_ceros = DATA_OCUPADOS$P6426[DATA_OCUPADOS$P6426 > 0 & !is.na(DATA_OCUPADOS$P6426)]

media_geometrica_P6426 = exp(mean(log(P6426_sin_ceros)))
media_geometrica_P6426

media_armonica_P6426 = length(P6426_sin_ceros) / sum(1 / P6426_sin_ceros)
media_armonica_P6426

# Ambas medias (~27.5 y ~8.4 meses) son mucho menores que la media aritmetica
# (~70.5 meses). Esto confirma que hay valores muy altos (personas con mucha
# antiguedad) que "jalan" fuertemente la media aritmetica hacia arriba.

## MAD - Desviacion absoluta mediana ----
mad_P6426 = mad(DATA_OCUPADOS$P6426 , na.rm = TRUE)
mad_P6426

# El MAD (~41.5) es bastante menor que la desviacion estandar (~98.7), lo que
# sugiere que la sd esta siendo fuertemente influenciada por valores extremos
# (outliers), mientras que el MAD, al ser mas robusto, no se ve tan afectado.

## Percentiles y deciles adicionales ----
quantile(DATA_OCUPADOS$P6426 , probs = seq(0.1, 0.9, by = 0.1) , na.rm = TRUE)

# El decil 9 (P90 = 216 meses = 18 anios) muestra que el 10% de las personas
# con mas antiguedad lleva 18 anios o mas en su empleo actual, mientras que
# el decil 1 (P10 = 3 meses) muestra que el 10% con menos antiguedad apenas
# lleva 3 meses o menos.

## Coeficiente de asimetria de Pearson ----
CAP_P6426 = (mean(DATA_OCUPADOS$P6426 , na.rm = TRUE) - median(DATA_OCUPADOS$P6426 , na.rm = TRUE)) /
  sd(DATA_OCUPADOS$P6426 , na.rm = TRUE)
CAP_P6426

# Un valor de ~0.37 (bastante alejado de 0) confirma una asimetria positiva
# considerable: la media (70.5 meses) es notablemente mayor que la mediana
# (34 meses), por la presencia de personas con muchos anios de antiguedad.

## Deteccion de valores atipicos (regla del boxplot) ----
Q1_P6426 = quantile(DATA_OCUPADOS$P6426 , 0.25 , na.rm = TRUE)
Q3_P6426 = quantile(DATA_OCUPADOS$P6426 , 0.75 , na.rm = TRUE)
IQR_P6426 = IQR(DATA_OCUPADOS$P6426 , na.rm = TRUE)

limite_inferior_P6426 = Q1_P6426 - 1.5 * IQR_P6426
limite_superior_P6426 = Q3_P6426 + 1.5 * IQR_P6426
limite_inferior_P6426
limite_superior_P6426

outliers_P6426 = DATA_OCUPADOS$P6426[DATA_OCUPADOS$P6426 < limite_inferior_P6426 |
                                        DATA_OCUPADOS$P6426 > limite_superior_P6426]
length(outliers_P6426)
round( (length(outliers_P6426) / n_validos_P6426) * 100 , 2)  # % de outliers

# A diferencia de la Edad, aqui SI aparecen valores atipicos: 4,066 casos
# (~10.66% de los datos validos) superan el limite superior de 196.5 meses.
# Esto es consistente con la asimetria positiva y la curtosis alta (~6.07):
# hay un grupo importante de personas con antiguedades muy largas (hasta 840
# meses = 70 anios) que se alejan bastante del resto de la muestra.


###########
# Graficos#
###########

## Boxplot ----
boxplot(DATA_OCUPADOS$P6426 ,
        main = "Boxplot de la Antiguedad Laboral en meses (P6426)" ,
        ylab = "Meses" ,
        col  = "lightgreen")

## Histograma ----
hist(DATA_OCUPADOS$P6426,
     main   = "Histograma de la Antiguedad Laboral (P6426)" ,
     xlab   = "Meses" ,
     ylab   = "Frecuencia" ,
     col    = "lightgreen" ,
     border = "white")

## Histograma con curva normal ----
hist(DATA_OCUPADOS$P6426,
     main   = "Histograma con curva normal" ,
     xlab   = "Meses" ,
     ylab   = "Densidad" ,
     col    = "lightgreen" ,
     border = "white" ,
     freq   = FALSE)

curve(dnorm(x,
            mean = mean(DATA_OCUPADOS$P6426, na.rm = TRUE) ,
            sd   = sd(DATA_OCUPADOS$P6426 , na.rm = TRUE)) ,
      col = "purple" ,
      lwd = 3 ,
      add = TRUE)

# Nota: como la variable tiene una asimetria fuerte y muchos outliers, la
# curva normal se va a ver muy distinta a las barras del histograma (a
# diferencia de lo que pasaba con la Edad). Esto es un buen ejemplo visual
# para explicar por que esta variable esta lejos de comportarse como normal.

## Todas las graficas juntas ----
par(mfrow = c(2, 2))

boxplot(DATA_OCUPADOS$P6426 ,
        main = "Boxplot Antiguedad Laboral" ,
        ylab = "Meses" ,
        col  = "lightgreen")

hist(DATA_OCUPADOS$P6426,
     main   = "Histograma Antiguedad Laboral" ,
     xlab   = "Meses" ,
     ylab   = "Frecuencia" ,
     col    = "lightgreen" ,
     border = "white")

hist(DATA_OCUPADOS$P6426,
     main   = "Histograma con curva normal" ,
     xlab   = "Meses" ,
     ylab   = "Densidad" ,
     col    = "lightgreen" ,
     border = "white" ,
     freq   = FALSE)
curve(dnorm(x,
            mean = mean(DATA_OCUPADOS$P6426, na.rm = TRUE) ,
            sd   = sd(DATA_OCUPADOS$P6426 , na.rm = TRUE)) ,
      col = "purple" , lwd = 3 , add = TRUE)

plot(density(DATA_OCUPADOS$P6426, na.rm = TRUE),
     main = "Densidad Antiguedad Laboral" ,
     xlab = "Meses" ,
     ylab = "Densidad" ,
     col  = "darkgreen" ,
     lwd  = 2)

par(mfrow = c(1, 1))


#######################################################################
# Analisis comparativo por ciudad (AREA) - P6426 (Antiguedad laboral) #
#######################################################################

# La variable AREA identifica la ciudad de la encuesta:
#   17 = Manizales | 63 = Armenia | 66 = Pereira

## Creamos una variable para las ciudades (Manizales , Armenia , Pereira) ----
DATA_OCUPADOS$Ciudad = factor(DATA_OCUPADOS$AREA ,
                               levels = c(17, 63, 66) ,
                               labels = c("Manizales", "Armenia", "Pereira"))

table(DATA_OCUPADOS$Ciudad , useNA = "always")

## Tabla de estadisticos de P6426 por ciudad ----
tabla_por_ciudad = DATA_OCUPADOS |>
  dplyr::group_by(Ciudad) |>
  dplyr::summarise(
    n           = sum(!is.na(P6426)) ,
    media       = mean(P6426 , na.rm = TRUE) ,
    mediana     = median(P6426 , na.rm = TRUE) ,
    sd          = sd(P6426 , na.rm = TRUE) ,
    CV_pct      = (sd(P6426 , na.rm = TRUE) / mean(P6426 , na.rm = TRUE)) * 100 ,
    asimetria   = skewness(P6426 , na.rm = TRUE) ,
    curtosis    = kurtosis(P6426 , na.rm = TRUE) ,
    Q1          = quantile(P6426 , 0.25 , na.rm = TRUE) ,
    Q3          = quantile(P6426 , 0.75 , na.rm = TRUE) ,
    max         = max(P6426 , na.rm = TRUE)
  )

as.data.frame(tabla_por_ciudad)

# Interpretacion esperada (valores de referencia calculados sobre la base):
#
#            n      media   mediana    sd     CV%    asimetria
# Manizales 14432   81.86     36     109.63   133.92    2.06
# Armenia   10372   54.82     24      78.82   143.79    2.83
# Pereira   13350   70.42     30      98.50   139.87    2.29
#
# - Manizales tiene la mayor antiguedad laboral promedio (81.9 meses ~ 6.8
#   anios), seguida de Pereira (70.4 meses) y Armenia (54.8 meses).

# - En las 3 ciudades la media es bastante mayor que la mediana, lo que
#   confirma que en todas hay asimetria positiva (personas con muchos anios
#   de antiguedad que "jalan" el promedio hacia arriba).

# - Armenia tiene la asimetria mas alta (2.83) y el CV mas alto (143.79%),
#   es decir, es la ciudad con mayor heterogeneidad relativa: conviven muchas
#   personas con poca antiguedad junto con algunas con antiguedades muy largas.

# - Manizales, aunque tiene la media mas alta, tiene el CV y la asimetria
#   mas bajos de las 3 ciudades, sugiriendo una distribucion relativamente
#   "menos extrema" que las otras dos.


## Boxplot comparativo por ciudad ----
boxplot(P6426 ~ Ciudad , data = DATA_OCUPADOS ,
        main = "Antiguedad Laboral (P6426) por Ciudad" ,
        xlab = "Ciudad" ,
        ylab = "Meses" ,
        col  = c("lightblue", "lightgreen", "lightpink"))

# Interpretacion del boxplot:

# - Se espera ver que la caja (IQR) y la mediana de Manizales esten un poco
#   mas arriba que las de Armenia y Pereira, reflejando su mayor antiguedad
#   laboral promedio.

# - Las 3 ciudades deberian mostrar varios puntos por fuera de los bigotes
#   (outliers), consistente con el ~10.7% de valores atipicos que ya
#   detectamos a nivel general.

# - Si el boxplot de Armenia se ve mas "apretado" hacia abajo pero con una
#   cola de outliers larga hacia arriba, eso confirma su mayor asimetria.


## Boxplot con escala logaritmica (opcional, mejora la lectura visual) ----

# Como la variable tiene una asimetria muy fuerte y varios outliers extremos,
# a veces ayuda visualizar en escala logaritmica para comparar mejor las
# cajas centrales sin que los outliers "aplasten" el grafico.
boxplot(log1p(P6426) ~ Ciudad , data = DATA_OCUPADOS ,
        main = "Antiguedad Laboral (P6426) por Ciudad - escala log" ,
        xlab = "Ciudad" ,
        ylab = "log(1 + Meses)" ,
        col  = c("lightblue", "lightgreen", "lightpink"))

##############################################################
# Exclusion de valores atipicos - P6426 (Antiguedad laboral) #
##############################################################

# Se reutilizan los limites ya calculados con la regla del boxplot
# (Q1 - 1.5*IQR ; Q3 + 1.5*IQR). Si no los tienes en el Environment, vuelve a
# correr ese bloque antes de este.

## Crear un subconjunto de datos SIN outliers ----
DATA_SIN_OUTLIERS = DATA_OCUPADOS |>
  dplyr::filter(P6426 >= limite_inferior_P6426 & P6426 <= limite_superior_P6426)

# Verificacion: cuantas filas quedaron vs cuantas se excluyeron
nrow(DATA_OCUPADOS)         # total antes de excluir (38154)
nrow(DATA_SIN_OUTLIERS)     # total despues de excluir
nrow(DATA_OCUPADOS) - nrow(DATA_SIN_OUTLIERS)   # cantidad de outliers excluidos


## Comparar estadisticos: CON outliers vs SIN outliers ----
DATA_OCUPADOS |> dplyr::group_by(1) |> dplyr::summarise(
  media_con     = mean(P6426, na.rm = TRUE) ,
  mediana_con   = median(P6426, na.rm = TRUE) ,
  sd_con        = sd(P6426, na.rm = TRUE) ,
  asimetria_con = skewness(P6426, na.rm = TRUE)
)

DATA_SIN_OUTLIERS |> dplyr::group_by(1) |> dplyr::summarise(
  media_sin     = mean(P6426, na.rm = TRUE) ,
  mediana_sin   = median(P6426, na.rm = TRUE) ,
  sd_sin        = sd(P6426, na.rm = TRUE) ,
  asimetria_sin = skewness(P6426, na.rm = TRUE)
)

# Interpretacion esperada:
# - Al excluir los outliers (~10.66% de los datos), la media deberia bajar
#   bastante (se acerca mas a la mediana), la desviacion estandar deberia
#   reducirse fuertemente, y la asimetria deberia disminuir mucho (la
#   distribucion se vuelve mas "normal", aunque seguramente sigue con algo
#   de asimetria positiva).
# - Esto demuestra el peso real que tenian esos valores extremos sobre los
#   estadisticos calculados con la base completa.


## Boxplot comparativo: con outliers vs sin outliers ----
par(mfrow = c(1, 2))

boxplot(DATA_OCUPADOS$P6426 ,
        main = "Con outliers" ,
        ylab = "Meses" ,
        col  = "lightgreen")

boxplot(DATA_SIN_OUTLIERS$P6426 ,
        main = "Sin outliers" ,
        ylab = "Meses" ,
        col  = "lightblue")

par(mfrow = c(1, 1))

#######################################################
# Comparativo por ciudad SIN valores atipicos - P6426 #
#######################################################

# Se reutiliza DATA_SIN_OUTLIERS (ya construido en la seccion de exclusion
# de outliers), que ya contiene la columna Ciudad heredada de DATA_OCUPADOS.

tabla_ciudad_sin_outliers = DATA_SIN_OUTLIERS |>
  dplyr::group_by(Ciudad) |>
  dplyr::summarise(
    n         = sum(!is.na(P6426)) ,
    media     = mean(P6426 , na.rm = TRUE) ,
    mediana   = median(P6426 , na.rm = TRUE) ,
    sd        = sd(P6426 , na.rm = TRUE) ,
    CV_pct    = (sd(P6426 , na.rm = TRUE) / mean(P6426 , na.rm = TRUE)) * 100 ,
    asimetria = skewness(P6426 , na.rm = TRUE)
  )

as.data.frame(tabla_ciudad_sin_outliers)

## Boxplot comparativo por ciudad, SIN outliers ----
boxplot(P6426 ~ Ciudad , data = DATA_SIN_OUTLIERS ,
        main = "Antiguedad Laboral (P6426) por Ciudad - Sin outliers" ,
        xlab = "Ciudad" ,
        ylab = "Meses" ,
        col  = c("lightblue", "lightgreen", "lightpink"))
