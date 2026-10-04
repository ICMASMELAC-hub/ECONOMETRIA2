########################################################################
######                                                            ######
#                      UNIVERSIDAD DEL QUINDÍO                         #
#                       PROGRAMA DE ECONOMÍA                           #
#                            ECONOMETRÍA                               #
######                                                            ######
########################################################################


# By: Iván Cristóbal Másmela Castro
# icmasmelac@uqvirtual.edu.co
# +57 310 820 6970

getwd()
options( 'scipen' = 100 , 'digits' = 4 )
rm(list = ls())


#Librerias
library('haven')
library('tidyverse')
library('lmtest')      # Para pruebas de hipótesis con errores robustos (coeftest)
library('sandwich')    # Para calcular la matriz de varianza-covarianza robusta (vcovHC)  # Errores robustos
library('plm')        # Estimación de modelos de panel
library('modelsummary') # Para tablas comparativas de modelos (equivalente a "estimates table")

## Base de datos
DATA_PANEL = haven::read_dta("C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/nls_panel.dta") |> data.frame()

## Descriptivos
head(DATA_PANEL)
str(DATA_PANEL)
skimr::skim(DATA_PANEL)

## Conocer la base de tipo panel contamos con i = 1 -> 716 (N); t = 1 -> 5 (T) 
print("Contamos con micropanel")
table(DATA_PANEL$id , useNA = "always")
table(DATA_PANEL$year , useNA = "always" )

# Si esta balanceado
DATA_PANEL |> count(id) |> count(n)

## Medias
ENTRE = DATA_PANEL |>  dplyr::group_by(id) |> 
  dplyr::summarise( media_unidad_SALARIO = mean(lwage , na.rm  = TRUE )   ,
                    media_unidad_EXP = mean(exper , na.rm  = TRUE ) ,
                    media_unidad_ANTG = mean(tenure , na.rm  = TRUE ) 
  )

print(ENTRE)

sd(ENTRE$media_unidad_SALARIO , na.rm = TRUE )
sd(ENTRE$media_unidad_EXP , na.rm = TRUE )
sd(ENTRE$media_unidad_ANTG , na.rm = TRUE )

# Variación dentro de cada unidad
DENTRO = DATA_PANEL |>  dplyr::group_by(id) |> 
  mutate( DESV_unidad_SALARIO = lwage - mean(lwage , na.rm  = TRUE )   ,
          DESV_unidad_EXP = exper - mean(exper , na.rm  = TRUE ) ,
          DESV_unidad_ANTG = tenure - mean(tenure , na.rm  = TRUE ) 
  )

sd(DENTRO$DESV_unidad_SALARIO , na.rm = TRUE )
sd(DENTRO$DESV_unidad_EXP , na.rm = TRUE )
sd(DENTRO$DESV_unidad_ANTG , na.rm = TRUE )


## Estadisticos por periodo
DATA_PANEL |>  dplyr::group_by(year) |> 
  dplyr::summarise( media_unidad_SALARIO = mean(lwage , na.rm  = TRUE )   ,
                    media_unidad_EXP = mean(exper , na.rm  = TRUE ) ,
                    media_unidad_ANTG = mean(tenure , na.rm  = TRUE ) 
  )



## Regresiones Clasicas
reg_82 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 82) )

summary(reg_82)


reg_83 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 83) )

summary(reg_83)


table(DATA_PANEL$year)
#reg_84 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 84) )

#summary(reg_84)


reg_85 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 85) )

summary(reg_85)


#reg_86 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 86) )

#summary(reg_86)


reg_87 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 87) )

summary(reg_87)


reg_88 = lm( lwage ~ exper + exper2 + tenure + tenure2 , data = DATA_PANEL , subset = (year == 88) )

summary(reg_88)

## ESTIMACIÓN POOLED

#DATA_PANEL

pooled = lm(  lwage ~ exper + exper2 + tenure + tenure2 + south + union + black + educ + 
                factor(year) , data = DATA_PANEL )

summary(pooled)

rm(pooled , reg_88 , reg_87 , reg_85 , reg_83 , reg_82)

# Efectos fijos por diferencias t_{1}: 82 , t_{2}: 88

# Primero base ancha tomando 82-88
DATOS_82_88 = DATA_PANEL |> 
  dplyr::filter( 
    year %in% c(82 , 88) 
  ) |> 
  dplyr::select(
    id , year, lwage, south , union , exper , exper2 , tenure  , tenure2
  ) |> pivot_wider( id_cols = id , names_from = year ,values_from = 
                      c(lwage, south , union , exper , exper2 , tenure  , tenure2))


# 1 - 82 0.34
# 1 - 88 0.53
# 2 - 82 0.65
# 2 - 88 0.55
# 
# id ln_82 ln_88
# 1 0.34 0.53
# 2 0.65 0.55

str(DATOS_82_88)

# Segundo calcular las diferencias t_{2} - t_{1}
DATA_DIFF = DATOS_82_88 |> 
  mutate( 
    c_lwage = lwage_88 - lwage_82 ,
    c_exper = exper_88 - exper_82 ,
    c_exper2 = exper2_88 - exper2_82 ,
    c_tenure = tenure_88 - tenure_82,
    c_tenure2 = tenure2_88 - tenure2_82 ,
    c_south = south_88 - south_82 ,
    c_union = union_88 - union_82 
  )

# Correr la regresión
red_diff = lm( c_lwage ~ c_exper + c_exper2 + c_tenure + c_tenure2 + c_south + c_union ,
               data = DATA_DIFF )

summary(red_diff)

rm(red_diff)

## Modelo de efectos fijos automatico
panel_de_datos = pdata.frame(DATA_PANEL , index = c("id" , "year" ) )

FE_INCORRECTA = plm( lwage ~ exper + exper2 + tenure  + tenure2 + educ + south + black + union ,
                     data = panel_de_datos , model = "within")

summary(FE_INCORRECTA)


## Descriptivos generales
glimpse(panel_de_datos)
summary(panel_de_datos)

pvar(panel_de_datos$educ)
pvar(panel_de_datos$black)
table(panel_de_datos$black)

## Miremos la variación (si existe) de black
DATOS_TRANS = DATA_PANEL |> 
  arrange( id , year ) |> 
  group_by( id) |> 
  mutate( BLACK_lag = lag(black) ) |> 
  ungroup()

str(DATOS_TRANS)

table(DATOS_TRANS$BLACK_lag , DATOS_TRANS$black , dnn = c ("t-1" , "t"))
