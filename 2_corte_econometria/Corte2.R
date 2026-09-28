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
options( 'scipen' = 100 , 'digits' = 6 )
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

