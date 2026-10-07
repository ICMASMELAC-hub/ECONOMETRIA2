########################################################################
######                                                            ######
#                      UNIVERSIDAD DEL QUINDÍO                         #
#                       PROGRAMA DE ECONOMÍA                           #
#                           ECONOMETRÍA II                             #
#             ACTIVIDAD 2 - ESTIMACIÓN SOBRE DATOS PANEL               #
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
library('lmtest')       
library('sandwich')     
library('plm')          
library('modelsummary') 

## Base de datos
DATA_PANEL = haven::read_dta("C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/nls_panel.dta") |> data.frame()


########################################################################
# 1. PLANTEAMIENTO DEL MODELO Y NUEVAS HIPÓTESIS
########################################################################
# Modelo de clase:
#
# lwage ~ exper + exper2 + tenure + tenure2 + south + union + black + educ
#
# NUEVO MODELO (salario explicado por jornada, afiliación, estado civil
# y localización):
#
# lwage_it ~ b1 hours_it + b2 union_it + b3 msp_it + b4 not_smsa_it
#          + b5 c_city_it + (b6 educ_i + b7 black_i) + a_i + u_it
#
# Categoría base de localización: vivir en zona suburbana de un área
# metropolitana (SMSA). not_smsa = fuera de SMSA; c_city = centro de ciudad.
#
# H1: b1 ≠ 0  Las horas usuales de trabajo afectan el salario por hora.
#             
# H2: b2 > 0   Los afiliados a sindicato ganan más (prima sindical).
#
# H3: b3 > 0   Estar casado con cónyuge presente aumenta el salario
#              
# H4: b4 < 0   Vivir fuera de un área metropolitana reduce el salario.
#
# H5: b5 > 0   Vivir en el centro de la ciudad aumenta el salario
#              
# H6: b6 > 0   Más educación aumenta el salario (solo pooled y RE).
#
# H7: Existen efectos individuales no observados (a_i) correlacionados con
#     los regresores, que sesgan el estimador pooled.


########################################################################
# 2. DESCRIPTIVOS
########################################################################
head(DATA_PANEL)
str(DATA_PANEL)
skimr::skim(DATA_PANEL)

## Conocer la base de tipo panel contamos con i = 1 -> 716 (N); t = 1 -> 5 (T)
print("Contamos con micropanel")
table(DATA_PANEL$id , useNA = "always")
table(DATA_PANEL$year , useNA = "always" )

# Si esta balanceado
DATA_PANEL |> count(id) |> count(n)

## Medias (variación ENTRE unidades)
ENTRE = DATA_PANEL |>  dplyr::group_by(id) |>
  dplyr::summarise( media_unidad_SALARIO = mean(lwage , na.rm  = TRUE ) ,
                    media_unidad_HORAS   = mean(hours , na.rm  = TRUE ) ,
                    media_unidad_UNION   = mean(union , na.rm  = TRUE ) ,
                    media_unidad_MSP     = mean(msp , na.rm  = TRUE ) ,
                    media_unidad_NOSMSA  = mean(not_smsa , na.rm  = TRUE ) ,
                    media_unidad_CCITY   = mean(c_city , na.rm  = TRUE )
  )

print(ENTRE)

sd(ENTRE$media_unidad_SALARIO , na.rm = TRUE )
sd(ENTRE$media_unidad_HORAS , na.rm = TRUE )
sd(ENTRE$media_unidad_UNION , na.rm = TRUE )
sd(ENTRE$media_unidad_MSP , na.rm = TRUE )
sd(ENTRE$media_unidad_NOSMSA , na.rm = TRUE )
sd(ENTRE$media_unidad_CCITY , na.rm = TRUE )

# Variación DENTRO de cada unidad
DENTRO = DATA_PANEL |>  dplyr::group_by(id) |>
  mutate( DESV_unidad_SALARIO = lwage - mean(lwage , na.rm  = TRUE ) ,
          DESV_unidad_HORAS   = hours - mean(hours , na.rm  = TRUE ) ,
          DESV_unidad_UNION   = union - mean(union , na.rm  = TRUE ) ,
          DESV_unidad_MSP     = msp - mean(msp , na.rm  = TRUE ) ,
          DESV_unidad_NOSMSA  = not_smsa - mean(not_smsa , na.rm  = TRUE ) ,
          DESV_unidad_CCITY   = c_city - mean(c_city , na.rm  = TRUE )
  )

sd(DENTRO$DESV_unidad_SALARIO , na.rm = TRUE )
sd(DENTRO$DESV_unidad_HORAS , na.rm = TRUE )
sd(DENTRO$DESV_unidad_UNION , na.rm = TRUE )
sd(DENTRO$DESV_unidad_MSP , na.rm = TRUE )
sd(DENTRO$DESV_unidad_NOSMSA , na.rm = TRUE )
sd(DENTRO$DESV_unidad_CCITY , na.rm = TRUE )


## Estadisticos por periodo
DATA_PANEL |> dplyr::group_by(year) |>
  dplyr::summarise( media_unidad_SALARIO = mean(lwage) ,
                    media_unidad_HORAS   = mean(hours) ,
                    media_unidad_UNION   = mean(union) ,
                    media_unidad_MSP     = mean(msp) ,
                    media_unidad_NOSMSA  = mean(not_smsa) ,
                    media_unidad_CCITY   = mean(c_city) ) |>
  as.data.frame()


########################################################################
# 3. REGRESIONES CLÁSICAS
########################################################################
# Años disponibles: 82, 83, 85, 87, 88 (no existen 84 ni 86)
table(DATA_PANEL$year)

reg_82 = lm( lwage ~ hours + union + msp + not_smsa + c_city , data = DATA_PANEL , subset = (year == 82) )
summary(reg_82)

reg_83 = lm( lwage ~ hours + union + msp + not_smsa + c_city , data = DATA_PANEL , subset = (year == 83) )
summary(reg_83)

reg_85 = lm( lwage ~ hours + union + msp + not_smsa + c_city , data = DATA_PANEL , subset = (year == 85) )
summary(reg_85)

reg_87 = lm( lwage ~ hours + union + msp + not_smsa + c_city , data = DATA_PANEL , subset = (year == 87) )
summary(reg_87)

reg_88 = lm( lwage ~ hours + union + msp + not_smsa + c_city , data = DATA_PANEL , subset = (year == 88) )
summary(reg_88)


########################################################################
# 4. ESTIMACIÓN POOLED
########################################################################
pooled = lm( lwage ~ hours + union + msp + not_smsa + c_city + black + educ +
               factor(year) , data = DATA_PANEL )

summary(pooled)

rm(pooled , reg_88 , reg_87 , reg_85 , reg_83 , reg_82)


########################################################################
# 5. EFECTOS FIJOS POR DIFERENCIAS  t_{1}: 82 , t_{2}: 88
########################################################################
# Primero base ancha tomando 82-88
DATOS_82_88 = DATA_PANEL |>
  dplyr::filter(
    year %in% c(82 , 88)
  ) |>
  dplyr::select(
    id , year, lwage, hours , union , msp , not_smsa , c_city
  ) |> pivot_wider( id_cols = id , names_from = year , values_from =
                      c(lwage, hours , union , msp , not_smsa , c_city))

str(DATOS_82_88)

# Segundo calcular las diferencias t_{2} - t_{1}
DATA_DIFF = DATOS_82_88 |>
  mutate(
    c_lwage    = lwage_88 - lwage_82 ,
    c_hours    = hours_88 - hours_82 ,
    c_union    = union_88 - union_82 ,
    c_msp      = msp_88 - msp_82 ,
    c_not_smsa = not_smsa_88 - not_smsa_82 ,
    c_c_city   = c_city_88 - c_city_82
  )

# Correr la regresión
  red_diff = lm( c_lwage ~ c_hours + c_union + c_msp + c_not_smsa + c_c_city ,
                 data = DATA_DIFF )
  
  summary(red_diff)
  
  rm(red_diff)


########################################################################
# 6. MODELO DE EFECTOS FIJOS AUTOMÁTICO (plm)
########################################################################
panel_de_datos = pdata.frame(DATA_PANEL , index = c("id" , "year" ) )

# Con educ y black: el estimador within las elimina (no varían en el tiempo)
FE_INCORRECTA = plm( lwage ~ hours + union + msp + not_smsa + c_city + educ + black ,
                     data = panel_de_datos , model = "within")

summary(FE_INCORRECTA)

# Modelo de efectos fijos correcto (solo variables que cambian en el tiempo)
FE = plm( lwage ~ hours + union + msp + not_smsa + c_city ,
          data = panel_de_datos , model = "within")

summary(FE)

## Descriptivos generales
glimpse(panel_de_datos)
summary(panel_de_datos)

pvar(panel_de_datos$educ)
pvar(panel_de_datos$black)
pvar(panel_de_datos$hours)
pvar(panel_de_datos$not_smsa)
pvar(panel_de_datos$c_city)
table(panel_de_datos$black)

## Miremos la variación (si existe) de not_smsa y c_city
DATOS_TRANS = DATA_PANEL |>
  arrange( id , year ) |>
  group_by( id) |>
  mutate( NOSMSA_lag = dplyr::lag(not_smsa) ,
          CCITY_lag  = dplyr::lag(c_city) ) |>
  ungroup()

table(DATOS_TRANS$NOSMSA_lag , DATOS_TRANS$not_smsa , dnn = c ("t-1" , "t"))
table(DATOS_TRANS$CCITY_lag , DATOS_TRANS$c_city , dnn = c ("t-1" , "t"))

# Pocas personas cambian de zona,lo cual hay poca variación "dentro", por eso los
# efectos de la localización son imprecisos en FE.


########################################################################
# 7. COMPLEMENTOS 
########################################################################

# Se vuelven a crear porque fueron eliminados con rm()
pooled = lm( lwage ~ hours + union + msp + not_smsa + c_city + black + educ +
               factor(year) , data = DATA_PANEL )

red_diff = lm( c_lwage ~ c_hours + c_union + c_msp + c_not_smsa + c_c_city ,
               data = DATA_DIFF )

# 7.1 Errores robustos agrupados por individuo
coeftest(pooled , vcov = vcovCL(pooled , cluster = DATA_PANEL$id))
coeftest(FE , vcov = vcovHC(FE , method = "arellano" , type = "HC1" , cluster = "group"))

# 7.2 Efectos aleatorios (aquí sí entran educ y black)
RE = plm( lwage ~ hours + union + msp + not_smsa + c_city + educ + black ,
          data = panel_de_datos , model = "random")
summary(RE)

# 7.3 H7: test F de efectos individuales (FE vs pooled)
POOL_PLM = plm( lwage ~ hours + union + msp + not_smsa + c_city ,
                data = panel_de_datos , model = "pooling")
pFtest(FE , POOL_PLM)

# 7.4 Hausman (FE vs RE con los mismos regresores)
RE_H = plm( lwage ~ hours + union + msp + not_smsa + c_city ,
            data = panel_de_datos , model = "random")
phtest(FE , RE_H)

# 7.5 Tabla comparativa final
modelsummary( list("Pooled" = pooled , "Dif 82-88" = red_diff ,
                   "FE" = FE , "RE" = RE) ,
              stars = TRUE , coef_omit = "factor\\(year\\)" ,
              gof_omit = "AIC|BIC|Log|F|RMSE" )

########################################################################
print("IVAN CRISTOBAL MASMELA CASTRO ~ ECONOMETRÍA II - JORNADA NOCTURNA")
########################################################################
