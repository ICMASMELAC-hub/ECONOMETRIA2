###############################################################################
###########                                                      ##############
#                        UNIVERSIDAD DEL QUINDIO                              #
#                         PROGRAMA DE ECONOMIA                                #
#                             ECONOMETRIA                                     #
########                                                               ########
###############################################################################

#BY: Lized Cristina Blandon Paladines, Iván	Cristóbal	Masmela	Castro, Juan	Jacobo	Obando	Franco

###############################################################################

## =========================================================================
## TALLER ECONOMETRIA II - PUNTO 2
## Estadisticos descriptivos: (A) Datos panel  |  (B) Serie de tiempo
## =========================================================================
## Bases:
##   - DATA_PANEL.dta      -> Panel de finanzas municipales (1103 municipios,
##                            1984-2021): INGRESOSTOTALES, Gastosdefuncionamiento,
##                            Ingresosnotributarios, GastosdeCapital, IngresosCorrientes
##   - Serie_Predial.xlsx  -> Serie mensual del recaudo de Predial (2009-2023)
## =========================================================================

###############################################################################

# librerias ____
library(skimr)
library(readxl)
library(stringr)
library(stringi)
library(haven)
library(tidyverse)
library(plyr)
library(rstatix)
library(e1071)

## 1. Cargar la base -----

DATA_INGRESOS <- read_xlsx(
  "C:/Users/Usuario/Downloads/TALLER 1 ECONOMETRIA 2/Serie_Predial.xlsx"
)

DATA_PANEL <- read_dta(
  "C:/Users/Usuario/Downloads/TALLER 1 ECONOMETRIA 2/DATA_PANEL.dta"
)

# ---- 0. Paquetes -------------------------------------------------------
# haven   -> leer archivos .dta de Stata
# readxl  -> leer archivos .xlsx
# dplyr   -> manipulacion de datos (group_by, summarise, mutate)
# plm     -> econometria de datos panel (pdata.frame, pdim, Between, Within)
# psych   -> estadisticos descriptivos ampliados (asimetria, curtosis)
# forecast-> descomposicion, medias moviles, graficos de series de tiempo
# tseries -> pruebas de raiz unitaria (adf.test) y de autocorrelacion
# ggplot2 -> graficos
paquetes <- c("haven", "readxl", "dplyr", "plm", "psych",
              "forecast", "tseries", "ggplot2", "lubridate")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(paquetes, library, character.only = TRUE))


## =========================================================================
## PARTE A. DATOS PANEL  ---------------------------------------------------
## =========================================================================

# ---- A.1 Cargar y limpiar ----------------------------------------------
panel <- haven::read_dta("DATA_PANEL.dta")

vars_panel <- c("INGRESOSTOTALES", "Gastosdefuncionamiento",
                "Ingresosnotributarios", "GastosdeCapital", "IngresosCorrientes")

# Se detecta un codigo de dato perdido (-2000) en una sola fila: se recodifica a NA
panel <- panel %>%
  mutate(across(all_of(vars_panel), ~ ifelse(. == -2000, NA, .)))

# ---- A.2 Declarar la estructura panel -----------------------------------
pdata <- pdata.frame(panel, index = c("CODIGO", "year"))
pdim(pdata)          # confirma N (municipios) y T (anios) y si el panel es balanceado

# ---- A.3 Estadisticos descriptivos generales (pooled) -------------------
psych::describe(panel[, vars_panel])

# ---- A.4 PROMEDIO GRUPAL (dimension "between": un valor por municipio) --
# Colapsa la serie de cada municipio en su promedio 1984-2021.
# Mide la HETEROGENEIDAD ENTRE MUNICIPIOS (por que unos recaudan mas que otros).

promedio_grupal <- panel %>%
  dplyr::group_by(CODIGO, MUNICIPIO) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_panel),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


psych::describe(promedio_grupal[, vars_panel])   # distribucion de los promedios municipales
promedio_grupal %>% arrange(desc(INGRESOSTOTALES)) %>% head(10)   # top 10 municipios
promedio_grupal %>% arrange(INGRESOSTOTALES) %>% head(10)         # 10 municipios mas bajos

# ---- A.5 PROMEDIO TEMPORAL (dimension "within": un valor por año) ------
# Colapsa la serie de todos los municipios en el promedio nacional cada año.
# Mide la EVOLUCION EN EL TIEMPO del sistema fiscal municipal en su conjunto.

promedio_temporal <- panel %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_panel),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

print(promedio_temporal, n = 40)

ggplot(promedio_temporal, aes(x = year, y = INGRESOSTOTALES)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Promedio temporal de INGRESOSTOTALES (todos los municipios)",
       x = "Año", y = "Ingresos totales (promedio nacional)") +
  theme_minimal()

# ---- A.6 Descomposicion de varianza: between vs. within -----------------
# Responde: ¿la variabilidad de los datos viene mas de las DIFERENCIAS ENTRE
# municipios (between) o de los CAMBIOS EN EL TIEMPO dentro de cada municipio (within)?
decomp_varianza <- sapply(vars_panel, function(v) {
  serie      <- pdata[[v]]
  ov  <- var(serie, na.rm = TRUE)                    # varianza total (overall)
  bv  <- var(Between(serie, na.rm = TRUE), na.rm = TRUE)   # varianza "between" (plm)
  wv  <- var(Within(serie, na.rm = TRUE), na.rm = TRUE)    # varianza "within"  (plm)
  c(overall = ov, between = bv, within = wv,
    pct_between = bv / ov * 100, pct_within = wv / ov * 100)
})
round(t(decomp_varianza), 2)


## =========================================================================
## PARTE B. SERIE DE TIEMPO (Recaudo Predial mensual, 2009-2023) ----------
## =========================================================================

# ---- B.1 Cargar y construir el objeto ts --------------------------------
predial_df <- readxl::read_excel("Serie_Predial.xlsx", sheet = "Hoja1")

predial_ts <- ts(predial_df$`PREDIAL(Vigencia Actual)`,
                 start = c(2009, 1), frequency = 12)   # frecuencia 12 = mensual

# ---- B.2 Estadisticos descriptivos de la serie --------------------------
summary(predial_ts)
sd(predial_ts)
cv <- sd(predial_ts) / mean(predial_ts)          # coeficiente de variacion
cv
psych::describe(as.numeric(predial_ts))          # incluye asimetria (skew) y curtosis

plot.ts(predial_ts, main = "Recaudo mensual de Predial 2009-2023",
        ylab = "Pesos", col = "darkred")

# ---- B.3 Descomposicion de la serie -------------------------------------
# Se usa STL (Seasonal-Trend decomposition using Loess) porque:
#   (i) permite que el componente estacional cambie de intensidad en el tiempo,
#   (ii) es robusto a valores atipicos (opcion robust = TRUE),
#   (iii) funciona bien aunque la estacionalidad no sea perfectamente regular,
#        como ocurre aqui por el descuento de pronto pago en el 1er trimestre.
descom_stl <- stl(predial_ts, s.window = "periodic", robust = TRUE)
plot(descom_stl)

# Alternativa clasica (multiplicativa), util para comparar:
descom_classic <- decompose(predial_ts, type = "multiplicative")
plot(descom_classic)

# Varianza explicada por cada componente (aproximacion)
comp <- descom_stl$time.series
var_total <- var(predial_ts)
sapply(list(tendencia = comp[, "trend"],
            estacional = comp[, "seasonal"],
            residuo   = comp[, "remainder"]),
       function(x) var(x) / var_total * 100)

# ---- B.4 Estacionalidad: promedio por mes -------------------------------
predial_df$mes <- lubridate::month(as.Date(paste0(predial_df$YEAR, "-", predial_df$MONTH, "-01")))
tapply(predial_df$`PREDIAL(Vigencia Actual)`, predial_df$mes, mean, na.rm = TRUE)
monthplot(predial_ts, main = "Patron estacional por mes - Predial")

# ---- B.5 Medias moviles -------------------------------------------------
ma3  <- forecast::ma(predial_ts, order = 3)    # suaviza ruido de corto plazo
ma12 <- forecast::ma(predial_ts, order = 12)   # aisla la tendencia (elimina estacionalidad)

plot(predial_ts, col = "gray60", main = "Serie original vs. medias moviles")
lines(ma3,  col = "blue",  lwd = 2)
lines(ma12, col = "red",   lwd = 2)
legend("topleft", legend = c("Serie original", "MA(3)", "MA(12)"),
       col = c("gray60", "blue", "red"), lty = 1, lwd = 2)

# ---- B.6 Correlacion serial ----------------------------------------------
acf(predial_ts,  lag.max = 24, main = "ACF - Predial")
pacf(predial_ts, lag.max = 24, main = "PACF - Predial")

Box.test(predial_ts, lag = 12, type = "Ljung-Box")   # H0: no autocorrelacion
Box.test(predial_ts, lag = 24, type = "Ljung-Box")

# ---- B.7 Estacionariedad (raiz unitaria) ----------------------------------
tseries::adf.test(predial_ts)                 # en niveles
tseries::adf.test(diff(predial_ts))           # en primeras diferencias
tseries::adf.test(diff(predial_ts, lag = 12)) # en diferencia estacional

## =========================================================================
## FIN PUNTO 2
## =========================================================================










