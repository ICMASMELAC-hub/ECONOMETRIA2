
######                                                            ######
#                      UNIVERSIDAD DEL QUINDÍO                         #
#                       PROGRAMA DE ECONOMÍA                           #
#                           ECONOMETRÍA II                             #
######                                                            ######
########################################################################

# By: Iván Cristóbal Másmela Castro
# icmasmelac@uqvirtual.edu.co
# +57 310 820 6970

# Librerias 

library(dynlm)
library(vars)
library(tseries)
library(urca)
library(strucchange)
library(wooldridge)

# 1. Base de datos (Boston College - Wooldridge: intdef)
# Frecuencia anual: 1948 - 2003 (T = 56 observaciones)

data("intdef")
D = intdef

head(D)
str(D)
skimr::skim(D)

# Verificar años con inflación negativa (deflación)
D[D$inf < 0, c("year", "i3", "inf")]

# 2. Series (Teoría económica: Efecto Fisher)
TASA = D$i3
INF  = D$inf

TASAT = ts(TASA, start = c(1948, 1), frequency = 1)
INFT  = ts(INF,  start = c(1948, 1), frequency = 1)

# Gráfico inicial de las series en niveles
ts.plot(TASAT, INFT, col = c("steelblue", "firebrick"), lty = c(1, 2),
        main = "Tasa de Interés (i3) e Inflación (inf) en EE. UU. (1948-2003)",
        ylab = "Porcentaje (%)")
legend("topright", legend = c("Tasa de Interés (i3)", "Inflación (inf)"),
       col = c("steelblue", "firebrick"), lty = c(1, 2))

# ==============================================================================
# =============== DESARROLLO EXAMEN PARCIAL III - ECONOMETRIA II ===============
# ==============================================================================

# ==============================================================================
# (a) Pruebas de raíz unitaria (ADF, Phillips-Perron y KPSS)
# ==============================================================================

# En niveles
adf.test(TASAT)
adf.test(INFT)

pp.test(TASAT)
pp.test(INFT)

kpss.test(TASAT)
kpss.test(INFT)

# En primeras diferencias
D.TASAT = diff(TASAT, 1)
D.INFT  = diff(INFT, 1)

adf.test(D.TASAT)
adf.test(D.INFT)

pp.test(D.TASAT)
pp.test(D.INFT)

kpss.test(D.TASAT)
kpss.test(D.INFT)

# ==============================================================================
# (b) Pruebas de cointegración (Engle-Granger, Phillips-Ouliaris y Johansen)
# ==============================================================================

D.T.I = data.frame(D.TASAT, D.INFT)
T.I   = data.frame(TASAT, INFT)

VARselect(T.I, lag.max = 6, type = "const")

# Prueba de "ENGLE-GRANGER"
EG.T.I = dynlm(TASAT ~ INFT)
summary(EG.T.I)
adf.test(resid(EG.T.I))

# Prueba de "PHILLIPS-OULIARIS"
po.test(T.I)

# Prueba de "JOHANSEN"
PJ.T.I.EIGEN = ca.jo(T.I, type = c("eigen"), ecdet = ("const"), K = 2, spec = c("longrun"))
summary(PJ.T.I.EIGEN)

PJ.T.I.TRACE = ca.jo(T.I, type = c("trace"), ecdet = ("const"), K = 2, spec = c("longrun"))
summary(PJ.T.I.TRACE)

# ==============================================================================
# (c) Modelo de VEC de Tasa de Interes e Inflación
# ==============================================================================

# Ecuación del VEC con términos de corrección de error (ect1)
VEC.T.I = cajorls(PJ.T.I.EIGEN, r = 1)
summary(VEC.T.I$rlm)

# Conversión a representación VAR en niveles
VAR.T.I = vec2var(PJ.T.I.EIGEN, r = 1)
VAR.T.I

# Gráfico de residuos, ACF y PACF del modelo VEC (en reemplazo de plot(VAR.T.I))
RES.VEC = resid(VAR.T.I)

plot.ts(RES.VEC, main = "Residuos del Modelo VEC (TASAT e INFT)")
acf(RES.VEC)


# ==============================================================================
# (d) Contraste de especificación al modelo VEC
# ==============================================================================

serial.test(VAR.T.I)     # (Asymptotic) Portmanteau Test - Autocorrelación
arch.test(VAR.T.I)       # (Multivariate) ARCH Test - Homocedasticidad
normality.test(VAR.T.I)  # (Multivariate) Jarque-Bera - Normalidad

# 1. Estabilidad matemática: Raíces del polinomio característico (Matriz Compañera)
A1 = VAR.T.I$A$A1
A2 = VAR.T.I$A$A2
M.COMP = rbind(cbind(A1, A2), cbind(diag(2), matrix(0, 2, 2)))
abs(eigen(M.COMP)$values)

# 2. Estabilidad estructural gráfica: Prueba CUSUM por ecuación
DATOS.VEC = data.frame(
  TASAT.d   = VEC.T.I$rlm$model[[1]][, 1],
  INFT.d    = VEC.T.I$rlm$model[[1]][, 2],
  ect1      = VEC.T.I$rlm$model$ect1,
  TASAT.dl1 = VEC.T.I$rlm$model$TASAT.dl1,
  INFT.dl1  = VEC.T.I$rlm$model$INFT.dl1
)

plot(efp(TASAT.d ~ ect1 + TASAT.dl1 + INFT.dl1 - 1, data = DATOS.VEC, type = "Rec-CUSUM"),
     main = "Estabilidad CUSUM - Ecuación Tasa de Interés (TASAT)")

plot(efp(INFT.d ~ ect1 + TASAT.dl1 + INFT.dl1 - 1, data = DATOS.VEC, type = "Rec-CUSUM"),
     main = "Estabilidad CUSUM - Ecuación Inflación (INFT)")
# ==============================================================================
# Funciones Impulso-Respuesta (Bidireccionales)
# ==============================================================================

IFR.T.I.1 = irf(VAR.T.I, impulse = "INFT", response = "TASAT")
plot(IFR.T.I.1)
IFR.T.I.1

IFR.T.I.2 = irf(VAR.T.I, impulse = "TASAT", response = "INFT")
plot(IFR.T.I.2)
IFR.T.I.2
