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


# ==============================================================================
# (a) ESPECIFICACIÓN DEL MODELO
# ==============================================================================

if (!require(wooldridge)) install.packages("wooldridge")
if (!require(tseries)) install.packages("tseries")

library(wooldridge)
library(tseries)

data("wage1")
wage1$lwage <- log(wage1$wage)

nrow(wage1) #Observaciones de corte transversal extraidas de la base

# ==============================================================================
# (b) ANÁLISIS DESCRIPTIVO CON TEST ESTADÍSTICOS
# ==============================================================================

# 1. Selección de variables
vars <- c("wage", "lwage", "educ", "exper", "tenure")
df_sub <- wage1[, vars]


# 2. Resumen estadístico (Medida de tendencia central y dispersión)
print("=== RESUMEN ESTADÍSTICO ===")
summary(df_sub)

# 3. Desviación Estándar de las variables
print("=== DESVIACIÓN ESTÁNDAR ===")
sapply(df_sub, sd)

# 4. Matriz de Correlación de Pearson
print("=== MATRIZ DE CORRELACIÓN ===")
round(cor(df_sub), 4)

# 5. Test Estadístico de Normalidad (Jarque-Bera)
print("=== TEST ESTADÍSTICO DE NORMALIDAD (JARQUE-BERA) ===")
cat("JB wage:  p-value =", jarque.bera.test(wage1$wage)$p.value, "\n")
cat("JB lwage: p-value =", jarque.bera.test(wage1$lwage)$p.value, "\n")

# ==============================================================================
# (c) ESTIMACIÓN DE LOS PARÁMETROS
# ==============================================================================

modelo_mincer <- lm(lwage ~ educ + exper + tenure, data = wage1)
summary(modelo_mincer)

# ==============================================================================
# (d) TABLA ANOVA
# ==============================================================================

# 1. Calcular las sumas de cuadrados
ss_total <- sum((wage1$lwage - mean(wage1$lwage))^2)  # SST
ss_res   <- sum(residuals(modelo_mincer)^2)            # SSE
ss_mod   <- ss_total - ss_res                         # SSR

df_mod <- modelo_mincer$rank - 1                      # k = 3
df_res <- df.residual(modelo_mincer)                  # n - k - 1 = 522
df_tot <- nrow(wage1) - 1                             # n - 1 = 525

ms_mod <- ss_mod / df_mod
ms_res <- ss_res / df_res

f_stat <- ms_mod / ms_res
p_val  <- pf(f_stat, df_mod, df_res, lower.tail = FALSE)

# 2. Mostrar la Tabla ANOVA 
tabla_anova_clean <- data.frame(
  Suma_Cuadrados = c(ss_mod, ss_res, ss_total),
  Grados_Libertad = c(df_mod, df_res, df_tot),
  Cuadrado_Medio = c(ms_mod, ms_res, NA),
  Estadistico_F = c(f_stat, NA, NA),
  p_valor = c(p_val, NA, NA),
  row.names = c("Modelo (Explicada)", "Residuos (No explicada)", "Total")
)

round(tabla_anova_clean, 4)

# ==============================================================================
# (e) PRUEBA DE SIGNIFICANCIA INDIVIDUAL 
# ==============================================================================

summary(modelo_mincer)

# 1. Valor crítico para la SIGNIFICANCIA INDIVIDUAL (Test t de Student, 2 colas)
qt(0.975, df = 522)
# Resultado: +/- 1.965

# ==============================================================================
# (f) PRUEBA DE SIGNIFICANCIA GLOBAL 
# ==============================================================================

# Extraer el estadístico F y el p-valor directamente del modelo
resumen <- summary(modelo_mincer)

f_stat  <- resumen$fstatistic[1]
df_num  <- resumen$fstatistic[2] # Grados de libertad del numerador (k = 3)
df_den  <- resumen$fstatistic[3] # Grados de libertad del denominador (n - k - 1 = 522)
p_valor <- pf(f_stat, df_num, df_den, lower.tail = FALSE)

cat("Estadístico F:", round(f_stat, 4), "\n")
cat("Grados de libertad:", df_num, "y", df_den, "\n")
cat("p-valor:", format.pval(p_valor, eps = 0.001), "\n")

# 1. Valor crítico para la SIGNIFICANCIA GLOBAL (Test F de Snedecor, 1 cola)
qf(0.95, df1 = 3, df2 = 522)
# Resultado: 2.622

# ==============================================================================
# (g) VERIFICACIÓN DE SUPUESTOS
# ==============================================================================

# Cargar librerías para la verificación de supuestos
library(lmtest)   # Para Breusch-Pagan y Durbin-Watson
library(car)      # Para VIF (Multicolinealidad)
library(tseries)  # Para Jarque-Bera (Normalidad)
library(sandwich) # Para Errores Estándar Robustos (White)

# 1. Multicolinealidad (Factor de Inflación de la Varianza)
vif(modelo_mincer)

# 2. Homocedasticidad (Test de Breusch-Pagan)
bptest(modelo_mincer)

# 3. No Autocorrelación Serial (Test de Durbin-Watson)
dwtest(modelo_mincer)

# 4. Normalidad de los Residuos (Test de Jarque-Bera)
jarque.bera.test(residuals(modelo_mincer))

# ----------------------------------------------------------------------
# CORRECCIÓN: Errores Estándar Robustos a Heterocedasticidad (White/HC1)
# ----------------------------------------------------------------------
coeftest(modelo_mincer, vcov = vcovHC(modelo_mincer, type = "HC1"))

# ==============================================================================
# Graficos - Modelo Mincer (Wage1)
# ==============================================================================

library(wooldridge)
data("wage1")

# Modelo econometrico (modelo_mincer)
modelo_mincer <- lm(lwage ~ educ + exper + tenure, data = wage1)

# ------------------------------------------------------------------------------
# BLOQUE 1: ANÁLISIS EXPLORATORIO Y DISPERSIÓN DE CAPITAL HUMANO 
# ------------------------------------------------------------------------------

if(!is.null(dev.list())) dev.off()

# --- Gráfico 1: Salario en Nivel ---
hist(wage1$wage, 
     breaks = 20,
     main = "1. Distribución del Salario\nen Nivel (Wage)", 
     xlab = "Salario por hora (USD)", 
     ylab = "Frecuencia",
     col = "lightblue", border = "white",
     cex.main = 0.9)

# --- Gráfico 2: Salario en Logaritmo ---
hist(wage1$lwage, 
     main = "2. Distribución del Log(Salario)\n(lwage)", 
     xlab = "Logaritmo del salario", 
     ylab = "Frecuencia",
     col = "lightgreen", border = "white",
     cex.main = 0.9)

# --- Gráfico 3: Educación vs Log(Wage) ---
plot(wage1$educ, wage1$lwage, 
     main = "3. Log(Wage) vs. Educación", 
     xlab = "Años de Educación (educ)", 
     ylab = "Logaritmo del Salario (lwage)", 
     col = "darkgreen", pch = 19,
     cex.main = 0.9)
abline(lm(lwage ~ educ, data = wage1), col = "red", lwd = 2)

# --- Gráfico 4: Experiencia vs Log(Wage) ---
plot(wage1$exper, wage1$lwage, 
     main = "4. Log(Wage) vs. Experiencia", 
     xlab = "Años de Experiencia (exper)", 
     ylab = "Logaritmo del Salario (lwage)", 
     col = "darkorange", pch = 19,
     cex.main = 0.9)
abline(lm(lwage ~ exper, data = wage1), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# BLOQUE 2: DIAGNÓSTICO Y SUPUESTOS SOBRE LOS RESIDUOS
# ------------------------------------------------------------------------------

if(!is.null(dev.list())) dev.off()

# --- Gráfico 5: Antigüedad vs Log(Wage) ---
plot(wage1$tenure, wage1$lwage, 
     main = "5. Log(Wage) vs. Antigüedad", 
     xlab = "Años de Antigüedad (tenure)", 
     ylab = "Logaritmo del Salario (lwage)", 
     col = "purple", pch = 19,
     cex.main = 0.9)
abline(lm(lwage ~ tenure, data = wage1), col = "red", lwd = 2)

# --- Gráfico 6: Histograma de Residuos ---
hist(residuals(modelo_mincer), 
     main = "6. Distribución de Residuos\n(Prueba Jarque-Bera)", 
     xlab = "Residuos del modelo (u)", 
     ylab = "Densidad", 
     col = "steelblue", border = "white", probability = TRUE,
     cex.main = 0.9)
lines(density(residuals(modelo_mincer)), col = "darkred", lwd = 2)

# --- Gráfico 7: Q-Q Plot de Normalidad ---
qqnorm(residuals(modelo_mincer), 
       main = "7. Q-Q Plot de Residuos\n(Normalidad Asintótica)", 
       xlab = "Cuantiles Teóricos", 
       ylab = "Cuantiles Muestrales", 
       col = "darkblue", pch = 19,
       cex.main = 0.9)
qqline(residuals(modelo_mincer), col = "red", lwd = 2)

# --- Gráfico 8: Residuos vs Valores Ajustados (Heterocedasticidad) ---
plot(fitted(modelo_mincer), residuals(modelo_mincer),
     main = "8. Residuos vs. Valores Ajustados\n(Prueba Breusch-Pagan)",
     xlab = "Salario Estimado (Valores Ajustados)",
     ylab = "Residuos (u)",
     pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5),
     cex.main = 0.9)
abline(h = 0, col = "red", lty = 2, lwd = 2)

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# BLOQUE ADICIONAL
# ------------------------------------------------------------------------------

if(!is.null(dev.list())) dev.off()

# 1. Definir años a evaluar (0 a 30)
anos <- 0:30

# 2. Extraer coeficientes del modelo Mincer
b_educ   <- coef(modelo_mincer)["educ"]
b_tenure <- coef(modelo_mincer)["tenure"]
b_exper  <- coef(modelo_mincer)["exper"]

# 3. Calcular efectos acumulados lineales
efecto_educ   <- b_educ * anos
efecto_tenure <- b_tenure * anos
efecto_exper  <- b_exper * anos

# 4. --- Gráfico 9: Comparación de Retornos Marginales
plot(anos, efecto_educ, type = "l", col = "red", lwd = 3,
     main = "Comparación de Retornos Marginales al Capital Humano",
     xlab = "Años Adicionales de Capital Humano",
     ylab = "Efecto Acumulado sobre Log(Salario)",
     ylim = c(0, max(efecto_educ)))

lines(anos, efecto_tenure, col = "darkgreen", lwd = 3, lty = 2)
lines(anos, efecto_exper,  col = "blue",      lwd = 3, lty = 3)

legend("topleft", 
       legend = c("Educación (educ)", "Antigüedad (tenure)", "Experiencia (exper)"),
       col = c("red", "darkgreen", "blue"), 
       lwd = 3, lty = c(1, 2, 3), bty = "n")