########################################################################
######                                                            ######
#                      UNIVERSIDAD DEL QUINDÍO                         #
#                       PROGRAMA DE ECONOMÍA                           #
#                           ECONOMETRÍA II                             #
######                                                            ######
########################################################################

# By: Iván Cristóbal Másmela Castro
# icmasmelac@uqvirtual.edu.co
# +57 310 820 6970


library(dynlm)

library(vars)

library(tseries)

library(FinTS)

D = read.csv("C:/Users/LENOVO/Documents/Eco-UQ/Semestre/Econometria II/Actividades/IPC_Índice de Precios al Consumidor.csv",
             header = TRUE, sep = ";", dec = ",", fileEncoding = "UTF-8-BOM")

head(D)
str(D)
skimr::skim(D)

fecha <- as.Date(D[, 1], format = "%Y/%m/%d")
ipc   <- D[, 2]
IPC   <- ipc[fecha >= as.Date("2000-01-01")]


LNIPC <- log(IPC)                                 # 1. logaritmo de la serie
LNIPCT <- ts(LNIPC, start=c(2000,1), frequency=12) # 2. convertir a serie de tiempo

start(LNIPCT)
end(LNIPCT)
length(LNIPCT)

plot(LNIPCT,
     main = "Logaritmo del IPC de Colombia (enero 2000 - septiembre 2026)",
     xlab = "Año",
     ylab = "Ln(IPC)")   
  
summary(LNIPCT)
exp(3.696)
exp(5.081)
sd(LNIPCT)
  
acf(LNIPCT, lag.max = 36, main = "ACF del ln(IPC)")
pacf(LNIPCT, lag.max = 36, main = "PACF del ln(IPC)")
  
D.LNIPCT <- diff(LNIPCT, 1)

monthplot(D.LNIPCT,
          main = "Inflación mensual por mes del año (ln IPC diferenciado)",
          ylab = "Variación mensual del ln(IPC)",
          xlab = "Mes")

aggregate(D.LNIPCT, nfrequency = 1, FUN = mean)
plot(aggregate(D.LNIPCT, nfrequency = 1, FUN = mean), type = "b",
     main = "Inflación mensual promedio por tramo de 12 meses",
     xlab = "Año", ylab = "Promedio de D.ln(IPC)")


# 1. Sin constante, sin tendencia
DF.SC.ST <- dynlm(d(LNIPCT,1) ~ L(LNIPCT,1) - 1)
summary(DF.SC.ST)

# 2. Con constante, sin tendencia
DF.CC.ST <- dynlm(d(LNIPCT,1) ~ L(LNIPCT,1))
summary(DF.CC.ST)

# 3. Con constante y tendencia
DF.CC.CT <- dynlm(d(LNIPCT,1) ~ L(LNIPCT,1) + trend(LNIPCT))
summary(DF.CC.CT)

adf.test(LNIPCT)

pp.test(LNIPCT)

kpss.test(LNIPCT)

D.LNIPCT <- diff(LNIPCT, 1)

DF.CC.ST.D <- dynlm(d(D.LNIPCT,1) ~ L(D.LNIPCT,1))
summary(DF.CC.ST.D)

DF.CC.CT.D <- dynlm(d(D.LNIPCT,1) ~ L(D.LNIPCT,1) + trend(D.LNIPCT))
summary(DF.CC.CT.D)

adf.test(D.LNIPCT)

pp.test(D.LNIPCT)

kpss.test(D.LNIPCT)



D.LNIPCT<-diff(LNIPCT,1)

plot(D.LNIPCT, main = "Primera diferencia del Ln (IPC)", ylab = "D.Ln(IPC)")

acf(D.LNIPCT, lag.max = 36, main = "ACF de la primera diferencia del Ln (IPC)")
acf(as.numeric(D.LNIPCT), lag.max = 36, plot = FALSE)

pacf(D.LNIPCT, lag.max = 36, main = "PACF de la primera diferencia del Ln (IPC)")
pacf(as.numeric(D.LNIPCT), lag.max = 36, plot = FALSE)

AR2.I1.MA0 <- arma(D.LNIPCT, order = c(2, 0))    # ARIMA(2,1,0)
summary(AR2.I1.MA0)

AR1.I1.MA1 <- arma(D.LNIPCT, order = c(1, 1))    # ARIMA(1,1,1)
summary(AR1.I1.MA1)

AR2.I1.MA1 <- arma(D.LNIPCT, order = c(2, 1))    # ARIMA(2,1,1)
summary(AR2.I1.MA1)


M.210 <- arima(D.LNIPCT, order = c(2, 0, 0))
M.111 <- arima(D.LNIPCT, order = c(1, 0, 1))
M.211 <- arima(D.LNIPCT, order = c(2, 0, 1))

AIC(M.210, M.111, M.211)
BIC(M.210, M.111, M.211)

logLik(M.210)
logLik(M.111)
logLik(M.211)

M.210$sigma2
M.111$sigma2
M.211$sigma2

n       <- length(D.LNIPCT)
entrena <- ts(D.LNIPCT[1:(n - 12)], start = start(D.LNIPCT), frequency = 12)
prueba  <- D.LNIPCT[(n - 11):n]

length(entrena)
length(prueba)

rmse <- function(orden) {
  ajuste <- arima(entrena, order = orden)
  pred   <- predict(ajuste, n.ahead = 12)$pred
  sqrt(mean((prueba - as.numeric(pred))^2))
}

rmse(c(2, 0, 0))
rmse(c(1, 0, 1))
rmse(c(2, 0, 1))

IARMA <- residuals(AR2.I1.MA1)
IARMA.NA <- na.omit(IARMA)

head(IARMA)
length(IARMA.NA)

Box.test(IARMA.NA, lag = 12, type = "Ljung-Box", fitdf = 3)
Box.test(IARMA.NA, lag = 24, type = "Ljung-Box", fitdf = 3)

jarque.bera.test(IARMA.NA)

qqnorm(IARMA.NA, main = "Q-Q de los residuos del ARIMA(2,1,1)")
qqline(IARMA.NA)

ArchTest(IARMA.NA, lags = 12)






