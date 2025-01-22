rm(list = ls())
set.seed(123)
library(forecast)
library(tseries)
library(datasets)
library(lmtest)
library(urca)

#Imposto il percorso del file 
file_path <- "/Users/Alessiospl/Il mio Drive/Università/Laboratorio Data Science/Tesina Serie storiche/STLAM.MI.csv"

Stellantis_data <- read.csv(file_path)
View(Stellantis_data)
#Converto la colonna Date in formato data 
Stellantis_data$Date <- as.Date(Stellantis_data$Date, format="%Y-%m-%d")

#Creo un data frame Stellantis con date e adj close
Stellantis <- data.frame(
  Date = Stellantis_data$Date,
  `Adj Close` = Stellantis_data$Adj.Close
)


# Grafico AdjClose
plot(Stellantis$Date, Stellantis$Adj.Close, type="l", col="blue",
     main="Prezzo di chiusura aggiustato 2021-2024",
     xlab="Date", ylab="Adj Close")

#Analisi dei correlogrammi
acf(Stellantis$Adj.Close)
pacf(Stellantis$Adj.Close)

#trasformazione logaritmica
LCloseStellantis<-log(Stellantis$Adj.Close)

#Grafico log AdjClose
plot(Stellantis$Date,LCloseStellantis,col=4,type = "l",main="Prezzo di chiusura aggiustato 2021-2024",
        xlab="Date", ylab ="Log Adj Close")

#Analisi correlogrammi log
acf(LCloseStellantis)
pacf(LCloseStellantis)


#Augmented Dickey Fuller
#Test regression "none"
ADF_test1 <- ur.df(LCloseStellantis, type = c("none"), lags=10, selectlags = c("AIC"))
summary(ADF_test1)
#Test regression "Drift"
ADF_test2 <- ur.df(LCloseStellantis, type = c("drift"), lags=10, selectlags = c("AIC"))
summary(ADF_test2)
#Test regression "Trend"
ADF_test3 <- ur.df(LCloseStellantis, type = c("trend"), lags=10, selectlags = c("AIC"))
summary(ADF_test3)

# Differenziazioni 

Diff_Stellantis = diff(LCloseStellantis, lag=1)

Diff_Stellantis_ADF1 <- ur.df(Diff_Stellantis, type = c("none"), lags=10, selectlags = c("AIC"))
summary(Diff_Stellantis_ADF1)

Diff_Stellantis_ADF2 <- ur.df(Diff_Stellantis, type = c("drift"), lags=10, selectlags = c("AIC"))
summary(Diff_Stellantis_ADF2)

Diff_Stellantis_ADF3 <- ur.df(Diff_Stellantis, type = c("trend"), lags=10, selectlags = c("AIC"))
summary(Diff_Stellantis_ADF3)

#Stima del modello
# Verifico la lunghezza dei vettori
length(Diff_Stellantis)
length(Stellantis$Date)

# Creare il grafico
plot(Stellantis$Date[-1],Diff_Stellantis, type="l", col="blue",
     main="Prezzo di chiusura aggiustato 2021-2024",
     xlab="Date", ylab="Adj Close")
par(mfrow = c(1, 2))
acf(Diff_Stellantis)
pacf(Diff_Stellantis)


#stimo il modello arima(0,1,1)
arima_model1 <- Arima(LCloseStellantis, order = c(0,1,1), include.constant = TRUE)
summary(arima_model1)
coeftest(arima_model1)

#stimo il modello arima(1,1,0)
arima_model2 <- Arima(LCloseStellantis, order = c(1,1,0), include.constant = TRUE)
summary(arima_model2)
coeftest(arima_model2)

#stimo il modello arima(1,1,1)
arima_model3 <- Arima(LCloseStellantis, order = c(1,1,1), include.constant = TRUE)
summary(arima_model3)
coeftest(arima_model3)

# Confronto degli AIC
AIC_values <- data.frame(
  Model = c("ARIMA(0,1,1)", "ARIMA(1,1,0)", "ARIMA(1,1,1)"),
  AIC = c(AIC(arima_model1), AIC(arima_model2), AIC(arima_model3))
)

# Ordinamento degli AIC in modo crescente
AIC_values_sorted <- AIC_values[order(AIC_values$AIC), ]
print(AIC_values_sorted)

##############################################################################
#MODELLO ARIMA(1,1,1)
par(mfrow = c(1, 1))

residui_arima_model3 <- arima_model3$residuals
plot(Stellantis$Date, residui_arima_model3, type="l", xlab="Date", ylab="Residui", main="Grafico dei Residui ARIMA (1,1,1)")
abline(h=0, col= "red")


par(mfrow = c(1, 2))
acf(residui_arima_model3, main = "ACF Residui ARIMA (1,1,1)", na.action = na.pass)
pacf(residui_arima_model3, main = "PACF Residui ARIMA (1,1,1)", na.action = na.pass)


par(mfrow = c(1, 1))
# Creare l'istogramma 
hist(residui_arima_model3,
     breaks = 30,  # Numero di bins
     prob = TRUE,  
     col = "skyblue",  
     border = "black", 
     xlab = "Residui",  
     ylab = "Densità", 
     main = "Istogramma Residui ARIMA (1,1,1)",  
     xlim = range(residui_arima_model3)  # Limiti dell'asse x
)

# Aggiungere una linea di densità
lines(density(residui_arima_model3), col = "red", lwd = 2)

# Aggiungere una griglia per migliorare la leggibilità
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Aggiungere un box intorno al grafico
box()


#Test normalità dei residui
jarque.bera.test(residui_arima_model3) 

#Test di indipendenza di Box-Pierce e Ljung-Box 
#(mancanza di autocorrelazione fino a lag = k)
Box.test (residui_arima_model3, lag = 8, type =c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model3, lag = 8, type = c("Ljung-Box"), fitdf = 1) 

#Test di indipendenza di Box-Pierce e Ljung-Box sui residui al quadrato
Box.test (residui_arima_model3*residui_arima_model3, lag = 8, type = c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model3*residui_arima_model3, lag = 8, type = c("Ljung-Box"), fitdf = 1)
############################################################################################
#MODELLO ARIMA(0,1,1) 
residui_arima_model1 <- arima_model1$residuals
plot(Stellantis$Date, residui_arima_model1, type="l", xlab="Date", ylab="Residui", main="Grafico dei Residui ARIMA (0,1,1)")
abline(h=0, col= "red")


par(mfrow = c(1, 2))
acf(residui_arima_model1, main = "ACF Residui ARIMA (0,1,1)", na.action = na.pass)
pacf(residui_arima_model1, main = "PACF Residui ARIMA (0,1,1)", na.action = na.pass)

# Creare l'istogramma 
hist(residui_arima_model1,
     breaks = 30,  # Numero di bins
     prob = TRUE,  
     col = "skyblue",  
     border = "black",  # Colore dei bordi delle barre
     xlab = "Residui",  
     ylab = "Densità",  
     main = "Istogramma Residui ARIMA (0,1,1)",  
     xlim = range(residui_arima_model1)  # Limiti dell'asse x
)

# Aggiungere una linea di densità
lines(density(residui_arima_model1), col = "red", lwd = 2)

# Aggiungere una griglia per migliorare la leggibilità
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Aggiungere un box intorno al grafico
box()


#Test normalità dei residui
jarque.bera.test(residui_arima_model1) 

#Test di indipendenza di Box-Pierce e Ljung-Box 
#(mancanza di autocorrelazione fino a lag = k)
Box.test (residui_arima_model1, lag = 8, type =c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model1, lag = 8, type = c("Ljung-Box"), fitdf = 1) 

#Test di indipendenza di Box-Pierce e Ljung-Box sui residui al quadrato
Box.test (residui_arima_model1*residui_arima_model1, lag = 8, type = c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model1*residui_arima_model1, lag = 8, type = c("Ljung-Box"), fitdf = 1)

############################################################################################################
#Modello ARIMA (1,1,0)

residui_arima_model2 <- arima_model2$residuals
plot(Stellantis$Date, residui_arima_model2, type="l", xlab="Date", ylab="Residui", main="Grafico dei Residui ARIMA (1,1,0)")
abline(h=0, col= "red")


par(mfrow = c(1, 2))
acf(residui_arima_model2, main = "ACF Residui ARIMA (1,1,0)", na.action = na.pass)
pacf(residui_arima_model2, main = "PACF Residui ARIMA (1,1,0)", na.action = na.pass)

# Creare l'istogramma 
hist(residui_arima_model2,
     breaks = 30,  # Numero di bins
     prob = TRUE,  # Mostrare densità invece di frequenza
     col = "skyblue",  # Colore delle barre
     border = "black",  # Colore dei bordi delle barre
     xlab = "Residui",  
     ylab = "Densità",  
     main = "Istogramma Residui ARIMA (1,1,0)",  
     xlim = range(residui_arima_model2)  # Limiti dell'asse x
)

# Aggiungere una linea di densità
lines(density(residui_arima_model2), col = "red", lwd = 2)

# Aggiungere una griglia per migliorare la leggibilità
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Aggiungere un box intorno al grafico
box()


#Test normalità dei residui
jarque.bera.test(residui_arima_model2)
#Test di indipendenza di Box-Pierce e Ljung-Box 
#(mancanza di autocorrelazione fino a lag = k)
Box.test (residui_arima_model2, lag = 8, type =c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model2, lag = 8, type = c("Ljung-Box"), fitdf = 1) 

#Test di indipendenza di Box-Pierce e Ljung-Box sui residui al quadrato
Box.test (residui_arima_model2*residui_arima_model2, lag = 8, type = c("Box-Pierce"), fitdf = 1)
Box.test (residui_arima_model2*residui_arima_model2, lag = 8, type = c("Ljung-Box"), fitdf = 1)


################################################################################
#PREVISIONI
#Previsione stimo togliendo gli ultimi 88 dati (10% campione 2021-2024)
model.prev1<- Arima(window(LCloseStellantis,end=794),order=c(1,1,1), include.constant = TRUE)
summary(model.prev1)
coeftest(model.prev1)

# Previsione a 88 passi
forecast1  <- forecast(model.prev1,h=88)
par(mfrow = c(1, 1))
plot(forecast(model.prev1, h = 88),main="Previsioni con Modello ARIMA (1,1,1)")
print(forecast(model.prev1,h=88))
lines(LCloseStellantis)
# Aggiungi una linea verticale rossa per separare le osservazioni dalle previsioni
abline(v = length(LCloseStellantis) - 88, col = "red", lty = "dashed")

matrix_excel <- cbind(forecast1$mean,forecast1$lower,forecast1$upper,LCloseStellantis[795:882])

write.csv(matrix_excel, file = "fore.csv")

matrix_excel <- read.csv("fore.csv")
head(matrix_excel)
View(matrix_excel)

# out-of-sample multi-step forecasts
accuracy(forecast(model.prev1,h=88),
         (window(LCloseStellantis,start=795)))

# Modello ARCH
r2 <- Diff_Stellantis^2
archmodel <- arima(x = r2, order = c(1, 0, 0), method = "CSS")

summary(archmodel)
coeftest(archmodel)

#residui archx
residuals_arch <- residuals(archmodel)

#Calcolare l'autocorrelazione dei residui
acf_resid_arch <- acf(residuals_arch, plot = FALSE)
plot(acf_resid_arch)

# Modello GARCH
GARCH_1 <- garch(x = Diff_Stellantis, order = c(1, 1, 1))
summary(GARCH_1)
coeftest(GARCH_1)

residuals_garch <- GARCH_1$residuals

#Controllare i valori mancanti nei residui
sum(is.na(residuals_garch))

#Rimuovere i valori mancanti dai residui
residuals_garch <- residuals_garch[!is.na(residuals_garch)]

#Calcolare l'autocorrelazione dei residui senza plottare
acf_resid_garch <- acf(residuals_garch, plot = FALSE)
plot(acf_resid_garch)

#Test di Ljung-Box e Box-Pierce per verificare l'autocorrelazione dei residui al quadrato
Box.test(residuals_garch^2, lag = 4, type = c("Ljung-Box"), fitdf = 2)
Box.test(residuals_garch^2, lag = 4, type = c("Box-Pierce"), fitdf = 2)










