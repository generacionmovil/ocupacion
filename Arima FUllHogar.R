#Cargamos la Base de datos
library(readxl)
Fullhogar <- read_xlsx("C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarpremium.xlsx")

#Borramos las variables que no vamos a utilizar
library(dplyr)
Fullhogar <- Fullhogar %>% select(-`Id Registro`,-`Codigo Zona`,-Placa,-`Tipo vehículo`,-Celda)

#Agregamos el dia de la semana al que corresponde la fecha
library(lubridate)
Fullhogar <- Fullhogar %>% mutate(DiaSemana=wday(Fullhogar$Fecha,week_start = getOption("lubridate.week.start", 7)))


#Observamos el tipo de las variables, tal cual como las reconoce R y sus posibles sesgos
str(Fullhogar)
#library(psych)
#write.csv(describe(Fullhogar),"C:/Users/danie/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/MFullhogar (72)/describe.csv", sep = ";",dec = ",")

#Cargamos las librerias que usaremos para el (S)ARIMA
library(forecast)
library(tseries)
library(fpp)

#Construimos para la serie un objeto tipo serie
producto1 <- ts(Fullhogar$Ocupacin, start=c(2018, 1), end=c(2020, 7), frequency=525600)

#Contruimos la Base de Entrenamiento y prueba
producto1.train <- window(producto1, start=c(2018, 1), end=c(2019, 400114))
producto1.test <- window(producto1, start=c(2019, 400115), end=c(2020, 400114))

#Graficamosel historico de ocupacion, estacionariedad y estacionalidad
plot(producto1.train)
plot(diff(producto1.train, differences=1440))
plot(diff(producto1.train, lag=400114, differences=1))

#Creamos el mejor modelo
set.seed(10)
modelo2<-auto.arima(producto1.train)
summary(modelo2)

#Validacion dle mododelo con el test
modeloar.test<-Arima(producto1.test, model=modelo2, use.initial.values=TRUE)
accuracy(modeloar.test)

#Realizacion de las pruebas de significancia del modelo
adf.test(residuals(modelo2))
Box.test(residuals(modelo2), lag=1, type="Ljung-Box")
?Box.test

autoplot(forecast(modelo2, h=25600))
forecast(modelo2, h=25600)

write.table(forecast(modelo2, h=25600),"C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarforecast2.csv", sep = ";", dec = ",")
