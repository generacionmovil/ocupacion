library(readxl)
library(lubridate)
library(dplyr)

#Cargue y visualizacion de los datos
dat <- read_xlsx("C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarpremium.xlsx")

glimpse(dat)

#Transformacion de las variables
dat$class = as.factor(dat$class)
dat$year = lubridate::year(dat$Fecha)
dat$yday = yday(dat$Fecha)
dat$quarter = quarter(dat$Fecha)
dat$month = lubridate::month(dat$Fecha)
dat <- dat %>% mutate(weekday=wday(dat$Fecha,week_start = getOption("lubridate.week.start", 7)))
dat$day = lubridate::day(dat$Fecha)
dat$hor = lubridate::hour(dat$Fecha)
dat$min = lubridate::minute(dat$Fecha)
glimpse(dat)

dat$month = as.factor(dat$month)
dat$year = as.factor(dat$year)
dat$quarter = as.factor(dat$quarter)
dat$weekday = as.factor(dat$weekday)
dat$hor = as.factor(dat$hor)
dat <- dat %>% select(-Fecha)


#particion de la base en validacion y prueba
set.seed(100)
train = dat[dat$class == 'Train',]
test = dat[dat$class == 'Test',]

train <- train %>% select(-class)
test <- test %>% select(-class)

library(caret)

traind <- dummyVars("~.", train)
traintt <- as.data.frame(predict(traind, newdata = train))

testd <- dummyVars("~.", test)
testt <- as.data.frame(predict(testd, newdata = test))

#Funcion de evaluacion del modelo
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

glimpse(testt)

#Borrando dummys para evitar problemas de multicolinealidad
traintt <- traintt %>% select(-year.2018, -quarter.1, -month.12, -weekday.5, -hor.7)
testt <- testt %>% select(-year.2018, -quarter.1, -month.12, -weekday.5, -hor.7)

#Regresion lineal
modelo1 <- lm(data = traintt, formula = Ocupacion ~ year.2019 + year.2020 + yday 
              + quarter.2 + quarter.3 + quarter.4 + month.1 + month.2 + month.3
              + month.4 + month.5 + month.6 + month.7 + month.8 + month.9 +
                month.10 + month.11 + weekday.1 + weekday.2 + weekday.3 + weekday.4
              + weekday.6 + weekday.7 + day + hor.0 + hor.1 + hor.3 + hor.4 +hor.5
              + hor.6 + hor.8 + hor.9 + hor.10 + hor.11 + hor.12 + hor.13 + hor.14
              + hor.15 + hor.16 + hor.17 + hor.18 + hor.19 + hor.20 + hor.21 +
                hor.22 + hor.23 + min)
summary(modelo1)
library(forecast)
accuracy(modelo1)

modelo.rm <- step(modelo1, newdata = testt)
summary(modelo.rm)

forecast(modelo1, newdata = testt, h=25600)
autoplot(forecast(modelo1, newdata = testt, h=1440))
accuracy(modelo.rm)

write.table(forecast(modelo1, newdata = testt, h=25600),"C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarforecast.csv", sep = ";", dec = ",")
