library(randomForest)
library(readxl)
library(lubridate)
library(dplyr)

#Cargue y visualizacion de los datos
dat <- read_xlsx("C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarpremium1.xlsx")

library(sqldf)
trial <- sqldf("select * from dat limit 428112")
dat <- dat %>% anti_join(trial)

glimpse(dat)

#Transformacion de las variables
dat$class = as.factor(dat$class)
dat$year = lubridate::year(dat$Fecha)
dat$yday = yday(dat$Fecha)
dat$quarter = quarter(dat$Fecha)
dat$month = lubridate::month(dat$Fecha)
dat$weekday = weekdays(dat$Fecha)
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

train <- sqldf("select * from test limit 68737")
test <- dat %>% anti_join(train)

#Funcion de evaluacion del modelo
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

glimpse(test)

#Random Forest
dat$Valorlluvia
set.seed(100)

modelo3 <- randomForest(Ocupacion ~ Valorlluvia + year + yday + quarter + month + weekday + day + hor + min , data = train, importance = TRUE, ntree = 58)

predictions = predict(modelo3, newdata = test)
predictions

summary(predictions)
plot(modelo3)
print(modelo3)

write.table(predictions,"C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Proceso/Mserie/Full Hogar/Fullhogarforecast3.csv", sep = ";", dec = ",")

