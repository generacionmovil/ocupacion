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

#dat$month = as.factor(dat$month)
#dat$year = as.factor(dat$year)
#dat$quarter = as.factor(dat$quarter)
#dat$weekday = as.factor(dat$weekday)
#dat$hor = as.factor(dat$hor)
dat <- dat %>% select(-Fecha)


#particion de la base en validacion y prueba
set.seed(100)
train = dat[dat$class == 'Train',]
test = dat[dat$class == 'Test',]

train <- train %>% select(-class)
test <- test %>% select(-class)

normalize <- function(x){
  return((x-min(x))/max(x)-min(x))
}

train= normalize(train)
test=normalize(test)

#Funcion de evaluacion del modelo
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#Neural Net
library(neuralnet)
set.seed(100)

modelo4 <- neuralnet(Ocupacion ~ year + yday + quarter + month + weekday + day + hor + min , data = train, stepmax = 50000, hidden = c(3,3,3,3))
 predic <- compute(modelo4, test)
plot(modelo4)
modelo4$result.matrix

accuracy(predic)
