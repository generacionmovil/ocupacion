library(readxl)
library(lubridate)
library(dplyr)

#Cargue y visualizacion de los datos
dat <- read_xlsx("C:/Users/Daniel/OneDrive - Pontificia Universidad Javeriana/Attachments/Maestria/Tesis/Consolidado General 11_2018-06_2020.xlsx")

dat <- dat %>% select(Codigo.Zona,Tipo.vehículo,Fecha.ingreso,Fecha.salida)

#dat <- dat %>% sample_frac(0.05)

glimpse(dat)

dat$Totalsegundos=dat$Fecha.ingreso-dat$Fecha.salida
dat <- dat %>% select(-Fecha.salida)

#Transformacion de las variables
dat$year = lubridate::year(dat$Fecha.ingreso)
dat$yday = yday(dat$Fecha.ingreso)
dat$quarter = quarter(dat$Fecha.ingreso)
dat$month = lubridate::month(dat$Fecha.ingreso)
dat <- dat %>% mutate(weekday=wday(dat$Fecha.ingreso,week_start = getOption("lubridate.week.start", 7)))
dat$day = lubridate::day(dat$Fecha.ingreso)
dat$hor = lubridate::hour(dat$Fecha.ingreso)
dat$min = lubridate::minute(dat$Fecha.ingreso)
glimpse(dat)

library(caret)

datb <- dummyVars("~.", dat)
datt <- as.data.frame(predict(datb, newdata = dat))

glimpse(datt)

datt <- datt %>% select(-Tipo.vehículoMotos,-Codigo.ZonaZONA.PRUEBA,-Fecha.ingreso)

train <- datt %>% sample_frac(0.8)
test <- datt %>% anti_join(train)

#Regresion lineal
modelo1 <- lm(data = train, formula = Totalsegundos ~ Codigo.ZonaCASA.DE.LA.CULTURA + Codigo.ZonaCASA.DE.LA.CULTURA.2+
                Codigo.ZonaCENSA+`Codigo.ZonaDOMICILIOS/CARGUE`+Codigo.ZonaED..SAN.FRANCISCO+Codigo.ZonaEDIF..LATINO+
                `Codigo.ZonaEL.ABUELO.Y.SON.&.SORBO`+Codigo.ZonaEL.CARRUSEL+Codigo.ZonaFULLHOGAR+Codigo.ZonaLA.CANOA+
                Codigo.ZonaLA.ESTACION+Codigo.ZonaMI.VAQUITA+Codigo.ZonaNOTARIA.UNICA+Codigo.ZonaOLAYA.HERRERA.CALDAS+
                Codigo.ZonaOLAYA.HERRERA.Y.SAN.FRANCISCO+Codigo.ZonaOPTICA.BOSTON+`Codigo.ZonaPACHES-CASONA`+Codigo.ZonaPARQUE.CALDAS+
                Codigo.ZonaTORRE.OASIS+Tipo.vehículoAutomoviles + year + yday + quarter + month +weekday + day + hor + min)
summary(modelo1)
library(forecast)
accuracy(modelo1)

modelo.rm <- step(modelo1, newdata = testt)
summary(modelo.rm)