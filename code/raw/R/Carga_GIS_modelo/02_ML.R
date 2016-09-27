## Creación de modelo de regresión lineal para estimar el precio de alquiler de una vivienda
## en Madrid en base a: tamaño, barrio, altura, garaje y año de construcción o reforma

## Bibliotecas




## Carga datos

clean.data.dir <- '../../../../data/clean/modelo'
file.fincas.ivima <- paste(clean.data.dir,  "/fincas_ivima.csv", sep = "")
file.fincas.cat <- paste(clean.data.dir,  "/fincas_catastro.csv", sep = "")

fincas.ivima <- read.csv(file = file.fincas.ivima, header = T,  encoding = 'UTF-8', stringsAsFactors = T)
fincas.catastro <- read.csv(file = file.fincas.cat, header = T,  encoding = 'UTF-8', stringsAsFactors = T)
fincas.catastro$idbarrio <- as.factor(fincas.catastro$idbarrio)
fincas.catastro$garage <- as.factor(fincas.catastro$garage)
colnames(fincas.catastro)[21] <- "Garaje"
colnames(fincas.catastro)[18] <- "metros"
colnames(fincas.catastro)[22] <- "anio_max"
colnames(fincas.catastro)[5] <- "altura_num"



str(fincas.ivima)
summary(fincas.ivima)

fincas.ivima$idbarrio <- as.factor(fincas.ivima$idbarrio)
fincas.ivima$Garaje <- as.factor(fincas.ivima$Garaje)
str(fincas.ivima)

modeloInd1=lm(Precio ~ metros, data = fincas.ivima)
summary(modeloInd1)

modeloInd2=lm(Precio ~ barrio, data = fincas.ivima)
summary(modeloInd2)

modeloInd3=lm(Precio ~ altura_num, data = fincas.ivima)
summary(modeloInd3)

modeloInd4=lm(Precio ~ anio_max, data = fincas.ivima)
summary(modeloInd4)

modeloInd5=lm(Preci5 ~ Garaje, data = fincas.ivima)
summary(modeloInd2)

modeloLmul=lm(Precio ~ metros+idbarrio+altura_num+anio_max+Garaje, data = fincas.ivima)
summary(modeloLmul)

## Limitamos los datos de test a los idbarrio del train

barrios.ivima <- as.data.frame(unique(fincas.ivima$idbarrio))
colnames(barrios.ivima)[1] <- 'idbarrio'

fincas.catastro <- merge(fincas.catastro, barrios.ivima, by.x = 'idbarrio', by.y = 'idbarrio')


fincas.catastro <- cbind(fincas.catastro, as.data.frame(predict.lm(modeloLmul, fincas.catastro)))
colnames(fincas.catastro)[26] <- 'precio.alquiler'

## Revisamos los datos estimados
summary(fincas.catastro)
fincas.catastro[fincas.catastro$precio.alquiler == max(fincas.catastro$precio.alquiler),]
fincas.catastro[fincas.catastro$precio.alquiler == min(fincas.catastro$precio.alquiler),]
fincas.catastro[fincas.catastro$metros == 0,]
