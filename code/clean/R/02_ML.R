## Creación de modelo de regresión lineal para estimar el precio de alquiler de una vivienda
## en Madrid en base a: tamaño, barrio, altura, garaje y año de construcción o reforma

## Bibliotecas
library(stringr)
library(data.table)
library(caret)
library(caTools)


## Carga datos

clean.data.dir <- '../../../data/clean/modelo'
out.data.dir <- '../../../reports'
file.fincas.ivima <- paste(clean.data.dir,  "/fincas_ivima.csv", sep = "")
file.fincas.cat <- paste(clean.data.dir,  "/fincas_catastro.csv", sep = "")
file.fincas.catastro.out <- paste(out.data.dir,  "/fincas_catastro_est_alquiler.csv", sep = "")
file.barrios.out <- paste(out.data.dir,  "/barrios_alquiler.csv", sep = "")

fincas.ivima <- read.csv(file = file.fincas.ivima, header = T,  encoding = 'UTF-8', stringsAsFactors = T)
fincas.catastro <- as.data.table(read.csv(file = file.fincas.cat, header = T,  encoding = 'UTF-8', stringsAsFactors = T))
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

## eliminamos los atípicos de superficie y altura de fincas catastro para homogeneizar los límites en el dashboard
fincas.catastro <- fincas.catastro[metros>0]

fincas.catastro[, lim.sup.m := exp(mean(log(metros))+4*sd(log(metros))), by=idbarrio]
fincas.catastro[, lim.inf.m := exp(mean(log(metros))-4*sd(log(metros))), by=idbarrio]

fincas.catastro <- fincas.catastro[metros <= lim.sup.m & metros >= lim.inf.m]

## en altura eliminamos aquellas fincas con altura menor que -1 ya que sólo consideramos viviendas

fincas.catastro <- fincas.catastro[altura_num >= -1]



modeloInd1=lm(Precio ~ metros, data = fincas.ivima)
summary(modeloInd1)

modeloInd2=lm(Precio ~ barrio, data = fincas.ivima)
summary(modeloInd2)

modeloInd3=lm(Precio ~ altura_num, data = fincas.ivima)
summary(modeloInd3)

modeloInd4=lm(Precio ~ anio_max, data = fincas.ivima)
summary(modeloInd4)

modeloInd5=lm(Precio ~ Garaje, data = fincas.ivima)
summary(modeloInd2)

## No puedo usar validación cruzada con el paquete caret ya que hay barrios con muy pocas fincas
## y es posible que en alguna selección elimine uno de estos barrios. Definimos manualmente los
## conjuntos de train y test

set.seed(1234)
muestra <-  sample.split(fincas.ivima$Precio, SplitRatio = .80)
fincas.ivima.train <- subset(fincas.ivima, muestra == TRUE)
fincas.ivima.test <-  subset(fincas.ivima, muestra == FALSE)

# train the model
## Poniendo interacción metros*idbarrio aparecen alquileres negativos aunque mejora el RSE y el R2
modeloLmul <- lm(Precio ~ metros+idbarrio+Garaje+altura_num+anio_max, data = fincas.ivima.train)
summary(modeloLmul)
plot(modeloLmul$residuals)
hist(modeloLmul$residuals)
qqnorm(modeloLmul$residuals); qqline(modeloLmul$residuals,col=2)
qqnorm(fincas.ivima.train$Precio)
confint(modeloLmul,level=0.95)

# RSE en fincas test. Hay que limitar las fincas de test a los datos de barrio de train
barrios.train <- as.data.frame(unique(fincas.ivima.train$idbarrio))
colnames(barrios.train) <- c("idbarrio")
fincas.ivima.test <- merge(fincas.ivima.test, barrios.train, by.x = "idbarrio", by.y = "idbarrio")
rmse.0 <- sqrt(mean((predict.lm(modeloLmul, fincas.ivima.test) - fincas.ivima.test$Precio)^2))

## modelo para la predicción sin tener el cuenta el idbarrio. Para completar aquellas fincas de catastro
## en las que no tenemos datos de barrio. Toda finca de catastro tendrá un valor estimado de alquiler

modeloLmul.nobarrio=lm(Precio ~ metros+altura_num+Garaje+anio_max, data = fincas.ivima.train)
summary(modeloLmul.nobarrio)
plot(modeloLmul.nobarrio$residuals)
hist(modeloLmul.nobarrio$residuals)
qqnorm(modeloLmul.nobarrio$residuals); qqline(modeloLmul.nobarrio$residuals,col=2)

confint(modeloLmul.nobarrio,level=0.95)

# RSE en fincas test. Hay que limitar las fincas de test a los datos de barrio de train
barrios.train <- as.data.frame(unique(fincas.ivima.train$idbarrio))
colnames(barrios.train) <- c("idbarrio")
fincas.ivima.test <-  subset(fincas.ivima, muestra == FALSE)
fincas.ivima.test <- merge(fincas.ivima.test, barrios.train, by.x = "idbarrio", by.y = "idbarrio")
rmse.1 <- sqrt(mean((predict.lm(modeloLmul.nobarrio, fincas.ivima.test) - fincas.ivima.test$Precio)^2))


## Modelo KNN con validación cruzada

set.seed(1234)

# train the model
train.control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = 1:20)
modeloKNN <- train(Precio ~ metros+idbarrio+Garaje+altura_num+anio_max, data = fincas.ivima, trControl = train.control, method = "knn", tuneGrid = grid)
print(modeloKNN)

## El modelo KNN da peores resultados que el modelo lineal multiple con interacción

## Aplicamos cada modelo al conjunto de datos de catastro adecuado

barrios.ivima <- barrios.train
colnames(barrios.ivima) <- c("idbarrio")
barrios.ivima$fuente <- 'I'
barrios.catastro <- as.data.frame(unique(fincas.catastro$idbarrio))
colnames(barrios.catastro) <- c("idbarrio")
barrios.catastro$fuente <- 'C'
barrios.total <- merge(barrios.catastro, barrios.ivima, by.x = 'idbarrio', by.y = 'idbarrio', all.x = T, all.y=T)
barrios.cat.no.ivima <- barrios.total[is.na(barrios.total$fuente.y) == T,]
barrios.ivima$fuente <- NULL
barrios.cat.no.ivima <- as.data.frame(barrios.cat.no.ivima$idbarrio)
colnames(barrios.cat.no.ivima) <- c("idbarrio")
fincas.catastro.otros.barrios <- merge(fincas.catastro, barrios.cat.no.ivima, by.x = 'idbarrio', by.y = 'idbarrio')
fincas.catastro <- merge(fincas.catastro, barrios.ivima, by.x = 'idbarrio', by.y = 'idbarrio')
fincas.catastro <- cbind(fincas.catastro, as.data.frame(predict.lm(modeloLmul, fincas.catastro, interval = "confidence")))
setnames(fincas.catastro, c('fit', 'lwr', 'upr'), c('precio.alquiler', 'iconf_low', 'iconf_up'))

fincas.catastro.otros.barrios <- cbind(fincas.catastro.otros.barrios,
                                       as.data.frame(predict.lm(modeloLmul.nobarrio, fincas.catastro.otros.barrios, interval = "confidence")))
setnames(fincas.catastro.otros.barrios, c('fit', 'lwr', 'upr'), c('precio.alquiler', 'iconf_low', 'iconf_up'))

fincas.catastro <- rbind(fincas.catastro, fincas.catastro.otros.barrios)

## Revisamos los datos estimados

summary(fincas.catastro)
fincas.catastro[fincas.catastro$precio.alquiler == max(fincas.catastro$precio.alquiler),]
fincas.catastro[fincas.catastro$precio.alquiler == min(fincas.catastro$precio.alquiler),]
fincas.catastro[fincas.catastro$precio.alquiler < 0,]


## Grabamos el fichero con las predicciones de catastro para los dashboards y presentaciones
## Un agrupado para los datos a nivel mapa y uno con direcciones para la búsqueda por dirección (sólo variables de interés)
## Agregamos al nivel de presentación para disminuir tamaño Calle, número, bis, altura, garaje, año, metros, precio alquiler

agrupado.barrio <- data.table(fincas.catastro)[,.N, by = .(idbarrio,barrio,metros,altura_num,Garaje,anio_max, precio.alquiler)]

fincas.catastro$idbarrio <- as.character(fincas.catastro$idbarrio)
fincas.catastro$idbarrio <- str_pad(fincas.catastro$idbarrio,side = 'left',width = 3,pad = '0')

agrupado.barrio$idbarrio <- as.character(agrupado.barrio$idbarrio)
agrupado.barrio$idbarrio <- str_pad(agrupado.barrio$idbarrio,side = 'left',width = 3,pad = '0')
agrupado.barrio$metros.totales <- agrupado.barrio$metros*agrupado.barrio$N
agrupado.barrio$precio.alquiler.total <- agrupado.barrio$precio.alquiler * agrupado.barrio$N

## Agregamos un indicador para calular la altura media ponderada en función de metros y altura y descargar cálculos del dashboard
agrupado.barrio$numerador.propon.alt <- agrupado.barrio$N * agrupado.barrio$metros * agrupado.barrio$altura_num^2
agrupado.barrio$denominador.propon.alt <- agrupado.barrio$N * agrupado.barrio$metros * agrupado.barrio$altura_num
agrupado.barrio$numerador.propon.anti <- agrupado.barrio$N * agrupado.barrio$metros * agrupado.barrio$anio_max^2
agrupado.barrio$denominador.propon.anti <- agrupado.barrio$N * agrupado.barrio$metros * agrupado.barrio$anio_max
agrupado.barrio$numerador.propon.superficie <- agrupado.barrio$N * agrupado.barrio$metros

write.csv(fincas.catastro,file = file.fincas.catastro.out, row.names = F)
write.csv(agrupado.barrio,file = file.barrios.out, row.names = F)

