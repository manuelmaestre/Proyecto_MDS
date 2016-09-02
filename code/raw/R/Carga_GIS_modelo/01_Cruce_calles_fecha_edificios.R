## Asociamos los literales de calle de catastro a los de Ivima para cada calle de este último listado.
## así podremos llevar el año de construcción de catastro a Ivima para utilizarlo como predictor en el 
## modelo. También asociaremos las coordenadas xy de catastro para poblar el barrio en Ivima desde el 
## shape de barrios

library(data.table)
library(stringr)
library(tidyr)

# El objetivo es normalizar las direcciones de Ivima y catastro para poder relacionar los datos de ambas fuentes en base a
# tipo_via, nombre_via, numero_finca, letra finca

fincas_ivima <- data.table(read.table(file = '../../../../data/clean/IVIMA/fincas_ivima.csv', header = T, comment.char = "", sep = "^"))

fincas_catastro <- data.table(read.table(gzfile('../../../../data/clean/ficheros_preparados/BI_28_900_U_2016-01-23.csv.gz'), header = T, sep = '^', encoding = 'utf-8'))

fincas_catastro <- data.table(fincas_catastro)
fincas_ivima <- data.table(fincas_ivima)

# Separamos el tipo de via del nombre de la via en los datos IVIMA

fincas_ivima$tipo_via <- data.frame(str_split(fincas_ivima$Calle, " ", n=2, simplify = T))[[1]]
fincas_ivima$nombre_via <- data.frame(str_split(fincas_ivima$Calle, " ", n=2, simplify = T))[[2]]

## Preparamos los tipos de via de catastro con su descripción en formato tabla

tipos_via_cat <- data.table(read.csv('../../../../data/clean/CAT/tipo_via_cat.csv', header = T, sep = ';'))

tipos_via_cat <- cbind(tipos_via_cat, data.table(str_split(tipos_via_cat$descripcion, ",", simplify = T)))
tipos_via_cat$V1 <- str_trim(tipos_via_cat$V1)
tipos_via_cat$V2 <- str_trim(tipos_via_cat$V2)
tipos_via_cat <- melt(tipos_via_cat,c("cod_tipo_via", "descripcion"), na.rm = T)
tipos_via_cat <- tipos_via_cat[value != ""]
tipos_via_cat$variable <- NULL
colnames(tipos_via_cat) <- c("cod_tipo_via", "descripcion",  "tipo_via")

# Eliminamos las tildes del tipo de via Ivima y del nombre de via
fincas_ivima$tipo_via <- chartr('ÁÉÍÓÚ','AEIOU', fincas_ivima$tipo_via)
fincas_ivima$nombre_via <- chartr('ÁÉÍÓÚ','AEIOU', fincas_ivima$nombre_via)

## Verificamos que los tipos de via de IVIMA y catastro coinciden sus literales
tipos_via_ivima <- data.table(table(fincas_ivima$tipo_via))
merge(tipos_via_ivima, tipos_via_cat,all.x = T, by.x = "V1", by.y = "tipo_via")

## Hay 1 finca sin tipo de via y 7 en "BULEVAR", que no existe en castastro que no podremos usar en el modelo

## Llevamos el tipo de via de catastro a la tabla de IVIMA
tipos_via_cat$descripcion <- NULL
colnames(tipos_via_cat) = c("tipo_via_cat", "tipo_via")
fincas_ivima <- merge(fincas_ivima, tipos_via_cat, all.x = T, by.x = "tipo_via", by.y = "tipo_via")

## Eliminamos las filas de Ivima que no tienen tipo de via correcto de catastro
fincas_ivima <- drop_na(fincas_ivima, tipo_via_cat)

## Preparamos las matrices para cruzar los literales de calle de catastro y de ivima

callejero_ivima <- fincas_ivima[, .N, by = .(tipo_via_cat, nombre_via)]
callejero_cat <- fincas_catastro[, .N, by = .(tipo_via, nombre_via)]
colnames(callejero_ivima) <- c("tipo_via", "nombre_via_ivima", "N")
colnames(callejero_cat) <- c("tipo_via", "nombre_via_cat", "N")
callejero_ivima$tipo_via <- as.character(callejero_ivima$tipo_via)
callejero_ivima$nombre_via_ivima <- as.character(callejero_ivima$nombre_via_ivima)
callejero_cat$tipo_via <- as.character(callejero_cat$tipo_via)
callejero_cat$nombre_via_cat <- as.character(callejero_cat$nombre_via_cat)

cruce_directo <- merge(callejero_ivima, callejero_cat, all.x = T, by.x = c("tipo_via", "nombre_via_ivima"), by.y = c("tipo_via", "nombre_via_cat"))


## Separamos las calles en las que los literales de catastro e ivima son iguales
calles_cruzadas <- cruce_directo[is.na(cruce_directo$N.y) == F]
calles_pendientes <- cruce_directo[is.na(cruce_directo$N.y) == T]

calles_pendientes$N.x <- NULL
calles_pendientes$N.y <- NULL
calles_cruzadas$N.x <- NULL
calles_cruzadas$N.y <- NULL
calles_cruzadas$nombre_via_cat <- calles_cruzadas$nombre_via_ivima


matriz_lev <- merge(calles_pendientes, callejero_cat, all.x = T, by.x = c("tipo_via"), by.y = c("tipo_via"), allow.cartesian = T)
matriz_lev$N <- NULL
matriz_lev <- matriz_lev[, distancia := adist(nombre_via_ivima, nombre_via_cat)/max(c(nchar(nombre_via_ivima), nchar(nombre_via_cat))), by = "nombre_via_ivima"]
matriz_lev <- matriz_lev[, ranking := rank(distancia, ties = "random"), by = "nombre_via_ivima"]

## Hay que revisar manual/visualmente las coincidencias, marcarlas como correctas añadir a calles cruzadas, eliminar de la matriz y repetir para el siguiente
## ranking, hasta conseguir una normalización suficiente

matriz_analisis <- matriz_lev[ranking==1]
matriz_analisis[order(matriz_analisis$ranking)]
setcolorder(matriz_analisis, c("tipo_via","nombre_via_ivima", "nombre_via_cat", "ranking", "distancia"))
setorder(matriz_analisis, tipo_via, nombre_via_ivima, ranking)
matriz_analisis$correcta <- 1
matriz_analisis <- edit(matriz_analisis)
#write.csv(matriz_analisis, '../../../../data/raw/matriz_cruce_manual.csv', row.names = F)
calles_cruzadas <- rbind(calles_cruzadas, matriz_analisis[is.na(matriz_analisis$correcta) == F, c("tipo_via", "nombre_via_ivima", "nombre_via_cat")])

# Eliminamos las cruzadas para analizar las de ranking 2
matriz_analisis <- matriz_lev[ranking==2]
calles_cruzadas$mark <- 1
matriz_analisis <- merge(matriz_analisis, calles_cruzadas, all.x = T, by.x = c("tipo_via", "nombre_via_ivima"), 
                         by.y = c("tipo_via", "nombre_via_ivima"))
matriz_analisis <- matriz_analisis[is.na(matriz_analisis$mark) == T, .(tipo_via,nombre_via_ivima, nombre_via_cat.x, distancia, ranking)]






