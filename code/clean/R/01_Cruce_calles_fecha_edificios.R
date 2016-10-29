## Asociamos los literales de calle de catastro a los de Ivima para cada calle de este último listado.
## así podremos llevar el año de construcción de catastro a Ivima para utilizarlo como predictor en el 
## modelo. También asociaremos las coordenadas xy de catastro para poblar el barrio en Ivima desde el 
## shape de barrios

library(data.table)
library(stringr)
library(tidyr)
library(rgdal)
library(readxl)


clean.data.dir <- '../../../data/clean'
file.fincas.ivima <- paste(clean.data.dir,  "/IVIMA/fincas_ivima.csv", sep = "")
file.fincas.cat <- paste(clean.data.dir,  "/ficheros_preparados/BI_28_900_U_2016-01-23.csv.gz", sep = "")
file.tipos.via.cat <- paste(clean.data.dir,  "/CAT/tipo_via_cat.csv", sep = "")
file.calles.cruzadas <- paste(clean.data.dir,  "/IVIMA/calles_cruzadas.csv", sep = "")
ruta.shp <- str_c(clean.data.dir, '/SHP/Barrios Madrid')



# El objetivo es normalizar las direcciones de Ivima y catastro para poder relacionar los datos de ambas fuentes en base a
# tipo_via, nombre_via, numero_finca, letra finca

fincas_ivima <- data.table(read.table(file = file.fincas.ivima, header = T, comment.char = "", sep = "^"))

fincas_catastro <- data.table(read.table(gzfile(file.fincas.cat), header = T, sep = '^', encoding = 'utf-8'))

fincas_catastro <- data.table(fincas_catastro)
fincas_ivima <- data.table(fincas_ivima)

# Separamos el tipo de via del nombre de la via en los datos IVIMA

fincas_ivima$tipo_via <- data.frame(str_split(fincas_ivima$Calle, " ", n=2, simplify = T))[[1]]
fincas_ivima$nombre_via <- data.frame(str_split(fincas_ivima$Calle, " ", n=2, simplify = T))[[2]]

## Preparamos los tipos de via de catastro con su descripción en formato tabla

tipos_via_cat <- data.table(read.csv(file.tipos.via.cat, header = T, sep = ';'))

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


### Hay que poner un if, el proceso manual sólo se ejecuta si no lo hemos ejecutado aún, creando el fichero de salida
### si el fichero de salida ya  existe, no ejecutamos el proceso manual. Objetivo obtener "calles_cruzadas", que asocia
### los literales de calle de catastro con los de Ivima para poder enriquecer la tabla de fincas Ivima


if (file.exists(file.calles.cruzadas) == F)
  {
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
  # Por defecto todas las lineas aparecen marcadas correcta=1, borrar manualmente el 1 de las líneas que no coincidan
  matriz_analisis <- edit(matriz_analisis)
  calles_cruzadas <- rbind(calles_cruzadas, matriz_analisis[is.na(matriz_analisis$correcta) == F, c("tipo_via", "nombre_via_ivima", "nombre_via_cat")])

    # Eliminamos las cruzadas para analizar las de ranking 2
  matriz_analisis <- matriz_lev[ranking==2]
  calles_cruzadas$mark <- 1
  matriz_analisis <- merge(matriz_analisis, calles_cruzadas, all.x = T, by.x = c("tipo_via", "nombre_via_ivima"), 
                           by.y = c("tipo_via", "nombre_via_ivima"))
  matriz_analisis <- matriz_analisis[is.na(matriz_analisis$mark) == T, .(tipo_via,nombre_via_ivima, nombre_via_cat.x, distancia, ranking)]
  colnames(matriz_analisis) <- c("tipo_via", "nombre_via_ivima", "nombre_via_cat", "distancia", "ranking")
  matriz_analisis$correcta <- 1
  matriz_analisis <- edit(matriz_analisis)
  matriz_analisis$mark <- 1
  calles_cruzadas <- rbind(calles_cruzadas, matriz_analisis[is.na(matriz_analisis$correcta) == F, c("tipo_via", "nombre_via_ivima", "nombre_via_cat", "mark")])

    # Eliminamos las cruzadas para analizar las de ranking 3
  matriz_analisis <- matriz_lev[ranking==3]
  calles_cruzadas$mark <- 1
  matriz_analisis <- merge(matriz_analisis, calles_cruzadas, all.x = T, by.x = c("tipo_via", "nombre_via_ivima"), 
                           by.y = c("tipo_via", "nombre_via_ivima"))
  matriz_analisis <- matriz_analisis[is.na(matriz_analisis$mark) == T, .(tipo_via,nombre_via_ivima, nombre_via_cat.x, distancia, ranking)]
  colnames(matriz_analisis) <- c("tipo_via", "nombre_via_ivima", "nombre_via_cat", "distancia", "ranking")
  matriz_analisis$correcta <- 1
  matriz_analisis <- edit(matriz_analisis)

  matriz_analisis$mark <- 1
  calles_cruzadas <- rbind(calles_cruzadas, matriz_analisis[is.na(matriz_analisis$correcta) == F, c("tipo_via", "nombre_via_ivima", "nombre_via_cat", "mark")])
  
  ## La ganancia por seguir avanzando en el ranking es muy pequeña. Finalizamos las iteraciones y guardamos el resultado
  calles_cruzadas$mark <- NULL
  write.csv(calles_cruzadas, file.calles.cruzadas, row.names = F)
} else
{
  calles_cruzadas <- data.table(read.table(file = file.calles.cruzadas, header = T, comment.char = "", sep = ","))
}

colnames(calles_cruzadas) <- c("tipo_via_cat", "nombre_via", "nombre_via_cat")
fincas.ivima.enriquecidas <- merge(fincas_ivima, calles_cruzadas, by.x = c("tipo_via_cat", "nombre_via"), by.y=c("tipo_via_cat", "nombre_via"))
fincas.ivima.enriquecidas$numfinca <- str_c(str_pad(fincas.ivima.enriquecidas$num_pol,width = 4,side = 'left',pad = '0'), fincas.ivima.enriquecidas$letra)
fincas_catastro$numfinca <- str_c(str_pad(fincas_catastro$num_pol1, width = 4, side = 'left', pad = '0'), fincas_catastro$bis)
portalero.catastro <- fincas_catastro[, .N, by = .(tipo_via, nombre_via, numfinca, x_coor, y_coor, anio_mejor)]
portalero.catastro <- portalero.catastro[, ranking := rank(-anio_mejor, ties = "random"), by = c("tipo_via","nombre_via","numfinca")]
portalero.catastro <- portalero.catastro[ranking==1]

portalero.ivima <- fincas.ivima.enriquecidas[, .N, by = .(tipo_via_cat, nombre_via_cat, numfinca)]

## Enriquecemos el portalero de catastro con el barrio,
## obtenido por cruce espacial con el shape de barrios de madrid

## La capa de barrios de Madrid se puede obtener en ed50 de: http://www.madrid.org/nomecalles/DescargaBDTCorte.icm. (Delimitaciones territoriales, barrios)

## Cargamos la capa de barrios. Está en proyección EPGS:23030, ED50/UTM30

barrios.shp <- readOGR(dsn = ruta.shp, layer = "200001465", encoding = "latin-1")
proj4string(barrios.shp) <- CRS("+init=epsg:23030")


# Creamos la capa de puntos desde las xy del portalero de catastro

### Generamos el shape para poder importar directamente en GIS (solo las fincas con coordenadas),proyeccion EPGS:25830
portalero.cat.con.coor <- portalero.catastro[portalero.catastro$x_coor != 0,]
coordenadas <- as.matrix(portalero.cat.con.coor[,.(x_coor, y_coor)])
capa.puntos <- SpatialPointsDataFrame(coordenadas, portalero.cat.con.coor, proj4string = CRS("+init=epsg:25830"), coords.nrs = c(4, 5), match.ID = T)
capa.puntos <- spTransform(capa.puntos, CRS("+init=epsg:23030"))

## Agregamos los datos del barrio al df de portales
portales.con.barrio <- over(capa.puntos, barrios.shp)
portales.con.barrio$indice <- rownames(portales.con.barrio)
capa.puntos$indice <- rownames(capa.puntos@data)
portales.con.barrio <- merge(capa.puntos@data, portales.con.barrio, by.x = "indice", by.y = "indice")

## Eliminamos columnas sin interés, poblamos el barrio y nos quedamos sólo con fincas vivienda de catastro

fincas_catastro <- fincas_catastro[clave_grupo_BI=='V',.(parcela_cat, cvia_DGC, tipo_via, nombre_via, num_pol1, bis,
                   num_pol2, bis2, Km, bloque, escalera, planta, puerta, dir_resto, 
                   m2_BI, m2_solares_sin_div_hor, coef_finca, garage, anio_mejor, numfinca)]

portales.con.barrio$idbarrio <- data.frame(str_split(portales.con.barrio$DESBDT, " ", n=2, simplify = T))[[1]]
portales.con.barrio$desbarrio <-  data.frame(str_split(portales.con.barrio$DESBDT, " ", n=2, simplify = T))[[2]]
portales.con.barrio <- data.table(portales.con.barrio[,c("tipo_via", "nombre_via", "numfinca", "x_coor", "y_coor", "idbarrio", "desbarrio")])
fincas_catastro <- merge(fincas_catastro, portales.con.barrio, by.x = c("tipo_via", "nombre_via", "numfinca"), by.y = c("tipo_via", "nombre_via", "numfinca"))
fincas.ivima.enriquecidas <- merge(fincas.ivima.enriquecidas, portales.con.barrio, by.x =c("tipo_via_cat", "nombre_via_cat", "numfinca"), by.y =c("tipo_via", "nombre_via", "numfinca") )
fincas.ivima.enriquecidas <- fincas.ivima.enriquecidas[,.(tipo_via_cat, nombre_via_cat, numfinca, metros, habitaciones, Garaje, Precio, eur_metro, eur_metro_round, planta_cat, num_pol, letra, x_coor, y_coor, idbarrio, desbarrio)]
anio.max.finca <- fincas_catastro[, .(anio_max = max(anio_mejor)), by = .(tipo_via, nombre_via, numfinca)]
fincas.ivima.enriquecidas <- merge(fincas.ivima.enriquecidas, anio.max.finca, by.x =c("tipo_via_cat", "nombre_via_cat", "numfinca"), by.y =c("tipo_via", "nombre_via", "numfinca") )

plantas_cat <- fincas_catastro[,.N, by = .(planta)]
plantas_ivima <- fincas.ivima.enriquecidas[,.N, by = .(planta_cat)]

## Tenemos poca varieadad de plantas en Ivima. Para poder extender el modelo a todas las viviendas de catastro
## es mejor pasar la variable planta (categórica) a altura (entero) de modo que el modelo se pueda generalizar
## Convertimos las plantas de catastro en un numérico, que llevaremos luego a las fincas de Ivima
## Calculamos en catastro el número de plantas para cada finca, para dar un número de planta a los áticos
## Todos los literales de planta que empiecen por número son numéricos, los que no, si empiezan por A son áticos, el
## resto los asimilamos a Bajos

## extraemos los números

plantas_cat$primer_caracter <- str_sub(plantas_cat$planta,1,1) 
plantas_cat$primer_caracter <- str_extract(plantas_cat$primer_caracter, '[a-zA-Z]')
plantas_cat$altura <- ifelse(is.na(plantas_cat$primer_caracter)==F,plantas_cat$primer_caracter,str_extract(plantas_cat$planta, '[+-]*[0-9]+'))
plantas_cat[grep('[B-Z]', altura), altura:='00']

## Hay que calcular la altura de los áticos relativa a su edificio
plantas_cat[,c('N','primer_caracter'):=NULL]
fincas_catastro <- merge(fincas_catastro, plantas_cat, by.x = 'planta', by.y = 'planta')
plantas_cat <- fincas_catastro[,.N, by = .(tipo_via, nombre_via, numfinca, altura)]
plantas_cat <- plantas_cat[,.N, by = .(tipo_via, nombre_via, numfinca)]
setnames(plantas_cat, 'N', 'alt_max')
plantas_cat$atico <- 'A'
fincas_catastro <- merge(fincas_catastro, plantas_cat, all.x = T, by.x=c('tipo_via', 'nombre_via', 'numfinca','altura'), by.y = c('tipo_via', 'nombre_via', 'numfinca','atico'))
fincas_catastro[altura == 'A', altura:=as.character(alt_max)]
fincas_catastro$alt_max <- NULL
fincas_catastro$altura <- as.integer(fincas_catastro$altura)
plantas_cat <- fincas_catastro[,.N, by = .(tipo_via, nombre_via, numfinca,planta, altura)]
plantas_cat$N <- NULL
setnames(plantas_cat, c('planta','tipo_via','nombre_via'), 
         c('planta_cat','tipo_via_cat','nombre_via_cat'))
 
## La planta de fincas Ivima pasamos a número con el mismo criterio que en catastro
## Sólo recuperamos los áticos desde catastro

fincas.ivima.enriquecidas$primer_caracter_planta <- str_sub(fincas.ivima.enriquecidas$planta_cat,1,1) 
fincas.ivima.enriquecidas$primer_caracter_planta <- str_extract(fincas.ivima.enriquecidas$primer_caracter_planta, '[a-zA-Z]')
fincas.ivima.enriquecidas$altura <- ifelse(is.na(fincas.ivima.enriquecidas$primer_caracter_planta)==F,
                                           fincas.ivima.enriquecidas$primer_caracter_planta,
                                           str_extract(fincas.ivima.enriquecidas$planta, '[+-]*[0-9]+'))

fincas.ivima.enriquecidas[grep('[B-Z]', altura), altura:='00']
fincas.ivima.enriquecidas$primer_caracter_planta <- NULL
aticos.cat <- plantas_cat[grep('^A',planta_cat)]
aticos.cat$planta_cat <- 'A'
setnames(aticos.cat, c('altura','planta_cat'), c('altura_num','altura'))

fincas.ivima.enriquecidas <- merge(fincas.ivima.enriquecidas, aticos.cat, all.x=T, 
                                   by.x=c('tipo_via_cat', 'nombre_via_cat', 'numfinca', 'altura'), 
                                   by.y=c('tipo_via_cat', 'nombre_via_cat', 'numfinca', 'altura'))

fincas.ivima.enriquecidas$altura_num <- as.character(fincas.ivima.enriquecidas$altura_num)

fincas.ivima.enriquecidas[is.na(altura_num)==T & altura != 'A', altura_num := altura]

fincas.ivima.enriquecidas$altura_num <- as.numeric(fincas.ivima.enriquecidas$altura_num)

#Corregimos el literal de barrio, los literales del shape tienen caracteres mal codificados
#Obtenemos el listado de barrios de http://www.madrid.org/iestadis/fijas/clasificaciones/descarga/cobar15.xls


listado.barrios <- data.table(read_excel(str_c(clean.data.dir, '/SHP/Barrios Madrid/cobar15.xls'),sheet = 'cobar15'))
listado.barrios$idbarrio <- str_c(listado.barrios$`Código distrito`, listado.barrios$`Código barrio`)
listado.barrios <- listado.barrios[, .N, by = .(idbarrio, `Literal barrio`)]
listado.barrios$N <- NULL
setnames(listado.barrios, 'Literal barrio', 'barrio')

fincas_catastro <- merge(fincas_catastro, listado.barrios, by.x = 'idbarrio', by.y = 'idbarrio')
fincas_catastro$desbarrio <- NULL
fincas_catastro <- fincas_catastro[is.na(fincas_catastro$altura) == F,]

fincas.ivima.enriquecidas <- merge(fincas.ivima.enriquecidas, listado.barrios, by.x = 'idbarrio', by.y = 'idbarrio')
fincas.ivima.enriquecidas$desbarrio <- NULL
fincas.ivima.enriquecidas <- fincas.ivima.enriquecidas[is.na(fincas.ivima.enriquecidas$altura_num) == F,]
fincas.ivima.enriquecidas$idbarrio <- as.factor(fincas.ivima.enriquecidas$idbarrio)

## Ponemos en el tipo de via de catastro un literal reconocible en lugar de la abreviatura

tipos_via_cat <- data.table(read.csv(file.tipos.via.cat, header = T, sep = ';'))
tipos_via_cat$descripcion <- str_replace(tipos_via_cat$descripcion, ', ', '-')
colnames(tipos_via_cat) = c('tipo_via', 'descripcion')
fincas_catastro <- merge(fincas_catastro, tipos_via_cat, by.x = 'tipo_via', by.y ='tipo_via')


write.csv(fincas_catastro, str_c(clean.data.dir, '/modelo/fincas_catastro.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(fincas.ivima.enriquecidas, str_c(clean.data.dir, '/modelo/fincas_ivima.csv'), row.names = F, fileEncoding = 'UTF-8')




