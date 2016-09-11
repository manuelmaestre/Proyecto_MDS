## Añade a un data frame de entrada con columnas de coordenadas, el nombre del barrio
## obtenido por cruce espacial con el shape de barrios de madrid
##

## La capa de barrios de Madrid se puede obtener en ed50 de: http://www.madrid.org/nomecalles/DescargaBDTCorte.icm. (Delimitaciones territoriales, barrios)


library(rgdal)
library(data.table)
library(sp)

clean.data.dir <- '../../../../data/clean'

ruta <- str_c(clean.data.dir, '/SHP/Barrios Madrid')

## Cargamos la capa de barrios. Está en proyección EPGS:23030, ED50/UTM30

barrios.shp <- readOGR(dsn = ruta, layer = "200001465", encoding = "LATIN-1")
proj4string(barrios.shp) <- CRS("+init=epsg:23030")
plot(barrios.shp)

# Creamos la capa de puntos desde las xy del portalero de catastro

### Generamos el shape para poder importar directamente en GIS (solo las fincas con coordenadas),proyeccion EPGS:25830
portalero.con.coor <- portalero.catastro[portalero.catastro$x_coor != 0,]
coordenadas <- as.matrix(portalero.con.coor[,.(x_coor, y_coor)])
capa.puntos <- SpatialPointsDataFrame(coordenadas, portalero.con.coor, proj4string = CRS("+init=epsg:25830"), coords.nrs = c(4, 5), match.ID = T)
capa.puntos <- spTransform(capa.puntos, CRS("+init=epsg:23030"))

#head(capa.puntos@data)
#test <- capa.puntos[capa.puntos$anio_mejor>=2014,]
#plot(barrios.shp)
#points(test)
portales.con.barrio <- over(capa.puntos, barrios.shp)
#head(unidos)
#head(test)
portales.con.barrio$indice <- rownames(portales.con.barrio)
capa.puntos$indice <- rownames(capa.puntos@data)
head(capa.puntos)
head(portales.con.barrio)

portales.con.barrio <- merge(capa.puntos@data, portales.con.barrio, by.x = "indice", by.y = "indice")
head(portales.con.barrio)
table(portales.con.barrio$DESBDT)
