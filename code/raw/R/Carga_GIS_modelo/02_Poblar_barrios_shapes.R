## Añade a un data frame de entrada con columnas de coordenadas, el nombre del barrio
## obtenido por cruce espacial con el shape de barrios de madrid
##

## La capa de barrios de Madrid se puede obtener en ed50 de: http://www.madrid.org/nomecalles/DescargaBDTCorte.icm. (Delimitaciones territoriales, barrios)


library(rgdal)

clean.data.dir <- '../../../../data/clean'

ruta <- str_c(clean.data.dir, '/SHP/Barrios Madrid')

barrios.shp <- readOGR(dsn = ruta, layer = "200001465")
plot(barrios.shp)
