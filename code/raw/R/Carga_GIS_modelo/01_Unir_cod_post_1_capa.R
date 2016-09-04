library(rgdal)

EPSG <- make_EPSG()
EPSG[grepl("ETRS89$", EPSG$note),]
#EPSG[grepl("UTM$", EPSG$note),]

## Objetivo: unir todas las capas de códigos postales provinciales en una única capa nacional
## desde los .zip individuales

# función capa.prepara(fichero), para un zip de entrada, devuelve la capa en proyección UTM30/ED50

capa.prepara <- function(ruta){
  unzip(zipfile = ruta, exdir = 'temp', overwrite = T, files = c("CODIGO_POSTAL.cpg",
                                                                     "CODIGO_POSTAL.dbf",
                                                                     "CODIGO_POSTAL.prj",
                                                                     "CODIGO_POSTAL.shp",
                                                                     "CODIGO_POSTAL.shx"))
  capa.temp <- readOGR(dsn = "temp", layer = "CODIGO_POSTAL")
  proj4string(capa.temp) <- CRS("+init=epsg:4258")
  capa.temp <- spTransform(capa.temp, CRS("+init=epsg:3042"))
  
  return(capa.temp)
  
}

list.fich <- list.files('03_maps/shp/carto/', full.names = T, include.dirs = T)

capa.cod.pos <-  do.call(rbind.SpatialPolygonsDataFrame, c(lapply(list.fich, capa.prepara), makeUniqueIDs = T))

capa.cod.pos$ID_CP <- NULL

#plot(capa.cod.pos)

suppressWarnings(writeOGR(capa.cod.pos, dsn = "03_maps/shp/generadas", layer = "CODIGO_POSTAL", driver = "ESRI Shapefile",overwrite_layer = T ))

plot(capa.cod.pos)



