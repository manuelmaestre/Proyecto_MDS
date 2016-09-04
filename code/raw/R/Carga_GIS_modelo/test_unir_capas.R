library(rgdal)

avila <- readOGR(dsn = "03_maps/test/avila", layer = "CODIGO_POSTAL")
albacete <- readOGR(dsn = "03_maps/test/albacete", layer = "CODIGO_POSTAL")

proj4string(avila)
proj4string(albacete)

EPSG <- make_EPSG()
EPSG[grepl("ETRS89$", EPSG$note),]
EPSG[grepl("UTM$", EPSG$note),]

proj4string(avila) <- CRS("+init=epsg:4258")
proj4string(albacete) <- CRS("+init=epsg:4258")

capa.nueva <- rbind.SpatialPolygonsDataFrame(avila, albacete, makeUniqueIDs = T)
capa.nueva <- spTransform(capa.nueva, CRS("+init=epsg:23030"))


plot(capa.nueva)
writeOGR(capa.nueva, dsn = "03_maps/test", layer = "CODIGO_POSTAL", driver = "ESRI Shapefile",overwrite_layer = T )


test.fie <- list.files('03_maps/shp/carto/', full.names = T, include.dirs = T)[1]
unzip(zipfile = test.fie, exdir = 'temp', overwrite = T, files = c("CODIGO_POSTAL.cpg",
                                                                   "CODIGO_POSTAL.dbf",
                                                                   "CODIGO_POSTAL.prj",
                                                                   "CODIGO_POSTAL.shp",
                                                                   "CODIGO_POSTAL.shx"))
