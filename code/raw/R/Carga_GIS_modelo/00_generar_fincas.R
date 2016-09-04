library(stringr)
library(data.table)
library(tidyr)
library(rgdal)

## Referencias a ficheros externos

fichero.bitstream <- 'C:/00_datos_usuario/01_projects/01_remedy/data/raw/BISTREAM_FINAL_MASMOVIL.zip'
fichero.adamo <- '../../data/raw/BITSTREAM_ADAMO.csv'
fichero.ref.fincas <- 'C:/00_datos_usuario/99_imports/01_Extraidos/06_Callejeros/R.txt/total_fincas.txt'


### Cargar fichero Remedies y Adamo. Formatearlos a la misma estructura

bitstream <- data.table(read.table(unz(fichero.bitstream, 'BISTREAM_FINAL_MASMOVIL.txt'),
                                   header = T,sep = ";",
                                   quote = "",
                                   comment.char = "",
                                   colClasses = "character",
                                   encoding = "UTF-8"))

names(bitstream)[names(bitstream)=="ID_DOMICILIO.TO"] <- "G37"
names(bitstream)[names(bitstream)=="ID_TECNICO_DE_LA_VIA"] <- "G12"

bitstream$G37 <- str_sub(bitstream$G37, 2)

adamo <- fread(fichero.adamo, colClasses = "character")
names(adamo)[names(adamo)=="GESCAL37"] <- "G37"

##cambiamos el nombre de las columnas de adamo al mismo que el de bitstream

colnames(adamo) <- c("G37","Codigo.Postal","Provincia","Poblacion","Tipo.via","Nombre.Via","Numero","BIS","Bloque_finca","Portal_puerta","Letra","Escalera","Planta","Mano1","Mano2","Obsservaciones.comentario","Flag.dummy","Cod.INE.Via","Codigo.Censal","Codigo.OLT","Codigo.CTO","Tipo.de.CTO","Direccion.Ubicación.CTO","Tipo.de.permiso","Tipo.caja.de.derivacion","N.unidades.inmobiliarias","address_id","apartment_id")

### Añadir columna con el operador gestor y G18
bitstream$gestor <- "J"
bitstream$G18 <- str_trim(str_sub(bitstream$G37, 1, 18), "both")

adamo$gestor <- "A"
adamo$G18 <- str_trim(str_sub(adamo$G37, 1, 18), "both")
adamo$G12 <- str_sub(adamo$G37, 1, 12)

### Verificar si hay coincidencias a nivel de G18. Si las hay dejar sólo 1 registro, el de Remedies
G18.adamo <- adamo[, list(cuenta = length(G37)), by = c("G18")]
G18.bitstream <- bitstream[, list(cuenta = length(G37)), by = c("G18")]

coincidentes <- merge(G18.bitstream, G18.adamo, by.x = "G18", by.y = "G18")

### Unificar en 1 sola tabla, solo las columnas relevantes

## dejamos solo columnas relevantes y compactamos a G18

cobertura.G37 <- rbind(bitstream[, .(G37, Codigo.Postal, Provincia, Poblacion, Tipo.via, Nombre.Via, G12, Numero, BIS, Bloque_finca, Portal_puerta, Letra, Escalera, Planta, Mano1, Mano2, Flag.dummy, gestor, G18)], 
  adamo[, .(G37, Codigo.Postal, Provincia, Poblacion, Tipo.via, Nombre.Via, G12, Numero, BIS, Bloque_finca, Portal_puerta, Letra, Escalera, Planta, Mano1, Mano2, Flag.dummy, gestor, G18)]
)

cobertura.G18 <- cobertura.G37[, list(UUII = length(G37)), by = c("G18","Provincia","Poblacion", "Codigo.Postal","Tipo.via","Nombre.Via","G12","Numero","BIS","Flag.dummy","gestor")]

resumen.UUII <- cobertura.G18[Flag.dummy == 0, list(UUII = sum(UUII)), by = c("Provincia","Poblacion", "gestor")]
resumen.UUII <- dcast(resumen.UUII, Provincia + Poblacion ~ gestor, fun.aggregate = sum, value.var = "UUII")

resumen.accesos <- cobertura.G18[Flag.dummy == 1, list(accesos = sum(UUII)), by = c("Provincia","Poblacion", "gestor")]
resumen.accesos <- dcast(resumen.accesos, Provincia + Poblacion ~ gestor, fun.aggregate = sum, value.var = "accesos")

### Cargar fichero fincas, solo con información relevante

fincas.total <- data.table(read.table(fichero.ref.fincas,
                                   header = T,sep = ";",
                                   comment.char = "",
                                   dec = ",",
                                   colClasses = "character"
                                   ))

fincas.total[, c("cod_finca_dgc", "GESCAL17", "tipo_cod_finca", "cprov", "cmundgc", "ine_mun", "PROVINCIA", "MUNICIPIO", "parcela", "TipoVia", "Calle", "Npolic", "letra", "codigo_postal", "SSCC", "CR_MEJOR_DATO", "CR_TYPE_MEJOR_DATO", "CR_ID_MEJOR_DATO", "EECC_FDTT", "MIGA_JAZZ", "CABECERA_JAZZ", "ARBOL", "ESTADO_FINCA", "Gerencia", "tipo_instalacion", "detalle_tipo_instalacion", "FECHA_ESTADO", "ANO_MES_ESTADO"):= NULL]

fincas.total[, c("X", "Y")] <- fincas.total[,lapply(list(X,Y),sub, pattern=",", replacement=".")]

fincas.total[, c("X", "Y")] <- fincas.total[,lapply(list(X,Y),as.double)]

fincas.total[, c("Viviendas_CAT", "Locales_CAT", "UUII_CAT", "UUII_EST")] <- fincas.total[,lapply(list(Viviendas_CAT, Locales_CAT, UUII_CAT, UUII_EST),as.integer)]

### Poblar desde fincas de referencia datos de interés (xy, uuii catastro)

cobertura.G18 <- merge(cobertura.G18, fincas.total, all.x = T, by.x = "G18", by.y = "cod_finca")

### Repoblar donde no hay x,y desde cartociudad



### Generamos el shape para poder importar directamente en GIS (solo las fincas con coordenadas)
fincas.con.coor <- cobertura.G18[is.na(cobertura.G18$X) == F,]
fincas.con.coor <- fincas.con.coor[fincas.con.coor$X != 0,]
coordenadas <- as.matrix(fincas.con.coor[,.(X, Y)])
capa.puntos <- SpatialPointsDataFrame(coordenadas, fincas.con.coor, proj4string = CRS("+init=epsg:23030"), coords.nrs = c(13,14), match.ID = T)
capa.puntos <- spTransform(capa.puntos, CRS("+init=epsg:3042"))

writeOGR(capa.puntos, dsn = "../../shapes", layer = "fincas", driver = "ESRI Shapefile",overwrite_layer = T )


### Exportar las tablas de resultado (importar en Qlik y/o Access, Qgis, etc.)

write.csv(cobertura.G18, '../../data/clean/coberturaG18.csv', row.names = F)
write.csv(cobertura.G37, '../../data/clean/coberturaG37.csv', row.names = F)
