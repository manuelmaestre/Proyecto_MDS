

# Estimación precio alquiler en Madrid.  

***

### (Trabajo Fin de Master Data Science Kschool. Manuel Maestre Rodríguez)  
### Octubre 2016


## Objetivo del estudio.

El estudio tiene por finalidad estimar el precio del alquiler de la vivienda en Madrid. Para ello se utilizarán las siguientes fuentes de datos:

   * Datos catastrales de Madrid, obtenidos del catastro
   * Datos del precio de alquiler en Madrid, obtenidos de IVIMA
   * Información gráfica en formato "shape" de los barrios de Madrid

## Descripción de los datos utilizados

### Datos catastrales

Los datos catastrales se pueden descargar de la [web de catastro](https://www.sedecatastro.gob.es/OVCFrames.aspx?TIPO=TIT&a=masiv), accesibles con certificado digital, dentro de la sección descarga de información alfanumérica.

Los datos catastrales se procesarán para obtener la información de las viviendas de Madrid, relativa a:

* localización de las viviendas tanto alfanumérica (dirección) como geolocalización (coordenadas x,y)
* superficie
* presencia de garage
* altura
* antigüedad

### Datos con precios de alquiler

Para obtener los precios de alquiler de un conjunto de viviendas de Madrid, se utilizarán los datos del IVIMA (Instituto de la vivienda de Madrid), alojados en [plan alquila](http://www.planalquila.org/).
Este conjunto de datos contiene información de:

* localización de las viviendas
* superficie
* presencia de garage
* altura
* supercifie
* precio de alquiler

### Datos y geolocalización de barrios de Madrid

Se utiliza también una capa _shape_ con la geolocalización de los barrios de Madrid, alojada en [barrios Madrid](http://www.madrid.org/nomecalles/DescargaBDTCorte.icm), sección _Delimitaciones territoriales_, fichero _Barrios Madrid._

Este fichero se utilizará para asociar el barrio a cada una de las viviendas, así como para la representación del cuadro de mando final.

### Enriquecimiento y relación entre fuentes de datos

Con la finalidad de que el modelo predictivo sea lo más ajustado posible, se enriquecerán las distintas fuentes de información disponibles, añadiendo la siguiente información a cada una de ellas:

* datos catastrales. Barrio desde el shape de barrios
* datos IVIMA. Coordenadas (x,y), y antigüedad desde catastro; Barrio desde el shape de barrios


## Estructura de ficheros y directorios

Para el correcto funcionamiento del código, se ha definido estructura de directorios en la que se alojarán los distintos ficheros de datos, código y resultados. Dicha estructura se refleja en la siguiente imagen:

![sin titulo](./graphs/dirs_tree.png)


## Descripción del código



## Descripción y uso del Dashboard


