install.packages("raster")
install.packages("sp")
install.packages("maptools")
install.packages("rgdal")
#cargar librerias
library(raster)
library(sp)
library(maptools)
library(rgdal)
#asignar directorio
setwd("C:/Users/manuel/Documents/UABCS/Rasters")
list.files()
#Cargar Raster y plot
BO2<-raster("BO2_carbonphytoltmin_bdmean_lonlat.tif")
plot(BO2)
#corte al area de estudio con archivo .asc
r = raster("C:/Users/manuel/Documents/UABCS/Rasters/asc/alk_max.asc")
plot(r)
#cortar el poligono
BO2_r<-crop(BO2,r)
plot(BO2_r)
#plotear raster con poligono
plot(BO2_r)
plot(r,add=TRUE)
#Asignar sistema de coordenada de refrencia CRS
proj4string(BO2_r) = CRS("+proj=lcc +lat_1=49.5 +lat_0=49.5 +lon_0=0 +k_0=0.999877341 +x_0=6 +y_0=2 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs")
proj4string(BO2_r)
BO2_r_crs <- projectRaster(BO2_r,crs = crs(BO2_r))
crs(BO2_r_crs)

#reasmple metodo bilinear
r_resam <- resample(BO2,r,method='bilinear')
plot(r_resam)

####funcione para el mapa ##

En  "function(x,y,)" nunca debes de tener una coma sin nada despues

Esto

x= BO2<-raster("BO2_carbonphytoltmin_bdmean_lonlat.tif")
y= r = raster("C:/Users/manuel/Documents/UABCS/Rasters/asc/alk_max.asc")

dentro de la funcion esta mal, nunca debes especificar que significan los argumentos dentro de la funcion. 
La idea de la funcion es que puedas generalizar y aplicarla a muchos casos

Aqui en estas lineas no debes de tener 2 "=" en una linea, tampoco un "<-" y "="
x= BO2<-raster("BO2_carbonphytoltmin_bdmean_lonlat.tif")
y= r = raster("C:/Users/manuel/Documents/UABCS/Rasters/asc/alk_max.asc")

Nombra tus funciones de manera clara, en base a lo que hacen

cortar_raster <- function(x,y,myCRS) {
  x_r<-crop(x,r)
  plot(x_r)
  plot(r,add=TRUE)
  proj4string(x_r) = CRS(myCRS)
  x_r_crs <- projectRaster(x_r,crs = crs(x_r))
  crs(x_r_crs)
  r_resam <- resample(x,r,method='bilinear')
  plot(r_resam)
  return(r_resam)
}

x <- raster("BO2_carbonphytoltmin_bdmean_lonlat.tif")

y <- raster("C:/Users/manuel/Documents/UABCS/Rasters/asc/alk_max.asc")

myCRS="+proj=lcc +lat_1=49.5 +lat_0=49.5 +lon_0=0 +k_0=0.999877341 +x_0=6 +y_0=2 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"

cortar_raster(x,y,myCRS)




