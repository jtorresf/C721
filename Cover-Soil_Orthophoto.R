
#Importación de Librerias
library(raster)
library(rgdal)
library(sp)

setwd("D:/001_Maestria/5. Cliclo 2019-I/2. Hidroinformatica/0_Proyecto/1. Gabinete/3. Orthophoto")

# Cargado Orthophoto
Combinado<-brick("orthophoto_Chucapaca.TIF")

# Lectura de shapefiles límite de la cuenca
cuenca <- readOGR("D:/001_Maestria/5. Cliclo 2019-I/2. Hidroinformatica/0_Proyecto/1. Gabinete/3. Orthophoto","Cuenca" )

# Recorte de la Orthophoto y Generación de una nueva Orhtophoto
Area_Cuenca <- crop(Combinado, extent(cuenca))
Area_Cuenca <- mask(Area_Cuenca, cuenca)
writeRaster(Area_Cuenca,"Area_Proyecto_v2.tif", drivername="GTiff", overwrite=TRUE)
Area_Cuenca <- brick("Area_Proyecto_v2.TIF")
plotRGB(Area_Cuenca, r=1, g=2, b=3, main="Orthophoto Original",cex.lab=1, cex.axis=1, cex.main=1 )

# Conversión del Raster a Vector
vr<-getValues(Area_Cuenca)
p<-which(!is.na(vr))

#Clasificación del tipo de cobertura de Suelo
km<-kmeans(na.omit(vr), centers=6, iter.max=100, nstart=10, algorithm="MacQueen")
km_class<-raster(Area_Cuenca)
km_class[p]<-km$cluster
Amarillo <-"#ffff00"
Rojo <-"#ff0000"
Verde <-"#00913f"
Gris <-"#9b9b9b"
Marron <-"#804000"
Naranja <-"#ff6600"
mycolor<-c(Amarillo, Rojo, Verde, Gris, Marron, Naranja)
plot(km_class, main="Clasificación con K-means", col=mycolor,cex.lab=1, cex.axis=1, cex.main=1)
writeRaster(km_class,"Soil_Cover_v4.tif", drivername="GTiff", overwrite=TRUE)
Cuenca_Result<-raster("Soil_Cover_v4.TIF")
plot(Cuenca_Result, main="Clasificación con K-means", col=mycolor, cex.lab=1, cex.axis=1, cex.main=1)

#Estadistica del análisis
vector<-c(0,0,0,0,0,0)
for(i in 1:6){
  Cuenca_Result<-raster("Soil_Cover_v4.TIF")
  Cuenca_Result[Cuenca_Result>i]<-NA
  Cuenca_Result[Cuenca_Result<=i]<-1
  vector[i]<-cellStats(Cuenca_Result, stat="sum")
}
suma_total<-vector[6]
for(i in 6:2){
  vector[i]<-vector[i]-vector[i-1]
}
porcentaje<-vector/suma_total*100
vector
porcentaje
cumsum(porcentaje)

