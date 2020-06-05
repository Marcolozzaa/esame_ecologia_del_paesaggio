######### DATI EARTHSTAT

# uso dati del soy beans


library(sp)
library(raster)
install.packages("rgdal")
library(rgdal)




#### FUNZIONE DI UNIRE LE COASTLINES



# metto la work direct. sulla cratella con lo shape file

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/ne_10m_coastline")

coastline <- readOGR("ne_10m_coastline.shp")

# ri imposto la work direct. sulla cartella SOYBEANS

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean")

soy_1995 <- raster("Soybean_1995_Yield.tif")

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)

plot(soy_1995,col=cl,zlim=c(0,5))

plot(coastline,add=T)   # aggiungo le coastlines all'immagine raster





#### FUNZIONE DIFFERENZA


soy_1995 <- raster("Soybean_1995_Yield.tif")
soy_2005 <- raster("Soybean_2005_Yield.tif")

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)


diff <- soy_2005-soy_1995

plot(diff,col=cl,zlim=c(0,5))
plot(coastline,add=T)


#### FUNZIONE DI CARICAMENTO TUTTI I DATI ASSIEME





##### FUNZIONE CROP



###  funzione CROP della zona del SUD AMERICA


extension <- c(-100, -20, -60, 20)   # zona SUD AMERICA

soy_1995_SA <- crop(soy_1995, extension)
soy_2005_SA <- crop(soy_2005, extension)

plot(soy_1995_SA,col=cl,zlim=c(0,5))
plot(coastline,add=T)   # vedo se funziona


par(mfrow=c(1,2))                       # le metto una di finaco all'altra
plot(soy_1995_SA,col=cl,zlim=c(0,5))
plot(coastline,add=T)
plot(soy_2005_SA,col=cl,zlim=c(0,5))
plot(coastline,add=T)




###### PROVO CON LE PATCHES


# posso fare la differenza dei due crop in SUD AMERICA

diff_soy_SA <- soy_2005_SA - soy_1995_SA
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)

plot(diff_soy_SA,col=cl,zlim=c(0,5))
plot(coastline,add=T)
















#### BOXPLOT 

boxplot(soy_1995) # cosi và, ma sarebbe da cambiare i valori 














