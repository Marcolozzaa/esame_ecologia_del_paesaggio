######### DATI EARTHSTAT

# uso dati del soy beans


library(sp)
library(raster)
install.packages("rgdal")
library(rgdal)






#### CARICO LE IMMAGINI                  zlim, è 0,5 giusto? si puo fare di meglio? IO FAREI 4


setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean")
soy_1995 <- raster("Soybean_1995_Yield.tif")
soy_2000 <- raster("Soybean_2000_Yield.tif")
soy_2005 <- raster("Soybean_2005_Yield.tif")
# cerco su internet tutto l'arrange dei colori di R e cerco i colori giusti 
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)

# plotto le immagini
plot(soy_1995,col=cl)
plot(soy_1995,col=cl,zlim=c(0,4)) # cambio la zlim per avere le aree meglio rappresentate

par(mfrow=c(2,2))
plot(soy_1995,col=cl,zlim=c(0,4))
plot(soy_2000,col=cl,zlim=c(0,4))
plot(soy_2005,col=cl,zlim=c(0,4))  # le mappe pero non hanno le coastlines, le aggiungo

dev.off()








#### FUNZIONE PER UNIRE LE COASTLINES



# metto la work direct. sulla cratella con lo shape file

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/ne_10m_coastline")

coastline <- readOGR("ne_10m_coastline.shp")

# ri imposto la work direct. sulla cartella SOYBEANS

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean")

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(soy_1995,col=cl,zlim=c(0,4))

plot(coastline,add=T)   # aggiungo le coastlines all'immagine raster e lo posso fare perche sono entrambe WGS84

# cambio pero la larghezza delle linee delle coaste per farle piu sottili con "lwd"

plot(coastline,lwd=0.3)

plot(soy_1995,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)


# le coastlines e le immagini tif pero hanno leggermente degli extent diversi per cui le allineo

coastlines  # extension : -180, 180, -85.22194, 83.6341  (xmin, xmax, ymin, ymax)
soy_1995    # extension : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)

# creo una nuova extension da applicare a tutte le immagini raster

extension <- c(-180, 180, -85.22194, 83.6341)
soy_1995 <- crop(soy_1995, extension) # mantengo i nomi originali ma li cambio l'estensione
soy_2000 <- crop(soy_2000, extension)
soy_2005 <- crop(soy_2005, extension)

par(mfrow=c(2,2))
plot(soy_1995,col=cl,zlim=c(0,4))  # riplotto tutto insieme aggiungendo le coastlines
plot(coastline,lwd=0.3,add=T)
plot(soy_2000,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)
plot(soy_2005,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)








#### FUNZIONE DIFFERENZA


diff <- soy_2005-soy_1995

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(diff,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)








##### FUNZIONE CROP



###  funzione CROP della zona del SUD AMERICA


extension <- c(-100, -20, -60, 20)   # zona SUD AMERICA

soy_1995_SA <- crop(soy_1995, extension)
soy_2000_SA <- crop(soy_2000, extension)
soy_2005_SA <- crop(soy_2005, extension)

plot(soy_1995_SA,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)   # vedo se funziona


par(mfrow=c(1,2))                       # le metto una di finaco all'altra
plot(soy_1995_SA,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)
plot(soy_2005_SA,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)




###### DIFFERENZA CON I CROP DEL SUD AMERICA


# posso fare la differenza dei due crop in SUD AMERICA

diff_soy_SA <- soy_2005_SA - soy_1995_SA
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)

plot(diff_soy_SA,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)








#### FUNZIONE DI CARICAMENTO TUTTI I DATI ASSIEME


###### PROVO CON LE PATCHES


#### BOXPLOT 

boxplot(soy_1995) # cosi và, ma sarebbe da cambiare i valori 

#### altre funzioni nel pacchetto raster??






















