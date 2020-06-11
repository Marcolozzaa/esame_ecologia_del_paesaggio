######### DATI EARTHSTAT

#immagini raster, ogni pixel è correlato ad un parametro geografico (ogni punto nel file è un punto sulla terra)
#come datum, ellissoide, proiezione (WGS84)

# uso dati del soy beans

# SONO TONNELLATA PER ETTARO. PER CUI NON MI DA I PAESI CHE NE PRODUCONO DI PIU, MA MI DA I PAESI DOVE PER ETTARE NE VIENE PRODOTTO DI PIU

library(sp)
library(raster)
install.packages("rgdal")
library(rgdal)



                        # aggiungo nomi-titoli(main=...) e didascalie ai grafici che produco

               
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
                           # potrei farla anche sul Nord America dove è aumentata la produzione


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
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)  # magari provo a cambiare palette per la diff?

plot(diff_soy_SA,col=cl,zlim=c(0,4))
plot(coastline,lwd=0.3,add=T)








#### FUNZIONE DI CARICAMENTO TUTTI I DATI ASSIEME, don't need them

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean/SOIA YIELD TIF")
rlist <- list.files(pattern=".tif", full.names=T)
list_rast <- lapply(rlist, raster)
soy_yiest.multitemp <- stack(list_rast)
plot(soy_yiest.multitemp)

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(soy_yiest.multitemp,col=cl,zlim=c(0,4))




###### PROVO CON LE PATCHES








#### BOXPLOT 

boxplot(soy_1995, horizontal=T,outline=F,axes=T)
boxplot(soy_2005, horizontal=T,outline=F,axes=T)

# con main aggiungo il titolo e le metto con una par una sotto all'altra

par(mfrow=c(2,1))
boxplot(soy_1995, horizontal=T,outline=F,axes=T,main="boxplot 1995")
boxplot(soy_2005, horizontal=T,outline=F,axes=T,main="boxplot 2005")






#### GRAFICO COLONNE GGPLOT2 PLOTTANDO ETTARI E TONNELLATE PER ETTARO


install.packages("gglpot2")
library(ggplot2)

freq(soy_1995)
fr1995 <- freq(soy_1995) # guardo la tabella e vedo i valori come sono organizzati
View(fr1995)

# mi creo una tabella con i valori che riporto anche sul grafico da 0 a 4 (dove ho la maggior parte dei dati)

tons_per_hectare <- c(0,1,2,3,4)
hectare <- c(24891,608400,187465,66019,6231)
# creo la tabella
tons_per_hectare_1995 <- data.frame(tons_per_hectare,hectare)
View(tons_per_hectare_1995)
# creo le basi per il grafico del 1995
ggplot1995 <- ggplot(tons_per_hectare_1995, aes(x=tons_per_hectare,y=hectare)) + geom_bar(stat="identity",fill="white")
plot(ggplot1995)

# cambio colore delle colonne in giallo,aggiungo i limiti sulla y, e il titolo

ggplot1995 <- ggplot(tons_per_hectare_1995, aes(x=tons_per_hectare,y=hectare)) + 
geom_bar(stat="identity", fill="darkgoldenrod1") +
ylim(0, 700000) +
labs(title="TONS PER HECTARE 1995")


# faccio la stessa cosa ma per il 2005

freq(soy_2005)
fr1995 <- freq(soy_2005) # guardo la tabella e vedo i valori come sono organizzati
View(fr2005)

tons_per_hectare2 <- c(0,1,2,3,4)
hectare2 <- c(21734,569464,223823,107568,10598)
tons_per_hectare_2005 <- data.frame(tons_per_hectare2,hectare2)
View(tons_per_hectare_2005)

ggplot2005 <- ggplot(tons_per_hectare_2005, aes(x=tons_per_hectare2,y=hectare2)) + 
geom_bar(stat="identity",fill="darkgoldenrod1") +
ylim(0, 700000) +
labs(title="TONS PER HECTARE 2005")


# Metto i due grafici appena ottenuti sulla stessa riga 

install.packages("gridExtra")
library("gridExtra")

# grid.arrange(plot1,plot2,nrow=1) = due grafici nella stessa finestra 

grid.arrange(ggplot1995,ggplot2005,nrow=1)


## Qua vedo come in 10 anni sono aumentate le tonnellate per ettaro
# gli ettari dove si produce solo 1 tonnellata sono diminuiti e dove se ne produono di piu di tonnellate sono aumentati








#### PROVO UNA FIGATA    RANDOM DATA

install.packages("dismo")
library(dismo)

# points presence, 1000 punti random 

points.p <- randomPoints(soy_1995, 1000 )

# plotto i punti sulla mappa (pch=20 puntini neri, cex= dimensione punti)
plot(points,pch=20,cex=0.1)

# plotto la mappa e sopra i punti
plot(soy_1995_brick,zlim=c(0,4),col=cl)
points(points,pch=20,cex=0.1)










