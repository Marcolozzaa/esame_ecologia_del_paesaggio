######### ESAME DI ECOLOGIA DEL PAESAGGIO, 2020/21                           MARCO LOZZA

# dati presi dal sito di EARTHSTAT
# dati rappresentanti la resa media in tonnellate per ettaro durante il periodo che va dal 1997 al 2003 



# 1. FUNZIONE DI CARICAMENTO IMMAGINI RASTER CON LAPPLY E STACK 

# 2. FUNZIONE PER UNIRE LE COASTLINES

# 3. FUNZIONE RASTER : CARICO LE IMMAGINI SINGOLARMENTE

# 4. FUNZIONE GGPLOT2 : GRAFICO COLONNE (PIXELS E TONNELLATE PER ETTARO)  

# 5. FUNZIONE BOXPLOT   

# 6. FUNZIONE DIFFERENZA      

# 7. FUNZIONE CROP     

# 8. FUNZIONE DIFFERENZA CON I CROP DEL SUD AMERICA   

# 9. FUNZIONE GGPLOT GRAFICO COLONNEDEL SUD AMERICA(1995-2005)  





# pacchetti necessari per procedere


library(sp)

install.packages("raster") 
library(raster)

install.packages("rgdal")          
library(rgdal)

install.packages("gglpot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("dismo")
library(dismo)
                       

  


#### 1. FUNZIONE DI CARICAMENTO IMMAGINI RASTER CON LAPPLY E STACK


setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean/SOIA YIELD TIF")
rlist <- list.files(pattern=".tif", full.names=T)
list_rast <- lapply(rlist, raster)
soy_yiest.multitemp <- stack(list_rast)
plot(soy_yiest.multitemp)

# imposto una color ramp palette e do il limite di valori da 0 a 4

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(soy_yiest.multitemp,col=cl,zlim=c(0,4))`

# il problema è che le mappe non hanno le coastlines 
# aggiungo le coastlines alle mappe






#### 2. FUNZIONE PER UNIRE LE COASTLINES


# imposto la work direct. sulla cratella con lo shape file

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/ne_10m_coastline")

coastline <- readOGR("ne_10m_coastline.shp")

# ri imposto la work direct. sulla cartella SOYBEANS

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean")







#### 3. FUNZIONE RASTER : CARICO LE IMMAGINI SINGOLARMENTE                



# questo perchè lo shape file non si attacca ad uno stack file ma solo alle immagini raster singole

setwd("~/Desktop/ESAME COPERNICUS/HarvAreaYield_4Crops_95-00-05_Geotiff/Soybean")
soy_1995 <- raster("Soybean_1995_Yield.tif")
soy_2000 <- raster("Soybean_2000_Yield.tif")
soy_2005 <- raster("Soybean_2005_Yield.tif")
# cerco su internet tutto l'arrange dei colori di R e cerco i colori giusti 
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)

# plotto le immagini
plot(soy_1995,col=cl)
# cambio la zlim per avere le aree meglio rappresentate e aggiungo il titolo
plot(soy_1995,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 1995") 

par(mfrow=c(2,2))
# le plotto tutte con lo stesso colore, stesso zlim e col titolo
plot(soy_1995,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 1995")
plot(soy_2000,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 2000")
plot(soy_2005,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 2005")  # le mappe pero non hanno le coastlines, le aggiungo

dev.off()

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(soy_1995,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 1995")

plot(coastline,add=T)   # aggiungo le coastlines all'immagine raster e lo posso fare perche sono entrambe WGS84

# cambio pero la larghezza delle linee delle coaste per farle piu sottili con "lwd"

plot(coastline,lwd=0.3)

plot(soy_1995,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 1995")
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
plot(soy_1995,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 1995")  # riplotto tutto insieme aggiungendo le coastlines
plot(coastline,lwd=0.3,add=T)
plot(soy_2000,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 2000")
plot(coastline,lwd=0.3,add=T)
plot(soy_2005,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD 2005")
plot(coastline,lwd=0.3,add=T)









#### 4. FUNZIONE GGPLOT2 : GRAFICO COLONNE (PIXELS E TONNELLATE PER ETTARO)  


install.packages("gglpot2")
library(ggplot2)

# con la funzione "freq" creo un frequency table dove mi fa il conto di tutti i pixel con uguali valori
freq(soy_1995)
# salvo la tabella come insieme di dati
fr1995 <- freq(soy_1995) # guardo la tabella e vedo i valori come sono organizzati
View(fr1995)

# mi creo una tabella con i valori che riporto anche sul grafico da 0 a 4 (dove ho la maggior parte dei dati)

tons_per_hectare <- c(1,2,3,4)
n.pixels <- c(608400,187465,66019,6231)
# creo la tabella
tons_per_hectare_1995 <- data.frame(tons_per_hectare,n.pixels)
View(tons_per_hectare_1995)
# creo le basi per il grafico del 1995
ggplot1995 <- ggplot(tons_per_hectare_1995, aes(x=tons_per_hectare,y=n.pixels)) + geom_bar(stat="identity",fill="white")
plot(ggplot1995)

# cambio colore delle colonne in giallo,aggiungo i limiti sulla y, e il titolo e cambio anche i nomi della x e della y

ggplot1995 <- ggplot(tons_per_hectare_1995, aes(x=tons_per_hectare,y=n.pixels)) + 
geom_bar(stat="identity", fill="darkgoldenrod1") +
ylim(0, 700000) +
labs(title="TONS PER HECTARE 1995",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")


# faccio la stessa cosa ma per il 2005

# con la funzione "freq" creo un frequency table dove mi fa il conto di tutti i pixel con uguali valori
freq(soy_2005)
fr1995 <- freq(soy_2005) # guardo la tabella e vedo i valori come sono organizzati
View(fr2005)

tons_per_hectare2 <- c(1,2,3,4)
n.pixels2 <- c(569464,223823,107568,10598)
tons_per_hectare_2005 <- data.frame(tons_per_hectare2,n.pixels2)
View(tons_per_hectare_2005)

ggplot2005 <- ggplot(tons_per_hectare_2005, aes(x=tons_per_hectare2,y=n.pixels2)) + 
geom_bar(stat="identity",fill="darkgoldenrod1") +
ylim(0, 700000) +
labs(title="TONS PER HECTARE 2005",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")


# Metto i due grafici appena ottenuti sulla stessa riga 

install.packages("gridExtra")
library(gridExtra)

# grid.arrange(plot1,plot2,nrow=1) = due grafici nella stessa finestra 

grid.arrange(ggplot1995,ggplot2005,nrow=1)


## Qua vedo come in 10 anni sono aumentate le tonnellate per ettaro
# gli ettari dove si produce solo 1 tonnellata sono diminuiti e dove se ne produono di piu di tonnellate sono aumentati










#### 5. FUNZIONE BOXPLOT 

boxplot(soy_1995, horizontal=T,outline=F,axes=T)
boxplot(soy_2005, horizontal=T,outline=F,axes=T)

# con main aggiungo il titolo e le metto con una par una sotto all'altra

par(mfrow=c(2,1))
boxplot(soy_1995, horizontal=T,outline=F,axes=T,main="SOY BEAN YIELD 1995")
boxplot(soy_2005, horizontal=T,outline=F,axes=T,main="SOY BEAN YIELD 2005")










#### 6. FUNZIONE DIFFERENZA   


diff <- soy_2005-soy_1995

# aggiungo al plot dell'immagine la color palette, il titolo e il limite dei valori con zlim
# aggiungo anche le coastlines

cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)
plot(diff,col=cl,zlim=c(0,4),main="GLOBAL SOY BEANS YIELD DIFFERENCE FROM 1995 TO 2005")
plot(coastline,lwd=0.3,add=T)










##### 7. FUNZIONE CROP      
                           

###  funzione CROP della zona del SUD AMERICA


extension <- c(-100, -20, -60, 20)   # zona SUD AMERICA

soy_1995_SA <- crop(soy_1995, extension)
soy_2000_SA <- crop(soy_2000, extension)
soy_2005_SA <- crop(soy_2005, extension)

# le plotto tutte insieme e aggiungo la color ramp, il limite zlim e il titolo
par(mfrow=c(2,2))  
plot(soy_1995_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD 1995")
plot(coastline,lwd=0.3,add=T) 
plot(soy_2000_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD 2000")
plot(coastline,lwd=0.3,add=T) 
plot(soy_2005_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD 2005")
plot(coastline,lwd=0.3,add=T)

# metto le mappe del 1995 e del 2005 una di finaco all'altra e aggiungo il titolo al grafico
par(mfrow=c(1,2))                       
plot(soy_1995_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD 1995")
plot(coastline,lwd=0.3,add=T)

plot(soy_2005_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD 2005")
plot(coastline,lwd=0.3,add=T)










###### 8. FUNZIONE DIFFERENZA CON I CROP DEL SUD AMERICA


# posso fare la differenza dei due crop in SUD AMERICA

diff_soy_SA <- soy_2005_SA - soy_1995_SA
cl <- colorRampPalette(c('aliceblue','darkgoldenrod1','darkgoldenrod4'))(100)  

# aggiungo sempre lo stesso zlim e il titolo del grafico
plot(diff_soy_SA,col=cl,zlim=c(0,4),main="SOUTH AMERICA SOY BEANS YIELD DIFFERENCE FROM 1995 TO 2005")
plot(coastline,lwd=0.3,add=T)











#### 9. FUNZIONE GGPLOT GRAFICO COLONNEDEL SUD AMERICA(1995-2005)

# con la funzione "freq" creo un frequency table dove mi fa il conto di tutti i pixel con uguali valori
freq.SA.1995 <- freq(soy_1995_SA)
freq.SA.2005 <- freq(soy_2005_SA) # guardo le tabella e vedo i valori come sono organizzati
 
View(freq.SA.1995)
View(freq.SA.2005)


# mi creo una tabella con i valori che riporto anche sul grafico da 0 a 4 (dove ho la maggior parte dei dati)
# tabella dei valori per il raster Sud America 1995


tons_per_hectare_SA1995 <- c(1,2,3,4)
n.pixels_SA1995 <- c(7901,45675,6078,196)

# creo la tabella
tons_per_hectare_1995SA <- data.frame(tons_per_hectare_SA1995,n.pixels_SA1995)
View(tons_per_hectare_1995SA)
# creo le basi per il grafico del 1995 del Sud America
ggplot1995SA <- ggplot(tons_per_hectare_1995SA, aes(x=tons_per_hectare_SA1995,y=n.pixels_SA1995)) + geom_bar(stat="identity",fill="white")
plot(ggplot1995SA)

# cambio colore delle colonne in giallo,aggiungo i limiti sulla y, e il titolo

ggplot1995SA <- ggplot(tons_per_hectare_1995SA, aes(x=tons_per_hectare_SA1995,y=n.pixels_SA1995) + 
geom_bar(stat="identity", fill="goldenrod") +
ylim(0, 60000) +
labs(title="TONS/HECTARE 1995 SOUTH AMERICA",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")
                       
# stessa funzione sopra ma R si vede che la prende solo scritta cosi tutta dritta                                         
ggplot1995SA <- ggplot(tons_per_hectare_1995SA, aes(x=tons_per_hectare_SA1995,y=n.pixels_SA1995)) + geom_bar(stat="identity",fill="goldenrod") + ylim(0, 60000) + labs(title="TONS/HECTARE 1995 SOUTH AMERICA",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")
plot(ggplot1995SA)

# faccio la stessa cosa ma per il 2005

View(freq.SA.2005)

tons_per_hectare_SA2005 <- c(1,2,3,4)
n.pixels_SA2005 <- c(10518,47556,25038,312)

# creo la tabella
tons_per_hectare_2005SA <- data.frame(tons_per_hectare_SA2005,n.pixels_SA2005)
View(tons_per_hectare_2005SA)
# creo le basi per il grafico del 20055 del Sud America
ggplot2005SA <- ggplot(tons_per_hectare_2005SA, aes(x=tons_per_hectare_SA2005,y=n.pixels_SA2005)) + geom_bar(stat="identity",fill="white")
plot(ggplot2005SA)

# cambio colore delle colonne in giallo,aggiungo i limiti sulla y, e il titolo e cambio il nome delle x e delle y

ggplot2005SA <- ggplot(tons_per_hectare_2005SA, aes(x=tons_per_hectare_SA2005,y=n.pixels_SA2005) + 
geom_bar(stat="identity", fill="goldenrod") +
ylim(0, 60000) +
labs(title="TONS/HECTARE 2005 SOUTH AMERICA",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")
                       
# stessa funzione ma R si vede che la prende solo scritta cosi tutta dritta    
ggplot2005SA <- ggplot(tons_per_hectare_2005SA, aes(x=tons_per_hectare_SA2005,y=n.pixels_SA2005)) + geom_bar(stat="identity",fill="goldenrod") + ylim(0, 60000) + labs(title="TONS/HECTARE 2005 SOUTH AMERICA",x = "TONS/HECTARE",y="N. OF PIXELS (SINGOL CELL AREA 10 km2)")


# Metto i due grafici appena ottenuti sulla stessa riga 

install.packages("gridExtra")
library("gridExtra")

# grid.arrange(plot1,plot2,nrow=1) = due grafici nella stessa finestra 

grid.arrange(ggplot1995SA,ggplot2005SA,nrow=1)










                       
#### PROVO FUNZIONE PER CREARE UN DATASET DI RANDOM DATA

install.packages("dismo")
library(dismo)

# points presence, 1000 punti random 

points.p <- randomPoints(soy_1995, 1000 )

# plotto i punti sulla mappa (pch=20 puntini neri, cex= dimensione punti)
plot(points,pch=20,cex=0.1)

# plotto la mappa e sopra i punti
plot(soy_1995_brick,zlim=c(0,4),col=cl)
points(points,pch=20,cex=0.1)










