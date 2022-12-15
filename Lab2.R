#Characterizing Wildland–Urban Interface in Virginia (Rscript)

library(GISTools)
#Read nlcd raster file
nlcd<-raster('./raster/nlcd.tif')
plot(nlcd)
nlcd
crs(nlcd)
#Read vector data(shpfile)
pophu<-readOGR('./shapefile',layer='y2010_51_pophu')
crs(pophu)
#Project pophu layer to nlcd arster(UTM17N)
pophu_p <- spTransform(pophu, crs(nlcd))

attribute.table<-data.frame(pophu_p)
attribute.table

#Find housing density for each census block
#First find the area in km2
pophu_p$Area<-gArea(pophu_p,byid=T)/1000000
pophu_p$hd<-pophu_p$HOUSING10/pophu_p$Area
summary(pophu_p)
data.frame(pophu_p)
choropleth(pophu_p,pophu_p$hd)

#Reclassify NLCD map to 2 classes: Vegetation(code=1)& Non-vegetation(code=0)
nlcd[nlcd==41|nlcd==42|nlcd==43|nlcd==52|nlcd==71|nlcd==90|nlcd==95]=1
nlcd[nlcd!=1]=0
plot(nlcd)

#Calculate percent of vegetation within each census block
#Need to convert shp to raster using rasterize functio. First, add a field of poly_id to shp
pophu_p$poly_id <-1:length(pophu_p)
pophu_raster <- rasterize(pophu_p, nlcd, field = pophu_p$poly_id)
plot(pophu_raster)

#Use zonal statistics, zonal(), to calculate percent of vegetation in each census block
veg_percent<-zonal(nlcd, pophu_raster, fun='mean')
veg_percent
colnames(veg_percent)
summary(pophu_p)

#Change column name 'zone' to 'newid'
colnames(veg_percent)[1]<-'poly_id'
colnames(veg_percent)[2]<-'pveg'

#Join veg_percent to pophu_p using merge function
output<-merge(pophu_p,veg_percent,by='poly_id')
output$pveg

#We can recode these NA value as -9999 using 
output$pveg[is.na(output$pveg)]<- -9999

#Export output as a shape file
writeOGR(obj=output, dsn=".", layer='output', driver="ESRI Shapefile",overwrite_layer=T)
 #Identify WUI census blocks
wui<-output[output$pveg>0.5&output$ hd>6.17,]
#Plot and export
plot(nlcd)
plot(wui,add=TRUE)
writeOGR(obj=wui, dsn=".", layer='wui', driver="ESRI Shapefile",overwrite_layer=T)
summary(wui)
plot(wui)

# WUI map for Albemarle County

VA_ras<-raster('./raster/Albemarle')
albe<-readOGR('./shapefile',layer='Albemarle')
summary(VA)
albe_p <- spTransform(albe, crs(VA_ras))
summary(VA_p)
#
albe<-VA_p[VA_p$COUNTYFP10==003]
plot(albe)
plot(nlcd_va_utm17)
summary(albe_p)
#
albe_p$Area<-gArea(albe_p,byid=T)/1000000
albe_p$hd<-albe_p$HOUSING10/albe_p$Area
#
VA_ras[VA_ras==41|VA_ras==42|VA_ras==43|VA_ras==52|VA_ras==71|VA_ras==90|VA_ras==95]=1
VA_ras[VA_ras!=1]=0
#
albe_p$poly_id <-1:length(albe_p)
albe_raster <- rasterize(albe_p, VA_ras, field = albe_p$poly_id)
plot(albe_p)

#

veg_mean<-zonal(VA_ras, albe_raster, fun='mean')

colnames(veg_mean)[1]<-'poly_id'
colnames(veg_mean)[2]<-'pveg'
#
output<-merge(albe_p,veg_mean,by='poly_id')
output$pveg[is.na(output$pveg)]<- -9999
#
albe_wui<-output[output$pveg>0.5&output$ hd>6.17,]
plot(albe_wui)
writeOGR(obj=albe_wui, dsn=".", layer='albe_wui', driver="ESRI Shapefile",overwrite_layer=T)
plot(VA_ras)
plot(albe_wui, add=TRUE)