#Fire Fuel Mapping and Modeling in a Forested Environment 

library(rgdal)
library(raster)
library(GISTools)

ndvi<-raster('ndvi.tif')
ndvi
plot(ndvi)
#High biomass, NDVI values >= 200 
ndvi[ndvi<200]<-0
ndvi[ndvi!=0]<-1
plot(ndvi)
#High forest fule in landcoverc
lc<-raster('landcover.tif')
plot(lc)
unique(lc)
lc[lc<=7]<-0
lc[lc!=0]<-1
plot(lc)
#Steeper slopes (slope greater than 35 degrees)
elev<-raster('elev.tif')
slope<-terrain(elev, opt='slope',unit='degrees')
slope[slope<=35]<-0
slope[slope!=0]<-1
plot(slope)
#Aspect (southwest direction)
aspect<-terrain(elev, opt='aspect',unit='degrees')
aspect[aspect<202.5|aspect>247.5]<-0
aspect[aspect!=0]<-1
plot(aspect)
#Areas along hiking trails (within 150m)
road<-readOGR('.', layer='roads')
road_bf<-buffer(road,150)
plot(road_bf, add=T)
road_ras<-rasterize(road_bf,elev)
road_ras[is.na(road_ras)]<-0
plot(road_ras)
#Areas within 500 meters water bodies
water<-raster('landcover.tif')
water[water!=1]<-NA
plot(water)
dist<-distance(water)
dist[dist<=500]<-0
dist[dist!=0]<-1
plot(dist)
#Crop all layers(same dim)
ndvi<-crop(ndvi,elev)
lc<-crop(lc,elev)
slope<-crop(slope,elev)
aspect<-crop(aspect,elev)
roads<-crop(road_ras,elev)
dist<-crop(dist,elev)

all<-ndvi+lc+slope+aspect+roads+dist
plot(all)
