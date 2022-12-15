# Image classification with R
#Will show only the tiff files
files<-list.files('.',pattern = '.TIF$')
files<-files[4:9]
landsat<stack(files)
#Read Raster (composite bands from 4 to 9)
writeRaster(landsat,'landsat.tif')
#brick is similar to stack but takes shorter time

#Lab 4: To implement digital image classification with R

install.packages('randomForest')
library(randomForest)
library(rgdal)
library(raster)
#import any specific band as an image template
B_temp<-raster('landsat.tif',band=4)
plot(B_temp)
B_temp
#Import trainingsamples polygon using readOGR() 
train_poly<-readOGR(dsn='.',layer = 'trainingsamples')
train_poly$classid
#Rasterize the polygon using rasterize function into same dim as B_temp for consistency
t_ras<-rasterize(train_poly,B_temp, field=train_poly$classid)
plot(t_ras)
summary(t_ras)
#If t_raster[1,1]= N/A, all pixels NA except the training pixels
#import multi-layer raster using brick
landsat<-brick('landsat.tif')
#Attach one raster layer containing training data (t_ras) using add layer
alldata<-addLayer(landsat,t_ras)
alldata 
#There are 7 layers now
#convert raster layer to dataframe and add column names
alldata_df<-as.data.frame(alldata)
summary(alldata_df)
dim(alldata_df) #59k is the remaining/background pixel number
#Renaming column names
cols = c("band1","band2","band3","band4","band5","band6","y")
colnames(alldata_df)<-cols
alldata_df

#Extract a dataframe containing training data points only
alltraining<-alldata_df[!is.na(alldata_df$y),] #!is.na()= not equal to NA.To identify training pixels
dim(alltraining)
alltraining
summary(alltraining) #No more NA value available in y layer
length(t_ras)

#training classification using random forest
#First build a random forest classifier
rftree <- randomForest(as.factor(y) ~. , data = alltraining)
#as.factor() function convert y variable to R factors(no-ranking, just discreet). We need to categorize y value to four land cover classes
#~. means model y based on the dataframe alltraining. 
#data=dataframe
rftree
#Perform classification for the entire study site 
rftree.pred = predict(rftree, alldata_df)
#predict is the function, rftree is the learned data and we want to apply that to alldata_df(all pixels)
rftree.pred
length(rftree.pred)
#Every pixel has been assigned a class(1-5) based on rftree.This is how the land is classified. 

output = setValues(B_temp, rftree.pred) #B_temp has band 4. SetValues put all values in B_temp as rftree.pred
plot(output)
output
summary(output)
class(output)
output$Area<-gArea(output,byid=T)
writeRaster(output,filename = "landcover.rf.tif",overwrite=TRUE)

#Find the area in km2
#First need to find pixel number for each class

landsat<-raster('./landsat.tif')

length(output[output==1])
length(output[output==2])
length(output[output==3])
length(output[output==4])
#Now calculate the area (30*30/1000*1000)
length(output[output==1])*900/1000000

#Generate NDVI map

Band4<-raster('landsat.tif',band=4)
Band3<-raster('landsat.tif',band=3)
plot(Band4)
NDVI <- (Band4 - Band3) / (Band4 + Band3)
plot(NDVI)
