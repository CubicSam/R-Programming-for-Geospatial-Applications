#Time-series NDVI analysis

#generate a simple dataframe with two columns: x and y (longitude and latitude)  
testpoint <- data.frame(x = -80.429361, y = 37.229596)
testpoint
# set spatial coordinates to create a Spatial object
coordinates(testpoint) <- ~ x + y
# add GCS information to the spatial object 
proj4string(testpoint) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# plot the one point spatial object
plot(testpoint)


#Set directory to MODIS files
#List all evi files in the working directory
files<-list.files('.',pattern='EVI.tif*')
files
summary(files)
all_st<-stack(files)
all_st
summary(all_st)
crs(all_st)
extent(all_st)

#read an EVI file using raster()
g<-raster('./MOD13Q1.A2001001.h11v05.006.2015140082007_250m_16_days_blue_reflectance.tif')
plot(g)
m<-extract(g,testpoint)
m
plot(m)

#Projecting 'bb' shp as stacked ndvi
all_st_pro<-projectRaster(all_st, crs = projection(testpoint))
crs(all_st_pro2)
extent(all_st_pro2)
extent(testpoint2)
plot(all_st_pro,
     zlim = c(1500, 10000),
     nc = 4)

# Extract values from a Raster* object at a specific point location 
n<-extract(all_st_pro,testpoint)
n
summary(n)
plot(n)
plot(n[1,])
lines(n[1,])
plot(n,
     zlim = c(1500, 10000),
     nc = 4)

#Question2
#generate a simple dataframe with two columns: x and y (longitude and latitude)  
testpoint2 <- data.frame(x = -80.447, y = 37.2269)
testpoint2
coordinates(testpoint2) <- ~ x + y
proj4string(testpoint2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
plot(testpoint2)
extent(testpoint2)

#Set directory to MODIS files
#List all evi files in the working directory
files<-list.files('.',pattern='EVI.tif*')
files
summary(files)
all_st<-stack(files)
all_st
summary(all_st)
crs(all_st)
extent(all_st)



#Projecting 'bb' shp as stacked ndvi
all_st_pro2<-projectRaster(all_st, crs = projection(testpoint2))
crs(all_st_pro2)
extent(all_st_pro2)

# Extract values from a Raster* object at a specific point location 
q<-extract(all_st_pro2,testpoint2)
n
summary(o)
plot(q)
plot(q[1,])
lines(q[1,])

#Question3

#list all EVI.tif files for each year; stack EVI images; 
#Calculate mean value for each pixel; Write as a Raster tiff

for (year in 2001:2017) {
#Using wildcard to list all EVI files each year. First iteration run will give EVI files for 2001. 
#Next stack the EVI for each year(23 raster).

wildcard<-paste('*A',year,'*EVI.tif', sep='')
wildcard
#Paste is to connect strings. E.g. A2001*EVI.tif

files<-Sys.glob(wildcard)
files
length(files)
  
#Next stack the EVI for 2001 (23 raster) unlike 391
evi<-stack(files)
evi  #multi-layered raster

##Calc will calculate the mean value for 23 layers. It's the temporal avg.
m_evi <- calc(evi, fun = mean)

library(mapview)
mapview(m_evi)
  
outfile<-paste('MODIS',year,'.tif',sep='')
outfile
writeRaster(m_evi,outfile,overwrite=T)
print(year)
}


df<-data.frame(year,ndvi=bvalue[1,])

df<-data.frame()


d_month<-seq(as.Date("2001-01-01"), as.Date("2017-12-31"), by="months")
plot(d_month,a$pcp,type='l',xlab='year',ylab='ppt')


#change detection

df<-data.frame(year,ndvi=bvalue[1,]) #where 2 cols are year-bvalue
df
#For linear regression model
lmodel<-lm(ndvi~year,data=df) 
lmodel
lmodel$coefficients
abline(lmodel)


tp_proj<-spTransform(gg,crs(testpoint))
tp_proj


g_proj<-projectRaster(testpoint,gg,crs)
?projectRaster
avalue<-extract(m,a_proj)
plot(year,avalue[1,])
df<-data.frame(year,ndvi=avalue[1,])

