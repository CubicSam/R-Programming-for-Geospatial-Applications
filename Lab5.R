#Assessment of vegetation loss during the Creek Fire (2020): A MODIS time series analysis
#For rat
files<-list.files('.',pattern='EVI.tif*')
files
summary(files)
all_st<-stack(files)
all_st
summary(all_st)
crs(all_st)
extent(all_st)

plot(all_st)
plot(all_st, 
     zlim = c(1000, 6000), 
     nc = 3)
a<-raster('MOD13Q1.A2020273.h08v05.006.2020291074019_250m_16_days_EVI.tif')
plot(a)
rasterVis::rasterTheme(a)
b<-raster('MOD13Q1.A2020241.h08v05.006.2020261220548_250m_16_days_EVI.tif')
plot(b)
#We have 12 layers instead of 23

#build a new dataframe based on evi Rasterstack.
#getValues convert raster data to matrix
df_evi<-getValues(all_st)
class(df_evi)
dim(df_evi)
df_evi
summary(df_evi)
plot(df_evi[1,])
lines(df_evi[1,])

d_year<-seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month")
d_year
length(d_year)
length(df_evi[1,])
plot(d_year,df_evi[1,],type='l',xlab='month',ylab='EVI') #type=l for line

#For linear regression model
month<-1:12
plot(month,df_evi[1,],type='l',xlab='Month',ylab='EVI')
df<-data.frame(month,ndvi= df_evi[1,]) #where 2 cols are year-bvalue
df
lmodel<-lm(ndvi~month,data=df) 
summary(lmodel)
lmodel
lmodel$coefficients
abline(lmodel)

#change detection
library(changepoint)

plot(avalue[1,])
fit_changepoint = cpt.mean(df_evi[1,])
fit_changepoint #Identified the changing point which is 2
plot(fit_changepoint)
cp <- cpts(fit_changepoint)

#Slope
t<-1:12
df<-df_evi[!is.na(df_evi[,1]),]
dim(df)
dfsize<-dim(df)
#For a regression line, we need slope and intercept. 
output<-matrix(0,nrow=dfsize[1],ncol=2)
output
dim(output)

for (i in 1:dfsize[1]){
  y<-df[i,]
  lmodel<-lm(y~t)
  output[i,1:2]<-lmodel$coefficients
  print(i)
}
output
max(output[,2])
min(output[,2])

c<-raster('MODIS2020.tif')
extent(c)
crs(c)

c[!is.na(c)]<-output[,2]
plot(c)
b[!is.na(b)]<-output[,1]
mapview(c)
writeRaster(c,filename = "slope_CF.tif",overwrite=TRUE)
#For maximum point
#generate a simple dataframe with two columns: x and y (longitude and latitude)  
testpoint3 <- data.frame(x = -119.242277, y= 37.210865)
plot(testpoint3)

coordinates(testpoint3) <- ~ x + y
proj4string(testpoint3) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
crs(testpoint3)
plot(testpoint3)
lines(testpoint3)

all_st_pro<-projectRaster(all_st, crs = projection(testpoint3))
plot(all_st_pro,
     zlim = c(1500, 10000),
     nc = 4)

# Extract values from a Raster* object at a specific point location 
p<-extract(all_st_pro,testpoint)
plot(m)
plot(p[1,])
lines(p[1,])
d_year<-seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month")
plot(d_year,m[1,],type='l',xlab='month',ylab='EVI')

testpoint <- data.frame(x = -119.159513, y= 37.148727)
plot(testpoint)

coordinates(testpoint) <- ~ x + y
proj4string(testpoint) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

all_st_pro2<-projectRaster(all_st, crs = projection(testpoint))
plot(all_st_pro,
     zlim = c(1500, 10000),
     nc = 4)

# Extract values from a Raster* object at a specific point location 
m<-extract(all_st_pro2,testpoint)
plot(m)
plot(m[1,])
d_year<-seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month")
plot(d_year,p[1,],type='l',xlab='month',ylab='EVI')
