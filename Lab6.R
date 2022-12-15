#logistic regression for urban growth simulation
library(raster)
library(rgdal)

#read pcp data (.csv)
a<-read.csv('pptmonthly.csv')
a
summary(a)
#seq(from, to, by, .) where by is increment. This makes the column ready for plot
d_month<-seq(as.Date("2001-01-01"), as.Date("2019-12-31"), by="months")
d_month
plot(d_month,a$pcp,type='l',xlab='year',ylab='ppt') #type=l for line
points(d_month,a$pcp)

#get PRISM temperature data and plot time-series
library(prism)
options(prism.path = ".") #downloaded data in current directory
prism::get_prism_monthlys(type = "tmean", years = 2015:2016, mon = 1:12, keepZip = F)
files<-prism_archive_ls()
pcp<-pd_stack(files) #all files stacked
?stack
pcp

#read a shapefile (one point location) and extract pcp values for this location
a<-readOGR('.',layer='blacksburg')
a
avalue<-extract(pcp,a)
avalue
plot(avalue[1,])

#trend analysis; We have 9 year's NDVI data from 2001 to 2009;
files<-list.files('.',pattern='ndvi*')
m<-stack(files)
m

#select a pixel location(row=40,col=60); extract ndvi values for this location point (another extraction method)
bvalue<-m[40,60,]
year<-1:9
plot(year,bvalue[1,])
df<-data.frame(year,ndvi=bvalue[1,]) #where 2 cols are year-bvalue
df
#For linear regression model
lmodel<-lm(ndvi~year,data=df) 
lmodel
lmodel$coefficients
abline(lmodel)


#Another location 
a<-readOGR('.',layer='blacksburg')
a
#Projecting 'bb' shp as stacked ndvi
a_proj<-spTransform(a,crs(m))
avalue<-extract(m,a_proj)
plot(year,avalue[1,])
df<-data.frame(year,ndvi=avalue[1,])
lmodel<-lm(ndvi~year,data=df)
lmodel$coefficients
abline(lmodel)

#Sen's slope
install.packages("trend")
library(trend)
sens.slope(avalue[1,], conf.level = 0.95)
install.packages("zyp")
library(zyp)
model<-zyp.sen(ndvi~year,df)
y<-model$coefficients[1]+model$coefficients[2]*year
lines(year,y,col='blue')


#change detection
install.packages("changepoint")
library(changepoint)
a<-readOGR('.',layer='blacksburg')
a_proj<-spTransform(a,crs(m))
avalue<-extract(m,a_proj)
plot(avalue[1,])
fit_changepoint = cpt.mean(avalue[1,])
fit_changepoint #Identified the changing point which is 2
plot(fit_changepoint)
cp <- cpts(fit_changepoint)

