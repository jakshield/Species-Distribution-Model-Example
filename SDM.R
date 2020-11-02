#Load Required packages
library(dismo)
library(maptools)
library(raster)
library(randomForest)

#Download Sylvilagus obscurus data
sobs<-gbif("Sylvilagus", 'obscurus ', sp = T)

#Import environmental data
setwd('C:/Users/Jake/Documents/Science/GIS DataBase/Worldclim/wc2.1_30s_bio')
wrldclim<-(dir())
wrldclim<-stack(wrldclim)

#Import Ecoregion shapefile
App_eco<-shapefile('C:/Users/Jake/Documents/Science/Species Distribution Model Example/App_Ecoregion.shp')
App_eco<-spTransform(App_eco, CRSobj = crs(wrldclim))


#Set extent to Ecoregion
app_wrldclim<-crop(wrldclim, extent(App_eco))

#Visualize Data
plot(app_wrldclim, 1)
points(sobs)

#Generate Training Data
set.seed(0)
groups<-kfold(sobs, 5)
train<- sobs[groups != 1,]
test<-sobs[groups == 1,]

#Generate absence points
set.seed(0)
backgroundpts<-randomPoints(app_wrldclim, 1000)

#Bioclim Model
bc<-bioclim(app_wrldclim, sobs)
pbc<-predict(app_wrldclim,bc)
ebc<-evaluate(p = sobs, a = backgroundpts, model = bc, x = app_wrldclim)

#MaxEnt Model
mx<-maxent(app_wrldclim, sobs)
pmx<-predict(app_wrldclim, mx)
emx<-evaluate(p = sobs, a = backgroundpts, model = mx, x = app_wrldclim)
plot(mx)
response(mx)

#Generalized Linear Model
p.values<-extract(app_wrldclim, sobs)
a.values<-extract(app_wrldclim, backgroundpts)
p.a<- c(rep(1, nrow(p.values)), rep(0, nrow(a.values)))
glm.data<-as.data.frame(cbind(p.a, rbind(p.values, a.values)))
elm<-evaluate(p =sobs, a = backgroundpts, model =  l.m, x = app_wrldclim)
l.m<- glm(p.a~wc2.1_30s_bio_10, data = glm.data)
plm<-predict(app_wrldclim, l.m)

#Random Forest
rfmodel<-p.a~wc2.1_30s_bio_10 + wc2.1_30s_bio_7 + wc2.1_30s_bio_4 + wc2.1_30s_bio_15
rf<-randomForest(rfmodel, data = glm.data)
erf<-evaluate(p = sobs, a = backgroundpts, model = rf, x = app_wrldclim)
prf<-predict(app_wrldclim, rf)

#Combine models via weighted means
auc<-sapply(list(elm, erf, emx, ebc), function(x) x@auc)
w.auc<-(auc-0.5)^2
models <- stack(plm, prf, pmx, pbc)
c.models<-weighted.mean(models, w.auc)
plot(c.models)





