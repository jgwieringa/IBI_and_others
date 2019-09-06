####For Loop SDM
library(randomForest)
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(kernlab)
library(sp)
library(ecospat)
library(rJava)


#####Land
setwd("~/rSpatial team")


files1 <- list.files(path = "./Environmental Layers/worldclim_cropped_present",pattern = '.grd$', full.names = TRUE)
pred=stack(files1[1],files1[2],files1[3],files1[4],files1[5],files1[6])

names(pred)=c("bio01","bio02","bio04","bio08","bio12","bio15")

files2=list.files(path = "./Environmental Layers/worldclim_cropped_LGM",pattern = '.grd$', full.names = TRUE)


rs=stack(files2[1],files2[2],files2[3],files2[4])
rs=rs/10

rs2=stack(files2[5],files2[6])

lgm=stack(rs,rs2)

ext=extent(lgm)

names(lgm)=c("bio01","bio02","bio04","bio08","bio12","bio15")

files <- list.files(,path="./Reduced_species_occurrences/Land",pattern = '.csv$', full.names = TRUE)

for(i in 1:length(files)){
  loc=read.csv(files[i])
  ###Give species name for output rasters before it is removed
  species=loc[2,2]
  
  ###Removing columns that are not lat/long
  ###Doing it this way allows us to remove other columns if needed later on
  rem=c(1,2) 
  loc=loc[,-rem]
  
  ####this is only if the columns are in the wrong order (needs to be long/lat)
  
  colnames(loc)=c("longitude","latitude")
  
  pres=extract(pred, loc)
  set.seed(0)
  backgr=randomPoints(pred, 500)
  
  ###sudo absence points
  absvals=extract(pred, backgr)
  pb=c(rep(1, nrow(pres)), rep(0, nrow(absvals)))
  sdmdata=data.frame(cbind(pb, rbind(pres, absvals)))
  
  group <- kfold(loc, 5)
  pres_train=loc[group != 1, ]
  pres_test=loc[group == 1, ]
  backg=randomPoints(pred, n=1000, ext=ext, extf = 1.25)
  colnames(backg) = c('Longitude', 'Latitude')
  group=kfold(backg, 5)
  backg_train=backg[group != 1, ]
  backg_test=backg[group == 1, ]
  
  ####maxent
  ###Need Java installed
  ###Need MaxENT in the dismo folder
  jar=paste(system.file(package="dismo"), "~/MaxENT/maxent.jar", sep='')
  xm=maxent(pred, pres_train)
  px=predict(pred, xm, ext=ext, progress='')
  px_lgm=predict(lgm, xm, ext=ext, progress='')


  
  ####RAndom Forest
  colnames(backg_train)=c("longitude","latitude")
  train=rbind(pres_train, backg_train)
  pb_train=c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
  envtrain=extract(pred, train)
  envtrain=data.frame( cbind(pa=pb_train, envtrain) )
  envtrain=na.omit(envtrain)
  #####Set this model to whatever variables we decided upon
  model=factor(pa)~bio01+bio02+bio04+bio08+bio12+bio15
  rf1=randomForest(model, data=envtrain)
  pr=predict(pred, rf1, ext=ext)
  pr_lgm=predict(lgm, rf1, ext=ext)


  
  ####GLM
  #####Same as above for variables
  glm1=glm(pa~bio01+bio02+bio04+bio08+bio12+bio15, data = envtrain)
  pg=predict(pred,glm1,ext=ext)
  pg_lgm=predict(lgm,glm1,ext=ext)

  
  
  ###Weighted Mean
  emx=evaluate(pres_test, backg_test, xm, pred)
  erf=evaluate(pres_test, backg_test, rf1, pred)
  egl=evaluate(pres_test, backg_test, glm1, pred)
  
  auc <- sapply(list(erf, egl, emx), function(x) x@auc)
  w <- (auc-0.5)^2
  models=stack(pr,pg,px)
  m1 <- weighted.mean(models, w)
  
  
  ###Generate threshold
  tr=ecospat.mpa(m1,pres_test,perc = 0.9)
  
  ###Generate p/np raster
  m2=m1 > tr
  
  ####Create weighted LGM model
  ###Weights are drawn from those in current conditions
  models2=stack(pr_lgm,pg_lgm,px_lgm)
  m1_lgm <- weighted.mean(models2, w)
  
  ###Generate p/np raster
  m2_lgm=m1_lgm > tr

  
  ###Writing eight rasters
  ###Two for each time period
  ###Two continous and two binary
  fn=paste(species, '.tif', sep = '')
  writeRaster(m1, filename = fn, format = 'GTiff', overwrite = TRUE)
  
  fn=paste(species, '_p_np.tif', sep = '')
  writeRaster(m2, filename = fn, format = 'GTiff', overwrite = TRUE)
  
  fn=paste(species, '_lgm.tif', sep = '')
  writeRaster(m1_lgm, filename = fn, format = 'GTiff', overwrite = TRUE)
  
  fn=paste(species, '_p_np_lgm.tif', sep = '')
  writeRaster(m2_lgm,filename = fn, format = 'GTiff', overwrite = TRUE)

  print(species)
  print("done")
}

