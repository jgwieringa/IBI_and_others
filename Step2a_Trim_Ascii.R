library(raster)
library(maps)
library(maptools)
library(sp)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgeos)


setwd("/Users/Heatherdame/Desktop/Environmental_Layers/test/wc5")


###### Crop the raster

# Get the raw Worldclim (or similar) grid
WC_1 <- raster("bio1.bil")

# Crop this using the above polygon as a "cookie cutter"
# This may take a couple minutes because usa_2_2 is a large file
cropped_shape <- mask(WC_1, usa_2_2)

# Crop all the files using the shape you just cut out to save time
for (i in 1:19) {
  Bio <- raster(paste("bio", i, ".bil", sep=""))
  masked <- mask(Bio, cropped)
  aoi <- extent(-130,-60,20,50)
  cropped2 <- crop(masked, aoi)
  full <- writeRaster(cropped2,paste("ascii_files_cropped_masked/Worldclim_", i, "_5m",sep=""),NAflag=-9999)
  plot(full, axes=TRUE)
}

##### For the past WorldClim (in geotiff) #####
# Crop all the files using the shape you just cut out to save time
for (i in 1:19) {
  Bio <- raster(paste("cclgmbi", i, ".tif", sep=""))
  aoi <- extent(-130,-60,20,50)
  cropped2 <- crop(Bio, aoi)
  full <- writeRaster(cropped2,paste("worldclim_cropped_LGM/Worldclim_", i, "cropped_LGM_5m",sep=""),NAflag=-9999, overwrite=TRUE)
  plot(full, axes=TRUE)
}

# For Marspec data
#Create a function (From Sbrocco 2012, MARSPEC) to create ascii files from GRID files

  for (i in 10:17) { 
  x <- raster(paste("biogeo", i, "_5m", sep=""))
aoi <- extent(-130,-60,20,50)
x.crop <- crop(x,aoi)
raster2 <- writeRaster(x.crop,paste("Marspec_LGM_cropped/Marspec_LGM_cropped", i, "_5m", sep=""),NAflag=-9999)
plot(raster2, axes=TRUE)
"ALL DONE!"
}

# Bathy data
x <- raster("bathy_5m")
aoi <- extent(-130,-60,20,50)
x.crop <- crop(x,aoi)
raster2 <- writeRaster(x.crop,"Marspec_LGM_cropped/Marspec_LGM_cropped_bathy_5m",NAflag=-9999)
plot(raster2, axes=TRUE)
"ALL DONE!"


### If you want to cut the raster to a certain country, do the below steps
setwd("/Users/Heatherdame/Desktop/Environmental_Layers/test/wc5")

# Get the shapefile
usa<-getData('GADM', country="USA", level=0)

# Reduce the size of the polygon in the shapefile to reduce processing time
# Get the main polygons, will determine by area (from online)
getSmallPolys <- function(poly, minarea=0.01) {
  # Get the areas
  areas <- lapply(poly@polygons, 
                  function(x) sapply(x@Polygons, function(y) y@area))
  # Quick summary of the areas
  print(quantile(unlist(areas)))
  # Which are the big polygons?
  bigpolys <- lapply(areas, function(x) which(x > minarea))
  length(unlist(bigpolys))
  # Get only the big polygons and extract them
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]] >= 1){
      poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
      poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
    }
  }
  return(poly)
}

#Run function you just made
usa_2_2 <- getSmallPolys(usa)

###### Crop the raster

# Get the raw Worldclim (or similar) grid
WC_1 <- raster("bio1.bil")

# Crop this using the above polygon as a "cookie cutter"
# This may take a couple minutes because usa_2_2 is a large file
cropped_shape <- mask(WC_1, usa_2_2)

# Crop all the files using the shape you just cut out to save time
for (i in 1:19) {
  Bio <- raster(paste("bio", i, ".bil", sep=""))
  masked <- mask(Bio, cropped)
  aoi <- extent(-130,-60,23,50)
  cropped2 <- crop(masked, aoi)
  full <- writeRaster(cropped2,paste("ascii_files_cropped_masked/Worldclim_", i, "_5m",sep=""),NAflag=-9999)
  plot(full, axes=TRUE)
}

