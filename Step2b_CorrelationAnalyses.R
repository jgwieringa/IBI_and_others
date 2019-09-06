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
library(openxlsx)



files=list.files(path="./",pattern='.grd',full.names = TRUE)

for(i in 1:length(files)){
  r=raster(files[i])
  fn=paste(files[i], '.tif', sep = '')
  writeRaster(r, filename = fn, format = 'GTiff', overwrite = TRUE)
}

files=list.files(path="./",pattern='.tif',full.names = TRUE)

rs=stack(files)

r_sample=sampleRandom(rs,size=500)

correlation <- cor(r_sample)

write.xlsx(correlation, "Marspec_LGM_correlations.xlsx")




