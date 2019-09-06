##### Analyze SDMs #####

library(raster)



setwd("~/Box Sync/DDuckett/Class/2018_Fall/Phylogeo_Seminar/Project/rspatial/SDM_Output")

#testing
# arubrum_lgm <- raster("Acer rubrum_p_np_lgm.tif") 
# plot(arubrum_lgm)
# 
# atigrinum_lgm <- raster("Ambystoma tigrinum_p_np_lgm.tif")
# plot(atigrinum_lgm)
# 
# comb_lgm <- sum(arubrum_lgm, atigrinum_lgm)
# plot(comb_lgm)
# test_stack <- stack(list("Acer rubrum_p_np_lgm.tif", "Ambystoma tigrinum_p_np_lgm.tif"))
# test_sum <- sum(test_stack)
# plot(test_sum)
#marine_test <- raster("Spisula solidissima_p_np.tif")

print_map <- function(file_name, object){ # function for printing maps
  png_name <- paste0(file_name, ".png")
  png(png_name)
  plot(object)
  dev.off()
  
  pdf_name <- paste0(file_name, ".pdf")
  pdf(pdf_name)
  plot(object)
  dev.off()
}


### Get names of SDM files
lgm_files <- list.files(path = ".", pattern = "*_p_np_lgm.tif") # get LGM file names
now_files <- list.files(path = ".", pattern = "*_p_np.tif") # get current file names

lgm_file_names <- lapply(lgm_files, function(x) strsplit(x, ".tif")[[1]][1]) # remove file extension
now_file_names <- lapply(now_files, function(x) strsplit(x, ".tif")[[1]][1]) # remove file extension

mapply(function(x,y) print_map(x, raster(y)), lgm_file_names, lgm_files) # print maps for all lgm SDMs for each species
mapply(function(x,y) print_map(x, raster(y)), now_file_names, now_files) # print maps for all current SDMs for each species

### LGM overlap - possible shared refugia ###
lgm_rasters <- stack(lgm_files) # read in all LGM rasters as a raster stack
lgm_sum <- sum(lgm_rasters) # sum rasters
#plot(lgm_sum)
writeRaster(lgm_sum, "lgm_sum", format = "GTiff")
writeRaster(lgm_sum, "lgm_sum", format = "raster")
print_map("lgm_sum", lgm_sum)

### Current overlap ###
now_rasters <- stack(now_files) # read in all current rasters as a raster stack
now_sum <- sum(now_rasters) # sum rasters
#plot(now_sum)
writeRaster(now_sum, "now_sum", format = "GTiff")
writeRaster(now_sum, "now_sum", format = "raster")
print_map("now_sum", now_sum)

### Function for getting stable habitat for each species
calc.stable <- function(lgm, now){
  
  lgm_raster <- raster(lgm)
  now_raster <- raster(now)
  
  sp_name <- paste(strsplit(unlist(names(lgm_raster)), "_")[[1]][1:2], collapse = " ") # get species name
  out_name <- paste0(sp_name, "_stable") # create file name
  
  sum <- sum(lgm_raster, now_raster) # sum rasters
  
  ### there's probably an actual function to do this, but this seems to work at least for now
  sum_recalc <- calc(sum, fun = function(x) {x[x<2] <- 0; return(x)}) # turn 1s into 0s
  sum_recalc <- calc(sum_recalc, fun = function(x) {x[x==2] <- 1; return(x)}) # turn 2s into 1s
  
  writeRaster(sum_recalc, out_name, format = "GTiff")
  writeRaster(sum_recalc, out_name, format = "raster")
  print_map(out_name, sum_recalc)
}

mapply(calc.stable, lgm_files, now_files) # calculate stability for all species in folder    

### Stable Overlap ###
stable_files <- list.files(path = ".", pattern = "*_stable.tif") # get stable file names
stable_rasters <- stack(stable_files) # read in all stable rasters as raster stack
stable_sum <- sum(stable_rasters) # sum rasters
#plot(stable_sum)
writeRaster(stable_sum, "stable_sum", format = "GTiff")
writeRaster(stable_sum, "stable_sum", format = "raster")
print_map("stable_sum", stable_sum)




