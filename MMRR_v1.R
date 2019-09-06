library(raster)
library(ecodist)
library(PopGenReport)

setwd("~/Phylogeo Seminar/MMRR/Species")

##Find all folders for analysis
folders=list.dirs(path='.')

###For loop style
  
  i=29

  print(i)
  
  ##Locate files for future analysis
  files1 <- list.files(path=folders[i],pattern = '.csv$', full.names = TRUE)
  files2 <- list.files(path=folders[i],pattern = '.tif$', full.names = TRUE)
  
  ###Genetic distance matrix
  ##read in csv file
  a1=read.csv(files1[2], row.names=1)
  ##convert data_frame to dist object
  gen_mat <- as.dist(a1,upper = FALSE,diag = FALSE)
  gen_mat2=as.dist(a1,upper = TRUE,diag = TRUE)
  
  
  ###Isolation by Distance
  ##Read in locations
  b1=read.csv(files1[1])
  b2=cbind(b1[,4],b1[,3])
  ##Calculate distance matrix
  ibd=dist(b2)
  ibd2=dist(b2,upper=TRUE,diag=TRUE)
  
  ###Isolation by Environment
  ##Read in tiff files
  c1=raster(files2[1])
  ##extracting sdm probabilities
  c2=extract(c1,b2)
  c2[is.na(c2)] <- 0
  ##Calculate distance matrix
  ibe=dist(c2)
  ibe2=dist(c2,upper=TRUE,diag=TRUE)
  
  
  ###Isolation by instability
  ###Read in tiff file
  d1=raster(files2[2])
  ##extracting stability probabilities
  d2=extract(d1,b2)
  d2[is.na(d2)] <- 0
  ##Calculating distance matrix
  ibi=dist(d2)
  ibi2=dist(d2,upper=TRUE,diag=TRUE)
  
  
  ###MRM function
  ##Create output
  fn=paste(folders[i],'/output.txt', sep = '')
  sink(fn)
  ##Put species in file
  print(files1)
  ##Create MMRR output
  print(MRM(formula = gen_mat~ibd+ibe+ibi))

  ##Close output
  sink()
  
  print(MRM(formula = gen_mat~ibd+ibe))
  print(MRM(formula = gen_mat~ibd+ibi))
