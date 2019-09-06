##########################################################################################
##############################Input your occurrence data##################################
##########################################################################################

#### Loop of downsampling
folders <- list.dirs(full.names = F, recursive = F)


for (f in folders) {
  #change to species f directory
  dir<-(paste(f,sep=""))
  setwd(dir)
  species<-paste(f," _completedata.csv",sep="")
  S_name <- read.csv(species, strip.white=TRUE)

  #Choose the columns only with long and lat
  SNAME <- S_name[,2:3]

  # Map all occurrences to make sure everything loaded correctly (activate if needed)
  # data(wrld_simpl)
  # plot(wrld_simpl, xlim=c(-66,75), ylim=c(-165,180), axes=TRUE, col="light green")
  # box()
  # points(SNAME$Lon, SNAME$Lat, col='red', pch=20, cex=0.75)
  
  # Rename the grid columns: Latitude = y; Longitude = x and make a new data frame
  # Pay attention to the order in which lat and long are in
  colnames(SNAME) <- c("y", "x")
  x <- SNAME$x
  y <- SNAME$y
  xy <- cbind(x,y)

  # Build a raster from an environmental layer being used
  envraster <- raster("../../Environmental_Layers/Cropped_Environmental_layers/Marspec_cropped_present/Marspec_cropped_present1_5m.gri")
  r <- raster()
  ext <- extent(envraster)
  extent(r) <- ext
  r <- setExtent(r, ext, keepres=TRUE)

  # Take one sample only per grid in the raster (if there is an occurrence in a grid,
  # it returns a single lat/long for that grid cell)
  SNAMEreduc <- gridSample(xy, r, n=1)
 
  # Keeeping the below 3 lines if you want to check
  # p <- rasterToPolygons(r)
  # plot(p, border='gray')
  # points(SNAMEreduc, cex=1, col='red', pch='x')
  
  ######Make a new data frame that has three columns: name, long, and lat#################
  reducx <- SNAMEreduc[,1]
  reducy <- SNAMEreduc[,2]
  len <- length(reducx)

  # Make a column of the name of the species based on the 1st column name of first file
  species_name<-rep(f, len)
  
  sp_pts<-cbind(species_name,reducx,reducy)
  colnames(sp_pts) <- c("species_name", "longitude", "latitude")
  
  # Write a csv file labelled the species name
  write.csv(sp_pts, file=paste(f, "_25_reduced_points_mar.csv", sep=""))
  file.copy(paste(f, "_25_reduced_points.csv", sep=""), "../../Reduced_files/With_marspec/")
  
  setwd("..")
  
}


####Check how many numbers of occurrences made it through

for (f in folders) {
  dir<-(paste(f,sep=""))
  setwd(dir)
  
  #read how many entries there are, remove those that are named
  numbers <- as.data.frame(read.csv(file=paste(f, "25_reduced_points.csv", sep="")))
  
  if (nrow(numbers)<25){
    print(f)
  } else {
    print("25+")
  }
  
  setwd("..")
}

####Move folders that are listed in the above list to a "junk" bin - not used in analysis



##########################################################################################
############Subsample records within an area to thin out records##########################
###########Build a grid at the same resolution as your environmental data#################
##########################################################################################

# INDIVIDUAL RUNNING, DON'T NEED TO RUN IF THE LOOP IS SUCCESSFUL

setwd("Acer rubrum")
S_name <- read.csv("Acer rubrum _completedata.csv", header=TRUE)

SNAME <- S_name[,2:3]

########################Check the first few lines:########################################
head(SNAME)

###########Map all occurrences to make sure everything loaded correctly###################
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-66,75), ylim=c(-165,180), axes=TRUE, col="light green")
box()
points(SNAME$Lon, SNAME$Lat, col='red', pch=20, cex=0.75)


#Create a raster with resolution to 5 arcmin (= 0.08333 degree grids) that matches the environmental grid layer resolution
res <- 0.083333
r <- raster(extent(-160,-20,15,80))
res(r) <- res

#####Take one sample only per grid in the raster (if there is an occurrence in a grid, ###
######it returns a single lat/long for that grid cell)####################################
SNAMEreduc <- gridSample(xy, r, n=1)

######Plot the original points and the old points on a grid###############################
plot(xy, cex=0.1)
points(SNAMEreduc, pch='x', col='red')

######Make a new data frame that has three columns: name, long, and lat#################
reducx <- SNAMEreduc[,1]
reducy <- SNAMEreduc[,2]
len <- length(reducx)

###Make a column of the name of the species based on the 1st column name of first file####
species_name<-rep(colnames(S_name)[3], len)

sp_pts<-cbind(species_name,reducx,reducy)
colnames(sp_pts) <- c("species_name", "longitude", "latitude")

############Write a csv file labelled the species name####################################
write.csv(sp_pts, file="species_name.csv")