#############################
### data formatting ##########
#############################
# Coleen Thompson 12/12/18 (With help from Drew)

install.packages("readxl")
install.packages("rgdal")
install.packages("randomForest")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("dismo")
install.packages("kernlab")
install.packages("sp")
install.packages("rgbif")
install.packages("tidyr")


library(readxl)
library(rgdal)
library(randomForest)
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(kernlab)
library(sp)
library(rgbif)
library(tidyr)
#######################################################################################
#######################################################################################
#######################################################################################
###########Use

library(readxl)
library(rgdal)
library(randomForest)
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(kernlab)
library(sp)
library(rgbif)
library(tidyr)


# Set up folders each with the name of " Genus species ". Include the space in the file name


setwd("../../../Loop_test/")

# list all species folders
folders <- list.dirs(full.names = F, recursive = F)
species<-as.data.frame(folders)

# If it's not in your working directory:
# folders <- list.dirs(path = "/Users/PAS1390/OSU10232/CompPhylo_Class/rSpatial/loop_test/", full.names = F, recursive = F)


# get species name in list
species_ <-as.data.frame(folders)
species_ <- as.list(species_)

species[] <- lapply(species, gsub, pattern='_', replacement=' ')
colnames(species)[1] <- "species"

# put list in alphabetical order	
sep_species<-separate(species, species, into = c("genus", "species"), sep = " (?=[^ ]+$)")
write.table(species,file="species.txt", row.names= FALSE)
write.table(sep_species,file="genus_species.txt", row.names= FALSE)

# pull species info from GBIF

scinames<-as.list(species)
scinames<-as.character(scinames[[1]])
spt<-sep_species$species

#get GBIF data (might take a minute or two)
for(i in 1:length(scinames)){ 
  dir<-(paste(species_[[1]][i],sep=""))
  splist2=scinames[i]
  keys <- sapply(splist2, function(x) name_backbone(name=x)$speciesKey, USE.NAMES=FALSE) 
  gb_data=occ_search(taxonKey=keys, fields=c('name','basisOfRecord','decimalLongitude','decimalLatitude','year','country'))
  colnames(gb_data$data)=c("species","basisofrecord","Lon","Lat","Year","Country")
  gb_data=gb_data$data
  write.csv(gb_data,file=paste(dir,"/",species_[[1]][i],"_gbif.csv",sep=""))
}

# If you're not already in the working directory, do:  
# dir<-(paste("/Users/PAS1390/OSU10232/CompPhylo_Class/rSpatial/loop_test/",species_[[1]][i],sep=""))

# subset and combine data
for (f in folders) {
  #change to species f directory
  dir<-(paste(f,sep=""))
  setwd(dir)
  
  #read in class data and GBIF data
  species<-paste(f,".csv",sep="")
  species<-read.csv(species, strip.white=TRUE)
  gbif<-paste(f,"_gbif.csv",sep="")
  gbif<-read.csv(gbif)
  
  
  #only keep certain data
  gbif<-gbif[gbif$basisofrecord=="PRESERVED_SPECIMEN" | gbif$basisofrecord=="LIVING_SPECIMEN" | gbif$basisofrecord=="MATERIAL_SAMPLE",]
  gbif<-gbif[gbif$Year >= 1900,]
  gbif<-gbif[gbif$Country== "United States of America" |  gbif$Country=="Canada",]
  
  #just keep species, lat, long
  gbif<-gbif[,c(2,4,5)]
  species<-species[,c(4,5,20,21)]
  
  #Standardize column header names for capitalization
  names(species)[1] <- "Genus"
  names(species)[2] <- "Species"
  names(species)[3] <- "Lat"
  names(species)[4] <- "Lon"
  
  #remove rows with NA
  gbif<-na.omit(gbif)
  
  #combine genus and species
  species$species <- paste(species$Genus,species$Species)
  species<-species[,c(3:5)]
  
  #put in correct order
  species<-species[,c(3,1,2)]
  gbif<-gbif[,c(1,3,2)]
  
  ##Remove species that are not target species (e.g. outgroup taxa)
  species <- species[species$species==f,]
  species<-na.omit(species)
  
  #add gbif data to existing class data, combine data frames
  complete_data<- rbind(gbif, species)
  
  #output file 
  write.csv(file=paste(f,"_completedata.csv", sep=""),complete_data, row.names=F,quote=F) 
  
  setwd("..")
}

####Check how many numbers of occurrences made it through

for (f in folders) {
  dir<-(paste(f,sep=""))
  setwd(dir)
  
  #read how many entries there are, remove those that are named
  numbers <- as.data.frame(read.csv(file=paste(f,"_completedata.csv")))
  
  if (nrow(numbers)<31){
    print(f)
  } else {
    print("30+")
  }
  
  setwd("..")
}

####Move folders that are listed in the above list to a "junk" bin - not used in analysis


data(wrld_simpl)
plot(wrld_simpl, xlim=c(-66,75), ylim=c(-165,180), axes=TRUE, col="light green")
box()
points(SNAME$Lon, SNAME$Lat, col='red', pch=20, cex=0.75)



