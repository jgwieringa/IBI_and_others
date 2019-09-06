require(pegas)
require(adegenet)
require(poppr)

p <- "~/Box Sync/DDuckett/Class/2018_Fall/Phylogeo_Seminar/Project/adegenet_pegas/Genetic data sorted by species (updated as of Jan. 21)"
folders <- list.dirs(path = p, full.names = F, recursive = F)

#loop through species folders
for (f in folders) {
  
  #change to species f directory
  dir<-(paste(p,f,sep="/"))
  #dir<-(paste(p,folders[4],sep="/")) #for testing
  print(dir)
  setwd(dir)
  
  ### Read in fasta files and remove outgroups
  files <- list.files(path = dir, pattern = ".fas$") # get fasta file names
  fas <- paste(dir, files, sep = "/") # create directory path
  print(fas)
  genetic_info_fas <- read.FASTA(fas) # read fasta file
  fas_names <- names(genetic_info_fas) # get sequence ids
  fas_names <- fas_names[grep("*_i_*", fas_names)] # remove outgroup ids
  genetic_info_fas <- genetic_info_fas[fas_names] # subset dnabin object to remove outgroups
  genetic_info_fas <- genetic_info_fas[unlist(lapply(sort(fas_names), function(x) which(names(genetic_info_fas) == x)))] # sort to match pop_assign
  
  ### Get pop assignments
  files2 <- list.files(path = dir, pattern = "*_assignments.csv") # get pop assignment file
  popas <- paste(dir, files2, sep = "/") # create directory path
  pop_assign <- read.csv(popas, stringsAsFactors = FALSE, row.names = 1, na.strings = c("", " ", "NA")) # read in pop assignment csv
  pop_assign <- pop_assign[order(pop_assign[,1]),] # sort by sample
  
  nas <- which(is.na(pop_assign[,2]) == FALSE) # get na rows
  genetic_info_fas <- genetic_info_fas[nas] # remove nas from samples
  pop_assign <- na.omit(pop_assign) # remove NAs from assignments
  
  arl_writer(title = "out", dnabin_obj = genetic_info_fas, pop_assign = pop_assign, md = "?", ploidy = 1) # write arlequin file
  
  system("../arlecoremac_64bit out.arp ../arl_run.ars") # run arlecore
  
  
  setwd(p)
  
}