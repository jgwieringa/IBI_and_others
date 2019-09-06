arl_writer<- function(title, dnabin_obj, pop_assign, md, ploidy){
    
    ut = "DNA"
  
    gd = ploidy - 1 # convert ploidy (arlequin uses 0 for haploid)
    
    np <- length(unique(pop_assign[,2])) # get number of pops
    
    # convert ape's bit coding to normal sequence values
    dnabin_trans <- lapply(dnabin_obj, function(x) gsub("18", "T", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("28", "C", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("48", "G", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("88", "A", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("c0", "R", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("a0", "M", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("90", "W", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("60", "S", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("50", "K", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("30", "Y", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("e0", "V", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("b0", "H", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("d0", "D", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("70", "B", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("f0", "N", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("04", "-", x))
    dnabin_trans <- lapply(dnabin_trans, function(x) gsub("02", md, x))

    dnabin_trans2 <- sapply(seq(1, length(dnabin_trans)), function(x) paste(unlist(dnabin_trans[x], use.names = FALSE), collapse = ""))
    
    df <- cbind(pop_assign, dnabin_trans2)
    
    frames2 <- split(df, df[,2], drop = FALSE) #split new df by reg
    reg.names<- as.vector(df[,2]) #get region names
    reg.names2<- unique(unlist(reg.names, use.names=TRUE)) #keep unique regions
    reg.names3<- sort(reg.names2) #sort alphabetically
    reg.names4<- append(reg.names3, "blank", after=0) #to be used in parsing
    
    
    fileConn<- file(sprintf("%s.arp", title), "wt")
    writeLines('[Profile]', fileConn)
    cat('\n', file=fileConn)
    cat('Title="', title, sep ="", file=fileConn)
    cat('"', file=fileConn)
    cat('\n', file=fileConn)
    cat('NbSamples=', np, sep="", file=fileConn) #input num pops
    cat('\n', file=fileConn)
    cat('DataType=', ut, sep="", file=fileConn) #input ut
    cat('\n', file=fileConn)
    cat('GenotypicData=', gd, sep="", file=fileConn) #input ploidy
    cat('\n', file=fileConn)
    writeLines('GameticPhase=0', fileConn)
    writeLines('LocusSeparator=TAB', fileConn) #what to use as locus separator?
    cat('MissingData="', md, sep="", file=fileConn)
    cat('"', file=fileConn)
    cat('\n', file=fileConn)
    cat('\n', file=fileConn)
    writeLines('[Data]', fileConn)
    cat('\n', file=fileConn)
    writeLines('[[Samples]]', fileConn)
    cat('\n', file=fileConn)
    close(fileConn)
    i=1
    for (frame2 in frames2){ #for each region
      a1<- data.frame(frame2[seq(3,ncol(frame2),2)]) #make df for allele 1
      a1.1<- as.data.frame(append(a1,1,after=0)) #add column of 1's
      a1.2<- cbind(frame2[1], a1.1) #add sample names
      aa1<- data.frame(lapply(a1.2, as.character), stringsAsFactors = FALSE) 
      
      fileConn<- file(sprintf("%s.arp", title), "at")
      cat('SampleName="', as.character(reg.names3[i]), sep="", file=fileConn)
      cat('"', file=fileConn)
      cat('\n', file=fileConn)
      cat('SampleSize= ', nrow(frame2), sep="", file=fileConn)
      cat('\n', file=fileConn)
      writeLines('SampleData= {', fileConn)
      cat('\n', file=fileConn)
      write.table(aa1, file = fileConn, quote = FALSE, sep = "\t", na = "", row.names = FALSE, col.names = FALSE)
      writeLines('}', fileConn)
      cat('\n', file=fileConn)
      close(fileConn)
      i=i+1
    }
    fileConn<- file(sprintf("%s.arp", title), "at")
    writeLines('[[Structure]]', fileConn)
    writeLines('StructureName=""', fileConn)
    writeLines('NbGroups=1', fileConn)
    writeLines('Group={', fileConn)
    close(fileConn)
    for (name in reg.names3){
      fileConn<- file(sprintf("%s.arp", title), "at")
      cat('\t"', name, sep="", file=fileConn)
      cat('"', file=fileConn)
      cat('\n', file=fileConn)
      close(fileConn)
    }
    fileConn<- file(sprintf("%s.arp", title), "at")
    writeLines('}', fileConn)
    close(fileConn) 
    
}