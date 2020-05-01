start_time <- Sys.time()

pacman::p_load(XML, dplyr, data.table)

#Import Clinical Trial File
temp<-tempfile()
temp1<-tempfile()
download.file("https://clinicaltrials.gov/AllPublicXML.zip", temp)
print(Sys.time())

unzip(temp, exdir=temp1)
print(Sys.time())

#Create data.frames
Location<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Country", "State/Province"))
Sponsors<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Sponsor", "Collaborator"))
SV<-data.frame(matrix(ncol=4, nrow=0)) %>% `names<-`(c("ID", "Type", "Phase", "Start Date"))
Condition<-data.frame(matrix(ncol=2, nrow=0))%>% `names<-`(c("ID", "Condition"))
Intervention<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Intervention Type", "Intervention Name"))


for(j in 2:length(list.files(temp1))-1){ 
  
  setwd(paste(temp1, "\\", list.files(temp1)[j+1], sep="")) 
  
  for(i in 1:length(list.files())){
    
    #Import trial info
    fold<- xmlParse(list.files()[i])  
    
    #Import location of ID 
    nodeID<-getNodeSet(fold, "//id_info")
    
    #Location
    nodes <- getNodeSet(fold, "//location/facility/address")
    if(length(nodes)!=0){
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      x <- sapply(nodes, xpathSApply, "./country", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      y<- sapply(nodes, xpathSApply, "./state", xmlValue,  trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      df<-cbind(w,x,y) %>% `names<-`(names(Location)) %>% unique(by="Country")
      Location<-rbind(Location, df)
    }else{
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      df<-cbind(w, NA, NA)%>% `names<-`(names(Location))
      Location<-rbind(Location, df)
    }
    
    #Sponsors
    nodes <- getNodeSet(fold, "//sponsors")
    if(length(nodes)!=0){
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      x <- sapply(nodes, xpathSApply, "./lead_sponsor/agency", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      y<- sapply(nodes, xpathSApply, "./collaborator/agency", xmlValue,  trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      df<-cbind(w,x,y) %>% `names<-`(names(Sponsors)) 
      Sponsors<-rbind(Sponsors, df)
    }else{
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      df<-cbind(w, NA, NA) %>% `names<-`(names(Sponsors))
      Sponsors<-rbind(Sponsors, df)
    }
    
    #"Single Value" variables: Type, Phase, Start Date
    v<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
    if(length(xpathSApply(fold, "//study_type", xmlValue))!=0){
      w<-xpathSApply(fold, "//study_type", xmlValue) %>% data.frame }else{w<-NA %>% data.frame}
    if(length(xpathSApply(fold, "//phase", xmlValue))!=0){
      x<-xpathSApply(fold, "//phase", xmlValue) %>% data.frame }else{x<-NA %>% data.frame}
    if(length(xpathSApply(fold, "//start_date", xmlValue))!=0){
      y<-xpathSApply(fold, "//start_date", xmlValue) %>% data.frame }else{y<-NA %>% data.frame}
    df<-cbind(v,w,x,y) %>% `names<-`(names(SV))
    SV<-rbind(SV, df)
    
    #Condition
    v<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
    if(length(xpathSApply(fold, "//condition", xmlValue))!=0){
      w<-xpathSApply(fold, "//condition", xmlValue) %>% data.frame }else{w<-NA %>% data.frame}
    df<-cbind(v,w) %>% `names<-`(names(Condition))
    Condition<-rbind(Condition, df)
    
    #Intervention
    nodes <- getNodeSet(fold, "//intervention")
    if(length(nodes)!=0){
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      x <- sapply(nodes, xpathSApply, "./intervention_type", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      y<- sapply(nodes, xpathSApply, "./intervention_name", xmlValue,  trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame
      df<-cbind(w,x,y) %>% `names<-`(names(Intervention)) 
      Intervention<-rbind(Intervention, df)
    }else{
      w<- sapply(nodeID, xpathSApply, "./nct_id", xmlValue, trim=TRUE) %>% sapply(function(z) ifelse(length(z)==0, NA, z)) %>% data.frame 
      df<-cbind(w, NA, NA) %>% `names<-`(names(Intervention))
      Intervention<-rbind(Intervention, df)
    }
    
    
    if(i%%250==0){
      print(paste(Sys.time(), ": Folder", j, round(100*i/length(list.files()), 2), "% complete"))
    }
    
  }
  
  print(paste(Sys.time(), ":", j, "folders complete, ", length(list.files(temp1))-1-j, "folders remaining" ))
  
  #Save output (to clear memory)
  if(j%%25==0){
    setwd("Output")
    write.csv(Condition, paste("Condition", j, ".csv", sep=""))
    write.csv(Intervention, paste("Intervention", j, ".csv", sep=""))
    write.csv(Location, paste("Location", j, ".csv", sep=""))
    write.csv(Sponsors, paste("Sponsors", j, ".csv", sep=""))
    write.csv(SV, paste("SV", j, ".csv", sep=""))
    rm(Condition, Intervention, Location, Sponsors, SV)
    Location<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Country", "State/Province"))
    Sponsors<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Sponsor", "Collaborator"))
    SV<-data.frame(matrix(ncol=4, nrow=0)) %>% `names<-`(c("ID", "Type", "Phase", "Start Date"))
    Condition<-data.frame(matrix(ncol=2, nrow=0))%>% `names<-`(c("ID", "Condition"))
    Intervention<-data.frame(matrix(ncol=3, nrow=0)) %>% `names<-`(c("ID", "Intervention Type", "Intervention Name"))
  }
  
}

#Export Final Set
write.csv(Condition, paste("Condition", 25*round(j/25), ".csv", sep=""))
write.csv(Intervention, paste("Intervention", 25*round(j/25), ".csv", sep=""))
write.csv(Location, paste("Location", 25*round(j/25), ".csv", sep=""))
write.csv(Sponsors, paste("Sponsors", 25*round(j/25), ".csv", sep=""))
write.csv(SV, paste("SV", 25*round(j/25), ".csv", sep=""))

#Combine the datasets and export
as.list(unique(substr(list.files(), 1,2))) %>% lapply( function(x){subset(list.files(), substring(list.files(), 1,2)==x)}) %>%
  lapply( function(x){do.call(rbind, lapply(x, read.csv))}) %>% lapply(function(x){write.csv(x, paste(colnames(x[ncol(x)]), ".csv", sep=""))})

unlink(c(temp, temp1))

end_time <- Sys.time()
end_time - start_time