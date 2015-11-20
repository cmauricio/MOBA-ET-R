bind.cvsfiles<-function(namefile1,namefile2) {
     
     namefile1<-paste(namefile1,".csv", sep = "")
     namefile2<-paste(namefile2,".csv", sep = "")
     
     map1<-read.csv(namefile1, header=TRUE, sep="", dec=".") # imports the CSV file to the variable gameMap
     map1<-as.data.frame(map1)
     
     map2<-read.csv(namefile2, header=TRUE, sep="", dec=".") # imports the CSV file to the variable gameMap
     map2<-as.data.frame(map2)
     
     
     jointMap<-as.data.frame(rbind(map1,map2))
     
     write.table(jointMap, paste("bind",namefile1, sep = "",dec = "."))
     
}
