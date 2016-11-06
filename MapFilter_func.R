#This is a function created to separate data belonging to the different
#AoIs depending on the value of the variable 'on_srf' given by the
#pupil eye-tracker raw data. If the value is TRUE it means that the data
#data analysed corresponds to that particular AoI

on.surface.gaze<-function(Directory){
     
     #Parameter passing
     wDirectory<-Directory
     
     
     #variable initialisation
     perc<-0
     fileNames<-array()
     i<-1
     id_start<-1
     id_end<-1
     saveName<-""
     gameMap<-matrix()
     sub_data<-matrix()
     
     setwd(wDirectory) 
     
     #this creates an array with the csv names of the files to analyse and puts them in the variable fileName
     fileNames<-list.files(wDirectory, pattern="*.csv", full.names=FALSE)
     
     id_end<-length(fileNames)
     
     for (i in id_start:id_end) {
          name<-fileNames[i]
          
          gameMap<-read.csv(name, header=TRUE, sep="", dec=".")
          
          gameMap<-as.data.frame(gameMap)
          
          sub_data<-subset(gameMap, on_srf=="True")
          
          saveName<-paste("OnSrf_",name, sep="")
     
          write.table(sub_data, saveName, sep=";", dec=",")
          
          perc=(i/length(fileNames))*100
          
          print(paste(round(perc,digits=2),"%"))
     }

}
