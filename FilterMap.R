setwd("C:/Users/ru25tas/Desktop/SaccTrial")

name<-"Map121.csv"

gameMap<-read.csv(name, header=TRUE, sep="\t", dec=".")

gameMap<-as.data.frame(gameMap)

id_start<-1
id_end<-nrow(gameMap)


for (i in id_start:id_end) {
     if (gameMap[i,"on_srf"]=="False") {
          gameMap[i,]<-0
     }
}

saveName<-paste("pros",name, sep="")
     
write.csv(gameMap, saveName)
