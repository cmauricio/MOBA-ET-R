wDirectory<-"C:/Users/ru25tas/Desktop/SaccTrial" # assigns the working directory

setwd(wDirectory)

fileNumber<-c("0121","0122")
fileName<-c("Map","MM")
maps<-(0)
minimaps<-(0)

for (i in 1:length(fileNumber)) {
     maps[i]<-paste(fileName[1],fileNumber[i],".csv",sep="")
     minimaps[i]<-paste(fileName[2],fileNumber[i],".csv",sep="")
}

map1<-maps[1]
map2<-maps[2]
MM1<-minimaps[1]
MM2<-minimaps[2]

map1<-read.csv(map1, header=TRUE, sep=",", dec=".") # imports the CSV file to the variable gameMap
map1<-as.data.frame(map1)

map2<-read.csv(map2, header=TRUE, sep=",", dec=".") # imports the CSV file to the variable gameMap
map2<-as.data.frame(map2)

MM1<-read.csv(MM1, header=TRUE, sep=",", dec=".") # imports the CSV file to the variable gameMap
MM1<-as.data.frame(MM1)

MM2<-read.csv(MM2, header=TRUE, sep=",", dec=".") # imports the CSV file to the variable gameMap
MM2<-as.data.frame(MM2)

joinMap<-as.data.frame(rbind(map1,map2))
joinMM<-as.data.frame(rbind(MM1,MM2))

write.csv(joinMap, toString(maps[1]))
write.csv(joinMM, toString(minimaps[1]))
