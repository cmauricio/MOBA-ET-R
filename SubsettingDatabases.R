# Separating the databases

# Setting the work directory and importing the files
wDirectory<-"C:/Users/ru25tas/Dropbox/Analysis"
csvFile<-"160222FullETDatabase.csv"

setwd(wDirectory)

fullDatabase<-read.table(csvFile, header=TRUE, sep=";", dec=",",na.strings="NA")
fullDatabase<-as.data.frame(fullDatabase) 

# Subsetting the databases according to game, group of expertise, and acondition.
LoLExperts<-subset(fullDatabase,Game=="lol"&Expertise=="expert"&Condition=="expert")
LoLExperts2<-subset(fullDatabase,Game=="lol"&Expertise=="expert"&Condition=="novice")
LoLNovices<-subset(fullDatabase,Game=="lol"&Expertise=="novice")
dotaExperts<-subset(fullDatabase,Game=="dota")

# Cleaning the data of NA columns
a<-which(colnames(LoLExperts)=="Elo")
LoLExperts<-LoLExperts[,-a]
a<-which(colnames(LoLExperts2)=="Elo")
LoLExperts2<-LoLExperts2[,-a]
a<-which(colnames(LoLNovices)=="Elo")
LoLNovices<-LoLNovices[,-a]
a<-which(colnames(LoLExperts)=="GroupByElo")
LoLExperts<-LoLExperts[,-a]
a<-which(colnames(LoLExperts2)=="GroupByElo")
LoLExperts2<-LoLExperts2[,-a]
a<-which(colnames(LoLNovices)=="GroupByElo")
LoLNovices<-LoLNovices[,-a]
a<-which(colnames(LoLNovices)=="Tier")
LoLNovices<-LoLNovices[,-a]
a<-which(colnames(dotaExperts)=="Tier")
dotaExperts<-dotaExperts[,-a]
a<-which(colnames(LoLNovices)=="Division")
LoLNovices<-LoLNovices[,-a]
a<-which(colnames(dotaExperts)=="Division")
dotaExperts<-dotaExperts[,-a]
a<-which(colnames(LoLNovices)=="GroupByTier")
LoLNovices<-LoLNovices[,-a]
a<-which(colnames(dotaExperts)=="GroupByTier")
dotaExperts<-dotaExperts[,-a]

# Writing tables for every group
write.table(LoLExperts, file="160222lolExpExp.csv",sep = ";", dec = ",")
write.table(LoLExperts2, file="160222lolExpNov.csv",sep = ";", dec = ",")
write.table(LoLNovices, file="160222lolNov.csv",sep = ";", dec = ",")
write.table(dotaExperts, file="160222dotaExp.csv",sep = ";", dec = ",")
