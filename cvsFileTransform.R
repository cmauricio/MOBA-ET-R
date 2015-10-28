wDirectory<-"C:/Users/ru25tas/Desktop/SaccTrial/ETData/Map" # assigns the working directory

setwd(wDirectory)

files<-list.files(wDirectory, pattern="*.csv", full.names=FALSE)

for (i in 1:length(files)) {
     reName<-read.csv(files[i], header=TRUE, sep="\t", dec=".")
     write.csv(reName, toString(files[i]))
}

