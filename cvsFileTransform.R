csv.transfrom<-function(Directory) {

     #Parameters pasing
     wDirectory<-Directory
     
     #Variable initialisation
     i=1
     files<-list()
     reName<-list()
     perc=0

     setwd(wDirectory)

     files<-list.files(wDirectory, pattern="*.csv", full.names=FALSE)

     for (i in 1:length(files)) {
          reName<-read.csv(files[i], header=TRUE, sep="", dec=".")
          write.csv2(reName, toString(files[i]), sep=";",dec=",")
          perc<-(i/length(files))*100
          print(perc)
     }

}
