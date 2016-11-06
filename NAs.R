# The purpose of these functions are to detect and extract or eliminate
# missing values (NA). The missing values are extracted or eliminated row-wise.
# That is to say that when a missing value is found for any variable the 
# whole row of data (observation) is deleted

# Function to obtain NAs
get.na<-function (dataframe,column="GazeInGame"){
     
     i<-1
     j<-which(colnames(dataframe)==column)
     na.database<-0
     c<-F

     for (i in 1:nrow(dataframe)) {
          c<-is.na (dataframe[i,j])
          if (c==T){
               na.database<-rbind(na.database,dataframe[i,])
          }
     }
     
     na.database[-1,]
}



# Function to clean NAs
clean.na<-function (dataframe){
     
     na.omit(dataframe)
     
}


# Extracting and deleting missing data from the databases and
# creating new databases with this data

lolexpexp<-read.table("160222lolExpExp.csv", header=T, sep=";", dec=",")
lolexpnov<-read.table("160222lolExpNov.csv", header=T, sep=";", dec=",")
lolnov<-read.table("160222lolNov.csv", header=T, sep=";", dec=",")
dotaexp<-read.table("160222dotaExp.csv", header=T, sep=";", dec=",")

na.lolexpexp<-get.na(lolexpexp)
write.table(na.lolexpexp, file="160222lolExpExpNAs.csv",sep = ";", dec = ",")
na.lolexpnov<-get.na(lolexpnov)
write.table(na.lolexpnov, file="160222lolExpNovNAs.csv",sep = ";", dec = ",")
na.lolnov<-get.na(lolnov,"Outcome")
write.table(na.lolnov, file="160222lolNovNAs.csv",sep = ";", dec = ",")
na.dotaexp<-get.na(dotaexp)
write.table(na.dotaexp, file="160222dotaExpNAs.csv",sep = ";", dec = ",")

clean.lolexpexp<-clean.na(lolexpexp)
write.table(clean.lolexpexp, file="160222lolExpExpClean.csv",sep = ";", dec = ",")
clean.lolexpnov<-clean.na(lolexpnov)
write.table(clean.lolexpnov, file="160222lolExpNovClean.csv",sep = ";", dec = ",")
clean.lolnov<-clean.na(lolnov)
write.table(clean.lolnov, file="160222lolNovClean.csv",sep = ";", dec = ",")
clean.dotaexp<-clean.na(dotaexp)
write.table(clean.dotaexp, file="160222dotaExpClean.csv",sep = ";", dec = ",")
