#alldata<- function(directory="C:/Users/Mauro/Desktop/autofolder", id = 1:7) {  ##function declaration
#     
#     files<-list.files(directory, pattern="*.csv", full.names=TRUE) ## assings to the object "files" all the csv files in "directory". the full names return an array with all the name including the path.
#     
#     id_start<-id[1]
#     id_end<-id[length(id)]
#     
#     alldata<-data.frame() ## creates an empty data frame to bulk the data
#     
#     for (i in id_start:id_end) { ## bulk the data from all the files into the created dataframe
#          alldata <- rbind(alldata,read.csv(files[i]))
#          print (files[i])
#     }
#     
#     med_pol<-mean(alldata[[pollutant]], na.rm=TRUE) ## gets the mean of the data in the collumn "pollutant" for the whole data. While extracting NAs
#     
#     med_pol ## returns the result
#}

wDirectory<-"C:/Users/ru25tas/Desktop/SaccTrial" # assigns the working directory

setwd(wDirectory) # sets up the working directory in the address assigned previously

library("saccades", lib.loc="~/R/win-library/3.2") # Imports the package saccades, used to detect the fixations

library("eyetrackR", lib.loc="~/R/win-library/3.2") # Imports the package eyetrakR, used to detect saccades ## This package is not compatible with the sampling ratio of the pupul eye tracker, so I need to pimplement another

counter<-c(411,421,431,441,451,461)
counter2<-array(0,dim=length(counter))
counter3<-array(0,dim=length(counter))
fileName<-c("MM","Map")

database_ET<-data.frame("Name"=integer(0),"NrFixationsGame"=numeric(0),"MeanFixationsGame"=numeric(0),"SDFixationsGame"=numeric(0),"MeanDurationGame"=numeric(0),"SDDurationGame"=numeric(0),"NrFixationsMM"=numeric(0),"MeanFixationsMM"=numeric(0),"SDFixationsMM"=numeric(0),"MeanDurationMM"=numeric(0),"SDDurationMM"=numeric(0),stringsAsFactors = TRUE)


for (j in 1:length(counter2)) {
          counter2[j]<-paste(fileName[1],counter[j],".csv",sep="")
          counter3[j]<-paste(fileName[2],counter[j],".csv",sep="")
}

for (k in 1:length(counter2) ) {

     nameGame<-as.character(counter3[k]) # gives "name" the name of the file to import. This is done so the process can be automated eventually
     nameMM<-toString(counter2[k])
     
     gameGame<-read.csv(nameGame, header=TRUE, sep="\t", dec=".") # imports the CSV file to the variable gameMap
     gameGame<-as.data.frame(gameGame) # transforms the variable gameMap into a data frame

     gameMM<-read.csv(nameMM, header=TRUE, sep="\t", dec=".") # imports the CSV file to the variable gameMap
     gameMM<-as.data.frame(gameMM) # transforms the variable gameMap into a data frame
    
######################################################3     
      
     id_start<-1 # counter for the following cycle
     id_end<-nrow(gameGame) # counter for the following cycle

     for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
          if (gameGame[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
               gameGame[i,]<-0
          }
     }

     saveName1<-paste("pros",nameGame, sep="") # Creates a variable "saveName" containing a variation of the name of the file so it doesn't match the importing file name when exporting it

     write.csv(gameGame, saveName1) # writes a CSV file with the processed entry datafiel

     sacTrial<-read.csv(saveName1, header=TRUE, sep=",", dec=".") # imports the file that has been previously exported under the variable "sacTrial"

     sample1<-as.data.frame(cbind(time=sacTrial[,"world_timestamp"], trial=sacTrial[,"world_frame_idx"], x=sacTrial[,"x_norm"], y=sacTrial[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"

     fixations<-0

     fixations<-detect.fixations(sample1, lambda=7, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"

     # Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of the algoritm creator

     fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 
     
     nrFix<-0

     nrFix<-nrow(fixations) # detects the amount of fows in the array "fixations". This works under the assumption that each row is indeed a fixation 
     
     database_ET[k,"Name"]<-as.integer(counter[k])
     
     database_ET[k,"NrFixationsGame"]<-nrFix

     summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"
     
     database_ET[k,"MeanFixationsGame"]<-summ[3,1]
     database_ET[k,"SDFixationsGame"]<-summ[3,2]
     database_ET[k,"MeanDurationGame"]<-summ[4,1]
     database_ET[k,"SDDurationGame"]<-summ[4,2]
     
################################################################
     
     i<-1
     id_start<-1 # counter for the following cycle
     id_end<-nrow(gameMM) # counter for the following cycle
     
     for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
          if (gameMM[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
               gameMM[i,]<-0
          }
     }
     
     saveName2<-paste("pros",nameMM, sep="") # Creates a variable "saveName" containing a variation of the name of the file so it doesn't match the importing file name when exporting it
     
     write.csv(gameMM, saveName2) # writes a CSV file with the processed entry datafiel
     
     sacTrial<-read.csv(saveName2, header=TRUE, sep=",", dec=".") # imports the file that has been previously exported under the variable "sacTrial"
     
     sample2<-as.data.frame(cbind(time=sacTrial[,"world_timestamp"], trial=sacTrial[,"world_frame_idx"], x=sacTrial[,"x_norm"], y=sacTrial[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"
     
     fixations<-0
     
     fixations<-detect.fixations(sample2, lambda=7, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"
     
     # Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of the algoritm creator
     
     fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 
     
     nrFix<-0
     
     nrFix<-nrow(fixations) # detects the amount of fows in the array "fixations". This works under the assumption that each row is indeed a fixation 
     
     database_ET[k,"NrFixationsMM"]<-nrFix
     
     summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"
     
     database_ET[k,"MeanFixationsMM"]<-summ[3,1]
     database_ET[k,"SDFixationsMM"]<-summ[3,2]
     database_ET[k,"MeanDurationMM"]<-summ[4,1]
     database_ET[k,"SDDurationMM"]<-summ[4,2]
     
}

write.csv(database_ET, "Databases.csv")# writes a CSV file containing the amount of fixations of the sample


#sacTrial<-read.csv(saveName, header=TRUE, sep=",", dec=".")

#xy<-as.matrix(cbind(sacTrial[,"x_norm"], sacTrial [,"y_norm"]))

#velocity<-vecvel(xy, 60, 1)

#saccades<-microsacc(xy, velocity, VFAC=6, MINDUR=3, discardMS = TRUE, removeGlissades = TRUE, DTGLISS = 30, MSamplCrit = 0.5)

#saccades<-as.data.frame(saccades)

#nrSac<-nrow(saccades)

#write.csv(saccades, "saccades.csv")
#write.csv(nrSac, "nrSac.csv")
