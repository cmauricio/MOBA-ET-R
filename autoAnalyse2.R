wDirectory<-"C:/Users/ru25tas/Desktop/SaccTrial" # assigns the working directory

setwd(wDirectory) # sets up the working directory in the address assigned previously

dataType<-"Map"

fileNames<-list.files(wDirectory, pattern="*.csv", full.names=FALSE)

database_ET<-data.frame("Name"=integer(0),"NrFixationsGame"=numeric(0),"MeanFixationsGame"=numeric(0),"SDFixationsGame"=numeric(0),"MeanDurationGame"=numeric(0),"SDDurationGame"=numeric(0),"NrFixationsMM"=numeric(0),"MeanFixationsMM"=numeric(0),"SDFixationsMM"=numeric(0),"MeanDurationMM"=numeric(0),"SDDurationMM"=numeric(0), "Saccades"=numeric(0), stringsAsFactors = TRUE)

for (k in 1:length(fileNames) ) {

     nameGame<-as.character(fileNames[k]) # gives "name" the name of the file to import. This is done so the process can be automated eventually

     gameGame<-read.csv(nameGame, header=TRUE, sep=",", dec=".") # imports the CSV file to the variable gameMap
     gameGame<-as.data.frame(gameGame) # transforms the variable gameMap into a data frame
     gameMM<-as.data.frame(gameGame)
     
######################################################3     
      
     id_start<-1 # counter for the following cycle
     id_end<-nrow(gameGame) # counter for the following cycle
     
     if (dataType=="Map") {

          for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
               if (gameGame[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
                    gameGame[i,]<-0
               }
          }

          sample1<-as.data.frame(cbind(time=gameGame[,"world_timestamp"], trial=gameGame[,"world_frame_idx"], x=gameGame[,"x_norm"], y=gameGame[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"

          fixations<-0

          fixations<-detect.fixations(sample1, lambda=7, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"

          sacc<-detect.saccades(sample1,lambda=7,smooth.saccades=TRUE)
          
          saccount<-0
          
          for (n in 1:nrow(sacc)){
               if (sacc[n,"saccade"]=="TRUE") {
                    saccount<-saccount+1
               }
          }
          
          # Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of the algoritm creator
          
          fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 
     
          nrFix<-0

          nrFix<-nrow(fixations) # detects the amount of fows in the array "fixations". This works under the assumption that each row is indeed a fixation 
     
          database_ET[k,"Name"]<-toString(fileNames[k])
     
          database_ET[k,"NrFixationsGame"]<-nrFix

          summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"
     
          database_ET[k,"MeanFixationsGame"]<-summ[3,1]
          database_ET[k,"SDFixationsGame"]<-summ[3,2]
          database_ET[k,"MeanDurationGame"]<-summ[4,1]
          database_ET[k,"SDDurationGame"]<-summ[4,2]
          database_ET[k,"Saccades"]<-saccount
     
################################################################
     } else if (dataType=="Minimap") {
          
          i<-1
          id_start<-1 # counter for the following cycle
          id_end<-nrow(gameMM) # counter for the following cycle
     
          for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
             if (gameMM[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
                gameMM[i,]<-0
               }
          }
     
          sample2<-as.data.frame(cbind(time=gameMM[,"world_timestamp"], trial=gameMM[,"world_frame_idx"], x=gameMM[,"x_norm"], y=gameMM[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"
     
          fixations<-0
     
          fixations<-detect.fixations(sample2, lambda=7, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"

          sacc<-detect.saccades(sample1,lambda=7,smooth.saccades=TRUE)
          
          saccount<-0
          
          for (n in 1:nrow(sacc)){
               if (sacc[n,"saccade"]=="TRUE") {
                    saccount<-saccount+1
               }
          }
          
          # Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of the algoritm creator
     
          fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 
     
          nrFix<-0
     
          nrFix<-nrow(fixations) # detects the amount of fows in the array "fixations". This works under the assumption that each row is indeed a fixation 
     
          database_ET[k,"Name"]<-toString(fileNames[k])
          
          database_ET[k,"NrFixationsMM"]<-nrFix
     
          summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"
     
          database_ET[k,"MeanFixationsMM"]<-summ[3,1]
          database_ET[k,"SDFixationsMM"]<-summ[3,2]
          database_ET[k,"MeanDurationMM"]<-summ[4,1]
          database_ET[k,"SDDurationMM"]<-summ[4,2]
     
     }
}

if (dataType=="Map") {
     write.csv(database_ET, "MapDatabases.csv")# writes a CSV file containing the amount of fixations of the sample
} else if (dataType=="Minimap") {
     write.csv(database_ET, "MinimapDatabases.csv")# writes a CSV file containing the amount of fixations of the sample
}