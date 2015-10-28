#This function is meant to automatically process the data from the eye tracking
#taking into account game maps separated from the minimaps analysis#
#
#The parameters Directory as the directory the data are.
#
#The parameter Datatype is meant to chose how to process and save the 
#data. Parameters accepted here are "Map" and "Minimap".
#
#The parameter Lambda is the lambda value from Malsbrug algorithm
#
#You can find Malsbrug algorithm for eye tracking analysis at
#https://github.com/tmalsburg/saccades

#' @name EyeTrackingAnalysis
#' @docType Function
#' @title Detection of Fixations and saccades in Raw Eye-Tracking Data
#' @author Carlos Mauricio Castano Diaz  \email{c.mauricio1985@@gmail.com}
#' @import saccades functions developed by Titus von der Malsburg \email{malsburg@@posteo.de}


eyetracking.analysis<- function (Directory, Datatype="Map", Lambda) {
     
     library("saccades", lib.loc="~/R/win-library/3.2")
     
     wDirectory<-Directory #Parameter passing from directory

     setwd(wDirectory) # sets up the working directory in the address assigned previously

     dataType<-Datatype # parameter passing to datatype
     
     lambda<-Lambda # parameter passing to lambda
     
     perc<-0
     
     #this creates an array with the csv names of the files to analyse and puts them in the variable fileName
     fileNames<-list.files(wDirectory, pattern="*.csv", full.names=FALSE)

     #This creates an empty dataframe with the name of the colums for the data analysed 
     database_ET<-data.frame("Name"=integer(0),"NrFixationsGame"=numeric(0),"MeanFixationsGame"=numeric(0),"SDFixationsGame"=numeric(0),"MeanDurationGame"=numeric(0),"SDDurationGame"=numeric(0),"NrFixationsMM"=numeric(0),"MeanFixationsMM"=numeric(0),"SDFixationsMM"=numeric(0),"MeanDurationMM"=numeric(0),"SDDurationMM"=numeric(0), "NrSaccadesGame"=numeric(0), "NrSaccadesMM"=numeric(0), stringsAsFactors = TRUE)

#This cycle runs through the array of files in the specified directory and 
#processess every file, puting the result in a database by the end
     
     for (k in 1:length(fileNames) ) {

          # gives "name" the name of the file to import.
          nameGame<-as.character(fileNames[k]) 

          # imports the current CSV file in the array to the variable gameMap
          gameGame<-read.csv(nameGame, header=TRUE, sep=",", dec=".") 
          gameGame<-as.data.frame(gameGame) 
          gameMM<-as.data.frame(gameGame)
     
########################################################################
# This part of the code is meant to process the map values if the      #   
# parameter Datatype chosen was "Map"                                  #
########################################################################
          
          id_start<-1 # counter for the following cycle
          id_end<-nrow(gameGame) # counter for the following cycle
     
          #This cycle processes tha map data and assigns ceros and NAs to all the data produced while the player was not looking inside the POI
          for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
               if (gameGame[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
                    gameGame[i,]<-0
               }
          }

          #Creates a sub-matrix with the columns needed for the eye tracking data
          sample1<-as.data.frame(cbind(time=gameGame[,"world_timestamp"], trial=gameGame[,"world_frame_idx"], x=gameGame[,"x_norm"], y=gameGame[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"

          fixations<-0

          #Put into the variable fixations the result of the fixation detection from Malsburg's functions
          fixations<-detect.fixations(sample1, lambda, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"

          #Put into the variable sacc the result of the saccade detection from Malsburg's functions
          sacc<-detect.saccades(sample1,lambda,smooth.saccades=TRUE)
          
          saccount<-0

          #Malsburg's saccade detection functions return a matrix indicating which measure was recognised as a saccade
          #this cycle goes trhough the matrix and counts the detected saccades and adds them to a counter
          for (n in 1:nrow(sacc)){
               if (sacc[n,"saccade"]=="TRUE") {
                    saccount<-saccount+1
                  }
          }
          
          
          fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 
     
          nrFix<-0

          nrFix<-nrow(fixations) # detects the amount of rows in the array "fixations". This works under the assumption that each row is indeed a fixation 
     
          database_ET[k,"Name"]<-toString(fileNames[k]) #adds the filename to the dataframe we will print later
     
          database_ET[k,"NrFixationsGame"]<-nrFix #adds the number of fixations to the dataframe we will print later

          #runs the summary function from Malsburg's algorithm and assign it to the variable sum
          summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"

          if (dataType=="Map") {
                    
               #adds the rest of the data to the dataframe
               database_ET[k,"MeanFixationsGame"]<-summ[3,1]
               database_ET[k,"SDFixationsGame"]<-summ[3,2]
               database_ET[k,"MeanDurationGame"]<-summ[4,1]
               database_ET[k,"SDDurationGame"]<-summ[4,2]
               database_ET[k,"NrSaccadesGame"]<-saccount
               
          } else if (dataType=="Minimap") {
               #adds the rest of the data to the dataframe     
               database_ET[k,"MeanFixationsMM"]<-summ[3,1]
               database_ET[k,"SDFixationsMM"]<-summ[3,2]
               database_ET[k,"MeanDurationMM"]<-summ[4,1]
               database_ET[k,"SDDurationMM"]<-summ[4,2]
               database_ET[k,"NrSaccadesMM"]<-saccount
               
          }
               perc<-(k/length(fileNames))*100
               
               print(paste(as.integer(perc),"%"))
     
     }
     database_ET
}
     

