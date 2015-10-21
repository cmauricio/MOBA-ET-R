wDirectory<-"C:/Users/ru25tas/Desktop/SaccTrial" # assigns the working directory

setwd(wDirectory) # sets up the working directory in the address assigned previously

library("saccades", lib.loc="~/R/win-library/3.2") # Imports the package saccades, used to detect the fixations

library("eyetrackR", lib.loc="~/R/win-library/3.2") # Imports the package eyetrakR, used to detect saccades ## This package is not compatible with the sampling ratio of the pupul eye tracker, so I need to pimplement another

name<-"MM411.csv" # gives "name" the name of the file to import. This is done so the process can be automated eventually

gameMap<-read.csv(name, header=TRUE, sep="\t", dec=".") # imports the CSV file to the variable gameMap

gameMap<-as.data.frame(gameMap) # transforms the variable gameMap into a data frame

id_start<-1 # counter for the following cycle
id_end<-nrow(gameMap) # counter for the following cycle


for (i in id_start:id_end) {  # This cycle is done in order to nullify the data when the gaze is not in the specified are, thus avoiding calculating gaze outside the specified areas
     if (gameMap[i,"on_srf"]=="False") { # This cycle takes the fields in the database, identifies the ones named "false" under the column "on surface" and transforms the row into zeros so it is calculated as missing data and not as a fixation
          gameMap[i,]<-0
     }
}

saveName<-paste("pros",name, sep="") # Creates a variable "saveName" containing a variation of the name of the file so it doesn't match the importing file name when exporting it

write.csv(gameMap, saveName) # writes a CSV file with the processed entry datafiel

sacTrial<-read.csv(saveName, header=TRUE, sep=",", dec=".") # imports the file that has been previously exported under the variable "sacTrial"

sample<-as.data.frame(cbind(time=sacTrial[,"world_timestamp"], trial=sacTrial[,"world_frame_idx"], x=sacTrial[,"x_norm"], y=sacTrial[,"y_norm"] )) # takes the CVS file and binds the rows necessary for dettecting fixations under the name "sample"

fixations<-0

fixations<-detect.fixations(sample, lambda=7, smooth.coordinates = TRUE, smooth.saccades = TRUE) # detects fixations using the package "saccades" and stores them under the variable "fixations"

# Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of the algoritm creator

fixations<-as.data.frame(fixations) # transforms the variable "fixations" into a data frame 

nrFix<-0

nrFix<-nrow(fixations) # detects the amount of fows in the array "fixations". This works under the assumption that each row is indeed a fixation 

nrFix<-as.data.frame(nrFix)

row.names(nrFix)<-"Number of Fixations"

summ<-calculate.summary(fixations) # makes a summary of the array "fixations" and stores it in the variable "summ"

#View(summ)

fix<-0

fix<-as.data.frame(cbind(start=fixations[,"start"], end=fixations[,"end"], x=fixations[,"x"], y=fixations[,"y"])) # takes the variable "fixations" and binds the rows necessary for carrying out a diagnostic plot

#diagnostic.plot(sample, fix)

fixationsName<-paste("fixationDB", name, sep="")
nrFixName<-paste("nrFixations", name, sep="")
summaryName<-paste("summary", name, sep="")

write.csv(fixations, fixationsName) # Writes a CSV file containing the matix of fixations with their data
write.csv(nrFix, nrFixName) # writes a CSV file containing the amount of fixations of the sample
write.csv(summ, summaryName) # writes a CSV file containing the summary of fixations data

#sacTrial<-read.csv(saveName, header=TRUE, sep=",", dec=".")

#xy<-as.matrix(cbind(sacTrial[,"x_norm"], sacTrial [,"y_norm"]))

#velocity<-vecvel(xy, 60, 1)

#saccades<-microsacc(xy, velocity, VFAC=6, MINDUR=3, discardMS = TRUE, removeGlissades = TRUE, DTGLISS = 30, MSamplCrit = 0.5)

#saccades<-as.data.frame(saccades)

#nrSac<-nrow(saccades)

#write.csv(saccades, "saccades.csv")
#write.csv(nrSac, "nrSac.csv")
