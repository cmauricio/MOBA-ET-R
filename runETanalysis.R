#This code runs the eye tracking analyses for all the raw data
#and uses the function 'eyetracking.analysis' found in the 
#file ETautoAnalysis_func.R

# Lambda of 2 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of Malsburg

wDirectoryMap<-"C:/Users/Mauro/Desktop/Experiment/Maps" # assigns the working directory
eyedata1<-eyetracking.analysis(wDirectoryMap, Datatype="Map", Lambda=2)

write.table(eyedata1, "MapDatabases.csv", sep=";", dec = ",")

wDirectoryMM<-"C:/Users/Mauro/Desktop/Experiment/Minimaps" # assigns the working directory
eyedata2<-eyetracking.analysis(wDirectoryMM, Datatype="Minimap", Lambda=2)

write.table(eyedata2, "MiniMapDatabases.csv", sep=";", dec = ",")


on.surface.gaze("C:/Users/Mauro/Desktop/New folder")

bind.cvsfiles("MM28111","MM28112")
