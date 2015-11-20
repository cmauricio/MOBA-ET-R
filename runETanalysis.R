wDirectoryMap<-"D:/RenamedProcessedData/Maps" # assigns the working directory
eyedata1<-eyetracking.analysis(wDirectoryMap, Datatype="Map", Lambda=2)

write.table(eyedata1, "MapDatabases.csv", sep=";", dec = ",")

wDirectoryMM<-"D:/RenamedProcessedData/Minimaps" # assigns the working directory
eyedata2<-eyetracking.analysis(wDirectoryMM, Datatype="Minimap", Lambda=2)

# Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of Malsburg

write.table(eyedata2, "MiniMapDatabases.csv", sep=";", dec = ",")


on.surface.gaze("D:/RenamedProcessedData")



