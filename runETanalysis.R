wDirectoryMap<-"C:/Users/ru25tas/Desktop/SaccTrial/ETData" # assigns the working directory
eyedata1<-eyetracking.analysis(wDirectoryMap, Datatype="Map", Lambda=7)

write.csv(eyedata1, "MapDatabases.csv")

wDirectoryMM<-"C:/Users/ru25tas/Desktop/SaccTrial/ETData" # assigns the working directory
eyedata2<-eyetracking.analysis(wDirectoryMM, Datatype="Minimap", Lambda=7)

# Lambda of 7 was chosen as the best fitting lambda for 60Hz taking into account different measurements done and the suggestions of Malsburg

write.csv(eyedata2, "MiniMapDatabases.csv")


