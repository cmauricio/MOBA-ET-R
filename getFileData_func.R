#Function for taking out all the data from different (same structure)
#Databases in a specific directory.
#The function can be used to extract the name of files in a specific directory

getFileData<- function(directory="C:/Users/ru25tas/Desktop/SaccTrial/ETData", id = 1:7) {  ##function declaration
     
     files<-list.files(directory, pattern="*.csv", full.names=TRUE) ## assings to the object "files" all the csv files in "directory". the full names return an array with all the name including the path.
     
     id_start<-id[1]
     id_end<-id[length(id)]
     
     getFileData<-data.frame() ## creates an empty data frame to bulk the data
     
     for (i in id_start:id_end) { ## bulk the data from all the files into the created dataframe
          getFileData <- rbind(getFileData,read.csv(files[i]))
          print (files[i])
     }
     
     getFileData ## returns the dataframe containing all the bulked data
}
