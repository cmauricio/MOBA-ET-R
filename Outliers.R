# Functions to extract outliers and to Clean outliers with more
# than three SD for the mean.

# Function to obtain outliers
get.outliers<-function (dataframe,variable=""){
     deviation<-0
     mn<-0
     i<-1
     column<-which(colnames(dataframe)==variable)
     outliers<-0
     deviation<-sd(dataframe[,column])
     mn<-mean(dataframe[,column])
     a<-mn+(3*deviation)
     b<-mn-(3*deviation)
     
     for(i in 1:length(dataframe[,column])) {
          if (is.na(dataframe[i,column])==T){
               next()
          } else if (dataframe[i,column]> a|| dataframe[i,column]< b){
               outliers<-rbind(outliers,dataframe[i,])
          }
     }
     outliers[-1,]
}



# Function to clean outliers
clean.outliers<-function (dataframe,variable=""){
     deviation<-0
     mn<-0
     i<-1
     counter<-0
     column<-which(colnames(dataframe)==variable)
     deviation<-sd(dataframe[,column])
     mn<-mean(dataframe[,column])
     a<-mn+(3*deviation)
     b<-mn-(3*deviation)
     
     for(i in 1:length(dataframe[,column])) {
          if (is.na(dataframe[i,column])==T){
               next()
          } else if (dataframe[i,column]> a|| dataframe[i,column]< b){
               dataframe[i,column]<-NA
               counter<-counter+1
          }
     }
     dataframe[,column]
}



