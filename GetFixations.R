library("saccades", lib.loc="~/R/win-library/3.2")

setwd("C:/Users/ru25tas/Desktop/SaccTrial")

sacTrial<-read.csv(saveName, header=TRUE, sep=",", dec=".")

sample<-as.data.frame(cbind(time=sacTrial[,"world_timestamp"], trial=sacTrial[,"world_frame_idx"], x=sacTrial[,"x_norm"], y=sacTrial[,"y_norm"] ))

fixations<-detect.fixations(sample, lambda=15, smooth.coordinates = TRUE, smooth.saccades = TRUE)

fixations<-as.data.frame(fixations)

nrFix<-nrow(fixations)

summ<-calculate.summary(fixations)

#View(summ)

fix<-as.data.frame(cbind(start=fixations[,"start"], end=fixations[,"end"], x=fixations[,"x"], y=fixations[,"y"]))

#diagnostic.plot(sample, fix)

write.csv(fixations, "fixations.csv")
write.csv(nrFix, "nrFixations.csv")
write.csv(summ, "summFixations.csv")
