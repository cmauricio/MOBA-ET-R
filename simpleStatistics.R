#b<-tapply(fullDatabase$NrFixationsGameT1, fullDatabase$Tier, mean)
#View(c)
#aggregate(verbal, by=list(schule,sex), FUN=mean)#it tells to separate the data in as many variables as we want
#aov(verbal~sex*schule)#perform an analysis of variables (the first factor is the dependant)
#barplot(table(schule)/length(schule))
#hist(iq) # Makes a histogram of the variable sampling the variable automatically
#a<-subset(fullDatabase, subset=Tier=="silver")
#Excluding missing values from analyses
#na.rm=T
# create new dataset without missing data 
#newdata <- na.omit(mydata)

#####Getting the data#####

wDirectory<-"C:/Users/Mauro/Dropbox/ETDataAnalysis"
csvFile<-"151105-FullDB.csv"

setwd(wDirectory)

fullDatabase<-read.csv2(csvFile, header=TRUE, sep=";", dec=",")
fullDatabase<-as.data.frame(fullDatabase) 

fullDatabase[,"Session"]<-as.factor(fullDatabase[,"Session"])
fullDatabase[,"Age"]<-as.integer(fullDatabase[,"Age"])
fullDatabase[,"Gender"]<-as.factor(fullDatabase[,"Gender"])
fullDatabase[,"Group"]<-as.factor(fullDatabase[,"Group"])
fullDatabase[,"Expertise"]<-as.factor(fullDatabase[,"Expertise"])
fullDatabase[,"Condition"]<-as.integer(fullDatabase[,"Condition"])
fullDatabase[,"Game"]<-as.factor(fullDatabase[,"Game"])
fullDatabase[,"Tier"]<-factor(fullDatabase[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))
fullDatabase[,"Division"]<-factor(fullDatabase[,"Division"], order=T, levels=c("1","2","3","4","5"))
fullDatabase[,"GroupByTier"]<-factor(fullDatabase[,"GroupByTier"], order=T, levels=c("A","B","C","D","E","F","G"))
fullDatabase[,"Elo"]<-as.integer(fullDatabase[,"Elo"])
fullDatabase[,"GroupByElo"]<-factor(fullDatabase[,"GroupByElo"], order=T, levels=c("A","B","C","D","E"))
fullDatabase[,"Outcome"]<-factor(fullDatabase[,"Outcome"], order=T, levels=c("lose","win"))

n<-which(colnames(fullDatabase)=="GazeGame.MM")


#####Spliting databases#####

#####Gatting the game Time#####

gameTimeTierDB<-cbind(fullDatabase$GroupByTier,fullDatabase[,40],fullDatabase[,41],fullDatabase[,42],fullDatabase[43])
#gameTimeTierDB<-na.omit(gameTimeTierDB)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByTier","SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")

plot(gameTimeTierDB)

#####Percentage of Gaze by Tier#####

percGazeDB<-cbind(fullDatabase$GroupByTier,fullDatabase[,37],fullDatabase[,38],fullDatabase[,39])
#percGazeDB<-na.omit(percGazeDB)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByTier","PercGazeGame","PercGazeMM","PercGazeout")

plot(percGazeDB)

#####Number of fixations by Tier#####

numberFixationsDB<-cbind(fullDatabase$GroupByTier,fullDatabase$NrFixationsGame, fullDatabase$NrFixNOMM,fullDatabase$NrFixationsMM)
#numberFixationsDB<-na.omit(numberFixationsDB)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByTier", "TotalFixationsGame", "NrFixationsMap","NrFixationsMM")

plot(numberFixationsDB)

#####Number of Saccades by Tier#####

numberSaccadesDB<-cbind(fullDatabase$GroupByTier,fullDatabase$SaccadesMap,fullDatabase$SaccadesMM)
#numberSaccadesDB<-na.omit(numberSaccadesDB)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByTier","SaccadesGame","SaccadesMM")

plot(numberSaccadesDB)

#####relation saccade-fixation by Tier#####

SacFixRatioDB<-cbind(fullDatabase$GroupByTier,fullDatabase$SacFixRatioMap, fullDatabase$SacFixRatioMM)
#SacFixRatioDB<-na.omit(SacFixRatioDB)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByTier","SacFixRatioGame","SacFixRatioMM")

plot(SacFixRatioDB)


#####Average (statistical mean) of fixations and saccades per game#####

meanSaccadesMap<-numberSaccadesDB$SaccadesGame/gameTimeTierDB$SurfaceVisGameSecs
meanSaccadesMM<-numberSaccadesDB$SaccadesMM/gameTimeTierDB$SurfaceVisMMSecs

meanSacsFixTier<-cbind(fullDatabase$GroupByTier,fullDatabase$MeanFixationsGame, meanSaccadesMap, fullDatabase$MeanFixationsMM, meanSaccadesMM)
meanSacsFixTier<-as.data.frame(meanSacsFixTier)

colnames(meanSacsFixTier)<-c("GroupByTier","MeanFixMap","MeanSacMap", "MeanFixMM", "MeanSacMM")

plot(meanSacsFixTier)

with(meanSacsFixTier, plot(MeanFixMM,MeanSacMM, col = GroupByTier)) #Makes a plot with colour based on the Tier
abline(h = 12, lwd = 2, lty = 2)

####Mean of fixation duration#####

meanFixTime<-cbind(fullDatabase$GroupByTier,fullDatabase$MeanDurationGame, fullDatabase$MeanDurationMM)
meanFixTime<-as.data.frame(meanFixTime)

colnames(meanFixTime)<-c("GroupByTier","MeanFixDurationMap","MeanFixDurationMM")

plot(meanFixTime)

#####Percentage of fixations according to time#####

perc1<-avgSacsFixTierTime$AvgFixMap*numberFixationsDB$NrFixationsMap
perc<-(sqrt(perc1)/sqrt(gameTimeTierDB$SurfaceVisGameSecs)) #bad

View(perc1)


#####Percentage of gaze according to time#####



#####Average of playing time according to game and tier#####


#####Print exploratory plots##### 

png('GameTierExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(gameTimeTierDB)
dev.off()
png('PercGazeExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(percGazeDB)
dev.off()
png('NumberFixationsExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(numberFixationsDB)
dev.off()
png('NumberSaccadesExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(numberSaccadesDB)
dev.off()
png('MeanSaccades.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(meanSacsFixTier)
dev.off()
png('MeanFixations.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(meanFixTime)
dev.off()

########################
png('saccadesmap.png', width = 1024, height = 768, units = "px", pointsize = 16) #Opens the writing machine

explore<-par(mfcol=c(2,3)) #plot various plots in one sheet

plot(fullDatabase$SaccadesMapT1~fullDatabase$Tier, main="saccades1")#The tilde is made with alt+126. At the left of the Tilde there is the dependant variable. An * between factors (in the right) is to make analysis of diferent factor interactions
abline(lm(fullDatabase$SaccadesMapT1~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT1~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SaccadesMapT2~fullDatabase$Tier, main="saccades2")
abline(lm(fullDatabase$SaccadesMapT2~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT2~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SaccadesMapT3~fullDatabase$Tier, main="saccades3")
abline(lm(fullDatabase$SaccadesMapT3~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT3~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SaccadesMapT4~fullDatabase$Tier, main="saccades4")
abline(lm(fullDatabase$SaccadesMapT4~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT4~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SaccadesMapT5~fullDatabase$Tier, main="saccades5")
abline(lm(fullDatabase$SaccadesMapT5~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT5~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SaccadesMapT6~fullDatabase$Tier, main="saccades6")
abline(lm(fullDatabase$SaccadesMapT6~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SaccadesMapT6~fullDatabase$Tier, f=2/3, iter=10), col="blue")

par(explore)
dev.off() #closes the writing device

#######################################

#to make a scatterplot  with a regression line and a smooth regression line
plot(fullDatabase$SacFixRatioGameT1 ~fullDatabase$Tier)
abline(lm(fullDatabase$SacFixRatioGameT1~fullDatabase$Tier), col="red")
lines(lowess(fullDatabase$SacFixRatioGameT1~fullDatabase$Tier, f=2/3, iter=10), col="blue")

plot(fullDatabase$SacFixRatioGameT1)

b<-tapply(fullDatabase$NrFixationsGameT1, fullDatabase$Tier, mean)
c<-tapply(fullDatabase$NrFixationsGameT2, fullDatabase$Tier, mean)

plot(b,c)

fit<-aov(fullDatabase$SacFixRatioGameT1~fullDatabase$Tier*fullDatabase$Gender)

plot(fit)

###############################

png('saccadesmap2.png', width = 1024, height = 768, units = "px", pointsize = 13) #Opens the writing machine

explore<-par(mfcol=c(4,6))

plot(fullDatabase$SacFixRatioGameT1~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioGameT2~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioGameT3~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioGameT4~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioGameT5~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioGameT6~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)

par(explore)
dev.off()

png('saccadesmap3.png', width = 1024, height = 768, units = "px", pointsize = 13) #Opens the writing machine

explore<-par(mfcol=c(4,6))

plot(fullDatabase$SacFixRatioMMT1~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioMMT2~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioMMT3~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioMMT4~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioMMT5~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)
plot(fullDatabase$SacFixRatioMMT6~fullDatabase$Tier*fullDatabase$Age*fullDatabase$Gender*fullDatabase$Group)

par(explore)
dev.off()

###########################

tbl<-table(fullDatabase$Tier,fullDatabase$SacFixRatioGameT1)
tbl

chisq.test()

############################

polyserial(x=fullDatabase$NrFixationsGameT1, y=fullDatabase$Tier, ML=T, std.err=T, maxcor=.9999, bins=4)

#######################








