#####Getting the data#####

wDirectory<-"C:/Users/ru25tas/Desktop/Analysis"
csvFile<-"151201-lolExpertsDB.csv"

setwd(wDirectory)

fullDatabase<-read.csv2(csvFile, header=TRUE, sep=";", dec=",",na.strings="NA")
fullDatabase<-as.data.frame(fullDatabase) 

# General fields
fullDatabase[,"Session"]<-as.factor(fullDatabase[,"Session"])
fullDatabase[,"Age"]<-as.integer(fullDatabase[,"Age"])
fullDatabase[,"Gender"]<-as.factor(fullDatabase[,"Gender"])
fullDatabase[,"Group"]<-as.factor(fullDatabase[,"Group"])
fullDatabase[,"Expertise"]<-as.factor(fullDatabase[,"Expertise"])
fullDatabase[,"Condition"]<-as.factor(fullDatabase[,"Condition"])
fullDatabase[,"Game"]<-as.factor(fullDatabase[,"Game"])
fullDatabase[,"Outcome"]<-factor(fullDatabase[,"Outcome"], order=T, levels=c("lost","win"))

# Only LoL Experts fields
fullDatabase[,"Tier"]<-factor(fullDatabase[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))
fullDatabase[,"Division"]<-factor(fullDatabase[,"Division"], order=T, levels=c("1","2","3","4","5"))
fullDatabase[,"GroupByTier"]<-factor(fullDatabase[,"GroupByTier"], order=T, levels=c("A","B","C","D","E","F","G"))

# Only DOTA fields
fullDatabase[,"Elo"]<-as.integer(fullDatabase[,"Elo"])
fullDatabase[,"GroupByElo"]<-factor(fullDatabase[,"GroupByElo"], order=T, levels=c("A","B","C","D","E"))


fullDatabase<-na.omit(fullDatabase)  # listwise deletion of missing values


#####Spliting databases for LoL Experts#####

group_a<-subset(fullDatabase, GroupByTier=="A")
group_b<-subset(fullDatabase, GroupByTier=="B")
group_c<-subset(fullDatabase, GroupByTier=="C")
group_d<-subset(fullDatabase, GroupByTier=="D")
group_e<-subset(fullDatabase, GroupByTier=="E")
group_f<-subset(fullDatabase, GroupByTier=="F")
group_g<-subset(fullDatabase, GroupByTier=="G")

group_silver<-subset(fullDatabase, Tier=="silver")
group_gold<-subset(fullDatabase, Tier=="gold")
group_diamond<-subset(fullDatabase, Tier=="diamond")

expert_expertDB<-subset(fullDatabase, Condition=="expert")
expert_noviceDB<-subset(fullDatabase, Condition=="novice")


#####Spliting databases for DOTA Experts#####

group_a<-subset(fullDatabase, GroupByTier=="A")
group_b<-subset(fullDatabase, GroupByTier=="B")
group_c<-subset(fullDatabase, GroupByTier=="C")
group_d<-subset(fullDatabase, GroupByTier=="D")
group_e<-subset(fullDatabase, GroupByTier=="E")

#####Spliting databases for LoL Novices#####

expert_tutorialDB<-subset(fullDatabase, Condition=="tutorial")
expert_noviceDB<-subset(fullDatabase, Condition=="novice")
expert_ev1DB<-subset(fullDatabase, Condition=="evaluation1")
expert_ev2DB<-subset(fullDatabase, Condition=="evaluation2")


#####Gatting the game Time#####

gameTimeTierDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$SurfaceVisGame.secs.,fullDatabase$SurfaceVisMM.secs.,fullDatabase$TotalSurfaceVisTotal.secs.,fullDatabase$VisibilityLost..secs.)
#gameTimeTierDB<-na.omit(gameTimeTierDB)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByTier", "Condition", "Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")

plot(gameTimeTierDB, col=fullDatabase$GroupByTier)

#####Percentage of Gaze by Tier#####

percGazeDB<-cbind(fullDatabase$GroupByTier,fullDatabase$PercGazeGame,fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$PercGazeMM,fullDatabase$PercGazeOut)
#percGazeDB<-na.omit(percGazeDB)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByTier", "Condition", "Outcome", "PercGazeGame","PercGazeMM","PercGazeout")

plot(percGazeDB, col=fullDatabase$GroupByTier)

#####Number of fixations by Tier#####

numberFixationsDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$NrFixationsGame, fullDatabase$NrFixNOMM,fullDatabase$NrFixationsMM)
#numberFixationsDB<-na.omit(numberFixationsDB)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByTier", "Condition", "Outcome", "TotalFixationsGame", "NrFixationsMap","NrFixationsMM")

plot(numberFixationsDB, col=fullDatabase$GroupByTier)

#####Number of Saccades by Tier#####

numberSaccadesDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$SaccadesMap,fullDatabase$SaccadesMM)
#numberSaccadesDB<-na.omit(numberSaccadesDB)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByTier", "Condition", "Outcome", "SaccadesGame","SaccadesMM")

plot(numberSaccadesDB, col=fullDatabase$GroupByTier)

#####relation saccade-fixation by Tier#####

SacFixRatioDB<-cbind(fullDatabase$GroupByTier,fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$SacFixRatioMap, fullDatabase$SacFixRatioMM)
#SacFixRatioDB<-na.omit(SacFixRatioDB)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByTier", "Condition", "Outcome", "SacFixRatioGame","SacFixRatioMM")

plot(SacFixRatioDB, col=fullDatabase$GroupByTier)

#####Average (statistical mean) of fixations and saccades per game#####

meanSaccadesMap<-numberSaccadesDB$SaccadesGame/gameTimeTierDB$SurfaceVisGameSecs
meanSaccadesMM<-numberSaccadesDB$SaccadesMM/gameTimeTierDB$SurfaceVisMMSecs

meanSacsFixTier<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$MeanFixationsGame, meanSaccadesMap, fullDatabase$MeanFixationsMM, meanSaccadesMM)
meanSacsFixTier<-as.data.frame(meanSacsFixTier)

colnames(meanSacsFixTier)<-c("GroupByTier","Condition", "Outcome", "MeanFixMap","MeanSacMap", "MeanFixMM", "MeanSacMM")

plot(meanSacsFixTier, col=fullDatabase$GroupByTier)

with(meanSacsFixTier, plot(MeanFixMM,MeanSacMM, col = GroupByTier)) #Makes a plot with colour based on the Tier
abline(h = 12, lwd = 2, lty = 2)

####Mean of fixation duration#####

meanFixTime<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$MeanDurationGame, fullDatabase$MeanDurationMM)
meanFixTime<-as.data.frame(meanFixTime)

colnames(meanFixTime)<-c("GroupByTier","Condition", "Outcome", "MeanFixDurationMap","MeanFixDurationMM")

plot(meanFixTime, col=fullDatabase$GroupByTier)

#####Percentage of fixations according to time#####

#perc1<-avgSacsFixTierTime$AvgFixMap*numberFixationsDB$NrFixationsMap
#perc<-(sqrt(perc1)/sqrt(gameTimeTierDB$SurfaceVisGameSecs)) #bad

#View(perc1)


#####Percentage of gaze according to time#####



#####Average of playing time according to game and tier#####


#####Print exploratory plots##### 

png('GameTierExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(gameTimeTierDB, col=fullDatabase$GroupByTier)
dev.off()
png('PercGazeExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(percGazeDB, col=fullDatabase$GroupByTier)
dev.off()
png('NumberFixationsExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(numberFixationsDB, col=fullDatabase$GroupByTier)
dev.off()
png('NumberSaccadesExpl.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(numberSaccadesDB, col=fullDatabase$GroupByTier)
dev.off()
png('MeanSaccades.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(meanSacsFixTier, col=fullDatabase$GroupByTier)
dev.off()
png('MeanFixations.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(meanFixTime, col=fullDatabase$GroupByTier)
dev.off()
png('SacFixRatio.png', width = 1024, height = 768, units = "px", pointsize = 13)
plot(SacFixRatioDB, col=fullDatabase$GroupByTier)
dev.off()

##### Next analyses by tier and game #####



##### Descriptives and plots #####

plot(fullDatabase$Gender)
plot(fullDatabase$Age)
plot(fullDatabase$Group)
plot(fullDatabase$GroupByTier)
plot(fullDatabase$Condition)


#hist(iq) # Makes a histogram of the variable sampling the variable automatically






########################
########################
########################
########################
########################
########################

##### Other analyses #####

#means and standard deviations, including the range for valid data (+/- 3 sd)

mn_game<-mean(fullDatabase$NrFixNOMM,na.rm=T)
sd_game<-sd(fullDatabase$NrFixNOMM, na.rm=T)
range_sd_game<-c(mn_game-(3*sd_game),mn_game+(3*sd_game))

mn_mm<-mean(fullDatabase$NrFixationsMM,na.rm=T)
sd_mm<-sd(fullDatabase$NrFixationsMM, na.rm=T)
range_sd_mm<-c(mn_mm-(3*sd_mm),mn_mm+(3*sd_mm))

mn_game_sac<-mean(fullDatabase$SaccadesMap,na.rm=T)
sd_game_sac<-sd(fullDatabase$SaccadesMap, na.rm=T)
range_sd_sac_game<-c(mn_game_sac-(3*sd_game_sac),mn_game_sac+(3*sd_game_sac))

mn_mm_sac<-mean(fullDatabase$SaccadesMM,na.rm=T)
sd_mm_sac<-sd(fullDatabase$SaccadesMM, na.rm=T)
range_sd_sac_mm<-c(mn_mm_sac-(3*sd_mm_sac),mn_mm_sac+(3*sd_mm_sac))

mn_game_rat<-mean(fullDatabase$SacFixRatioMap,na.rm=T)
sd_game_rat<-sd(fullDatabase$SacFixRatioMap, na.rm=T)
range_sd_sac_rat<-c(mn_game_rat-(3*sd_game_rat),mn_game_rat+(3*sd_game_rat))

mn_mm_rat<-mean(fullDatabase$SacFixRatioMM,na.rm=T)
sd_mm_rat<-sd(fullDatabase$SacFixRatioMM, na.rm=T)
range_sd_rat_mm<-c(mn_mm_rat-(3*sd_mm_rat),mn_mm_rat+(3*sd_mm_rat))

mn_vision_lost<-mean(fullDatabase$VisionLostPercentage,na.rm=T)
sd_vision_lost<-sd(fullDatabase$VisionLostPercentage,na.rm=T)
range_vision_lost<-c(mn_vision_lost-(3*sd_vision_lost),(mn_vision_lost+(3*sd_vision_lost)))


#b<-tapply(fullDatabase$NrFixationsGameT1, fullDatabase$Tier, mean)
#aggregate(verbal, by=list(schule,sex), FUN=mean)#it tells to separate the data in as many variables as we want
#aov(verbal~sex*schule)#perform an analysis of variables (the first factor is the dependant)




##### Chi squared ######

chisq.test(expert_expertDB$NrFixNOMM,expert_noviceDB$NrFixationsMM)

##### Linear regressions #####

Linear1<-lm(fullDatabase$NrFixNOMM ~ fullDatabase$NrFixationsMM)
summary(Linear1)
plot(Linear1)

plot(fullDatabase$NrFixNOMM ~ fullDatabase$NrFixationsMM)
abline(Linear1, col="red")


##### Cluster analyses #####

scaled_data<-scale(no_na_data$NrFixNOMM)# standardize variables

library(mclust)
fit <- Mclust(scaled_data)
plot(fit) # plot results 
summary(fit) # display the best model


# K-Means Clustering with 5 clusters
fit <- kmeans(scaled_data, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(scaled_data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(scaled_data, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)

##### Anova ######

anov_1<-aov(group_a$NrFixNOMM ~ group_a$NrFixationsMM)
summary(anov_1)
plot(anov_1)
boxplot(group_a$NrFixNOMM ~ group_a$NrFixationsMM)

########



