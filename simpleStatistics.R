##### Functions #####

# Function to make histograms with distribution line

distribution_hist<-function (variable, breaks=5, histcol="red", xlab="xlabel", ylab="ylabel", title="Histogram with Normal Curve", curvelength=50, curvecol="blue"){
     dataDist<-na.omit(variable)
     h<-hist(dataDist, breaks=breaks, col=histcol, xlab=xlab, ylab=ylab, main=title) 
     xfit<-seq(min(dataDist),max(dataDist),length=curvelength) #generates a sequence with seq
     yfit<-dnorm(xfit,mean=mean(dataDist),sd=sd(dataDist)) #gets the normal distribution dnorm
     yfit <- yfit*diff(h$mids[1:2])*length(dataDist) 
     lines(xfit, yfit, col=curvecol, lwd=2)
}

# Cleaning outliers 

outliers.na<-function (array){
     deviation<-0
     mn<-0
     i<-1
     counter<-0
     array2<-na.omit(array)
     deviation<-sd(array2)
     mn<-mean(array)
     a<-mn+3*deviation
     b<-mn-3*deviation
     
     for(i in 1:length(array)) {
          if (is.na(array[i])==T){
               next()
          } else if (array[i]> a|| array[i]< b){
               array[i]<-NA
               counter<-counter+1
          }
     }
     array     
}

# Anova

quick.anova<-function(var1, var2, var3){
     
     Data <- data.frame(Value=c(var1,var2, var3), Group=factor(rep(c("var1", "var2", "var3"), times=c(length(var1), length(var2), length(var3)))))
     print (summary(Data))
     an1<-aov(Value~Group, Data)
     tu<-TukeyHSD(an1)
     anova(an1)
     tu
}

quick.anova(group_silver$ratioSacTimeGame,group_gold$ratioSacTimeGame,group_diamond$ratioSacTimeGame)

##### Getting the data #####

library("graphics", lib.loc="C:/Program Files/R/R-3.2.1/library")

#setwd("C:/Users/Mauro/Dropbox/Analysis")
#setwd("C:/Users/ru25tas/Dropbox/Analysis")

wDirectory<-"C:/Users/ru25tas/Dropbox/Analysis"
csvFile<-"151201-FullETDatabase.csv"

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
fullDatabase[,"Tier"]<-factor(fullDatabase[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))
fullDatabase[,"Division"]<-factor(fullDatabase[,"Division"], order=T, levels=c("1","2","3","4","5"))
fullDatabase[,"GroupByTier"]<-factor(fullDatabase[,"GroupByTier"], order=T, levels=c("A","B","C","D","E","F","G"))
fullDatabase[,"Elo"]<-as.integer(fullDatabase[,"Elo"])
fullDatabase[,"GroupByElo"]<-factor(fullDatabase[,"GroupByElo"], order=T, levels=c("A","B","C","D","E"))
fullDatabase[,"SacFixRatioMap"]<-as.numeric(fullDatabase[,"SacFixRatioMap"])
fullDatabase[,"SacFixRatioMM"]<-as.numeric(fullDatabase[,"SacFixRatioMap"])

grouplist<-c("group_a","group_b", "group_c", "group_d","group_e","group_f", "group_g")
grouptier<-c("silver","gold","diamond")

# Ratio of saccades and fixations in time 

numberFixGame<-fullDatabase$NrFixNOMM
totalVisTimeGame<-fullDatabase$SurfaceVisGame.secs.
ratioFixTimeGame<-round(numberFixGame/totalVisTimeGame,2)
ratioFixTimeGame<-as.numeric(ratioFixTimeGame)

numberFixMM<-fullDatabase$NrFixationsMM
totalVisTimeMM<-fullDatabase$SurfaceVisMM.secs.
ratioFixTimeMM<-round(numberFixMM/totalVisTimeMM,2)
ratioFixTimeMM<-as.numeric(ratioFixTimeMM)

numberSacGame<-fullDatabase$SaccadesMap
totalVisTimeGame<-fullDatabase$SurfaceVisGame.secs.
ratioSacTimeGame<-round(numberSacGame/totalVisTimeGame,2)
ratioSacTimeGame<-as.numeric(ratioSacTimeGame)

numberSacMM<-fullDatabase$SaccadesMM
totalVisTimeMM<-fullDatabase$SurfaceVisMM.secs.
ratioSacTimeMM<-round(numberSacMM/totalVisTimeMM,2)
ratioSacTimeMM<-as.numeric(ratioSacTimeMM)

fullDatabase<-cbind(fullDatabase, ratioFixTimeGame,ratioFixTimeMM,ratioSacTimeGame,ratioSacTimeMM)

##### Descriptives database #####

wDirectory<-"C:/Users/ru25tas/Dropbox/Analysis"
csvFile<-"151201-Descriptives.csv"

setwd(wDirectory)

Descriptives<-read.csv2(csvFile, header=TRUE, sep=";", dec=",",na.strings="NA")
Descriptives<-as.data.frame(Descriptives) 

Descriptives[,"Registry"]<-as.integer(Descriptives[,"Registry"])
Descriptives[,"Game"]<-as.factor(Descriptives[,"Game"])
Descriptives[,"Expertise"]<-factor(Descriptives[,"Expertise"], order=T, levels=c("novice","expert"))
Descriptives[,"Participant"]<-as.integer(Descriptives[,"Participant"])
Descriptives[,"Group"]<-as.integer(Descriptives[,"Group"])
Descriptives[,"Age"]<-as.integer(Descriptives[,"Age"])
Descriptives[,"Gender"]<-as.factor(Descriptives[,"Gender"])
Descriptives[,"Studies"]<-factor(Descriptives[,"Studies"], order=T, levels=c("highschool","bachelor","postgraduate"))
Descriptives[,"MOBAs.Experience"]<-as.factor(Descriptives[,"MOBAs.Experience"])
Descriptives[,"Video.Gaming.Experience"]<-factor(Descriptives[,"Video.Gaming.Experience"], order=T, levels=c("nogame","casual","gamer","hardcore"))
Descriptives[,"Tier"]<-factor(Descriptives[,"Tier"], order=T, levels=c("unranked","bronce","silver","gold","diamond","platinum"))
Descriptives[,"GroupByTier"]<-factor(Descriptives[,"GroupByTier"], order=T, levels=c("unranked","A","B","C","D","F","G"))
Descriptives[,"GroupByElo"]<-factor(Descriptives[,"GroupByElo"], order=T, levels=c("A","B","C","D","E"))

numberoflolnovices<-nrow(subset(Descriptives, Game=="lol"&Expertise=="novice"))
numberoflolexperts<-nrow(subset(Descriptives, Game=="lol"&Expertise=="expert"))
numberofdotaexperts<-nrow(subset(Descriptives, Game=="dota2"))
numberoffemales<-nrow(subset(Descriptives, Gender=="female"))
numberofmales<-nrow(subset(Descriptives, Gender=="male"))

##### Getting the different databases #####

lolNovicesDB<-subset(fullDatabase, Game=="lol" & Expertise=="novice")
lolNovicesDB[,"Tier"]<-NULL
lolNovicesDB[,"Division"]<-NULL
lolNovicesDB[,"GroupByTier"]<-NULL
lolNovicesDB[,"Elo"]<-NULL
lolNovicesDB[,"GroupByElo"]<-NULL
lolNovicesDB<-na.omit(lolNovicesDB)
lolNovicesDB[,"GazeInGame"]<-outliers.na(lolNovicesDB$GazeInGame)
lolNovicesDB[,"GazeMM"]<-outliers.na(lolNovicesDB$GazeMM)
lolNovicesDB[,"NrFixationsGame"]<-outliers.na(lolNovicesDB$NrFixNOMM)
lolNovicesDB[,"NrFixationsMM"]<-outliers.na(lolNovicesDB$NrFixationsMM)
lolNovicesDB[,"SaccadesMAp"]<-outliers.na(lolNovicesDB$SaccadesMap)
lolNovicesDB[,"SaccadesMM"]<-outliers.na(lolNovicesDB$SaccadesMM)
lolNovicesDB[,"SacFixRatioMap"]<-outliers.na(lolNovicesDB$SacFixRatioMap)
lolNovicesDB[,"SacFixRatioMM"]<-outliers.na(lolNovicesDB$SacFixRatioMM)
lolNovicesDB[,"ratioFixTimeGame"]<-outliers.na(lolNovicesDB$ratioFixTimeGame)
lolNovicesDB[,"ratioFixTimeMM"]<-outliers.na(lolNovicesDB$ratioFixTimeMM)
lolNovicesDB[,"ratioSacTimeGame"]<-outliers.na(lolNovicesDB$ratioSacTimeGame)
lolNovicesDB[,"ratioSacTimeMM"]<-outliers.na(lolNovicesDB$ratioSacTimeMM)
lolNovicesDB<-na.omit(lolNovicesDB)

lolExpertsDB<-subset(fullDatabase, Game=="lol" & Expertise=="expert")
lolExpertsDB[,"Elo"]<-NULL
lolExpertsDB[,"GroupByElo"]<-NULL
lolExpertsDB<-na.omit(lolExpertsDB)

lolExpExpDB<-subset(lolExpertsDB, Condition=="expert")
lolExpExpDB[,"GazeInGame"]<-outliers.na(lolExpExpDB$GazeInGame)
lolExpExpDB[,"GazeMM"]<-outliers.na(lolExpExpDB$GazeMM)
lolExpExpDB[,"NrFixationsGame"]<-outliers.na(lolExpExpDB$NrFixNOMM)
lolExpExpDB[,"NrFixationsMM"]<-outliers.na(lolExpExpDB$NrFixationsMM)
lolExpExpDB[,"SaccadesMAp"]<-outliers.na(lolExpExpDB$SaccadesMap)
lolExpExpDB[,"SaccadesMM"]<-outliers.na(lolExpExpDB$SaccadesMM)
lolExpExpDB[,"SacFixRatioMap"]<-outliers.na(lolExpExpDB$SacFixRatioMap)
lolExpExpDB[,"SacFixRatioMM"]<-outliers.na(lolExpExpDB$SacFixRatioMM)
lolExpExpDB[,"ratioFixTimeGame"]<-outliers.na(lolExpExpDB$ratioFixTimeGame)
lolExpExpDB[,"ratioFixTimeMM"]<-outliers.na(lolExpExpDB$ratioFixTimeMM)
lolExpExpDB[,"ratioSacTimeGame"]<-outliers.na(lolExpExpDB$ratioSacTimeGame)
lolExpExpDB[,"ratioSacTimeMM"]<-outliers.na(lolExpExpDB$ratioSacTimeMM)
lolExpExpDB<-na.omit(lolExpExpDB)

lolExpNovDB<-subset(lolExpertsDB, Condition=="novice")
lolExpNovDB[,"GazeInGame"]<-outliers.na(lolExpNovDB$GazeInGame)
lolExpNovDB[,"GazeMM"]<-outliers.na(lolExpNovDB$GazeMM)
lolExpNovDB[,"NrFixationsGame"]<-outliers.na(lolExpNovDB$NrFixNOMM)
lolExpNovDB[,"NrFixationsMM"]<-outliers.na(lolExpNovDB$NrFixationsMM)
lolExpNovDB[,"SaccadesMAp"]<-outliers.na(lolExpNovDB$SaccadesMap)
lolExpNovDB[,"SaccadesMM"]<-outliers.na(lolExpNovDB$SaccadesMM)
lolExpNovDB[,"SacFixRatioMap"]<-outliers.na(lolExpNovDB$SacFixRatioMap)
lolExpNovDB[,"SacFixRatioMM"]<-outliers.na(lolExpNovDB$SacFixRatioMM)
lolExpNovDB[,"ratioFixTimeGame"]<-outliers.na(lolExpNovDB$ratioFixTimeGame)
lolExpNovDB[,"ratioFixTimeMM"]<-outliers.na(lolExpNovDB$ratioFixTimeMM)
lolExpNovDB[,"ratioSacTimeGame"]<-outliers.na(lolExpNovDB$ratioSacTimeGame)
lolExpNovDB[,"ratioSacTimeMM"]<-outliers.na(lolExpNovDB$ratioSacTimeMM)
lolExpNovDB<-na.omit(lolExpNovDB)

dota2DB<-subset(fullDatabase, Game=="dota")
dota2DB[,"Tier"]<-NULL
dota2DB[,"Division"]<-NULL
dota2DB[,"GroupByTier"]<-NULL
dota2DB<-na.omit(dota2DB)
dota2DB[,"GazeInGame"]<-outliers.na(dota2DB$GazeInGame)
dota2DB[,"GazeMM"]<-outliers.na(dota2DB$GazeMM)
dota2DB[,"NrFixationsGame"]<-outliers.na(dota2DB$NrFixNOMM)
dota2DB[,"NrFixationsMM"]<-outliers.na(dota2DB$NrFixationsMM)
dota2DB[,"SaccadesMAp"]<-outliers.na(dota2DB$SaccadesMap)
dota2DB[,"SaccadesMM"]<-outliers.na(dota2DB$SaccadesMM)
dota2DB[,"SacFixRatioMap"]<-outliers.na(dota2DB$SacFixRatioMap)
dota2DB[,"SacFixRatioMM"]<-outliers.na(dota2DB$SacFixRatioMM)
dota2DB[,"ratioFixTimeGame"]<-outliers.na(dota2DB$ratioFixTimeGame)
dota2DB[,"ratioFixTimeMM"]<-outliers.na(dota2DB$ratioFixTimeMM)
dota2DB[,"ratioSacTimeGame"]<-outliers.na(dota2DB$ratioSacTimeGame)
dota2DB[,"ratioSacTimeMM"]<-outliers.na(dota2DB$ratioSacTimeMM)
dota2DB<-na.omit(dota2DB)

##### Descriptive plots #####

genderCount<-table(Descriptives$Gender)
genderPerc<-round(100*genderCount/sum(genderCount),2)
pie(genderPerc, edges=500, radius=1, main="Participants by gender", labels=paste(genderPerc,"%"), col=heat.colors(length(genderCount)))
legend("topright", c("female","male"), cex=1, fill=heat.colors(length(genderCount)))

gameCount<-table(Descriptives$Game)
gamePerc<-round(100*gameCount/sum(gameCount),2)
pie(gamePerc,edges = 500, radius=1, main="Participation by game",labels=paste(genderPerc,"%"), col=heat.colors(length(gameCount)))
legend("topright", c("DOTA2","League of Legends"), cex=0.8, fill=heat.colors(length(gameCount)))

expertiseCount<-table(Descriptives$Expertise)
expertisePerc<-round(100*expertiseCount/sum(expertiseCount),2)
pie(expertisePerc,edges=500, radius=1, main="Participants expertise", labels=paste(expertisePerc,"%"), col=heat.colors(length(expertiseCount)))
legend("topright", c("experts","novices"), cex=1, fill=heat.colors(length(expertiseCount)))

labelsMobaExperience <- table(Descriptives$MOBAs.Experience)
midpoints <- barplot(table(Descriptives$MOBAs.Experience), col=heat.colors(1,alpha=1), border="black", main="Participants by Tier", xlab="Tier", ylab="Frequency")
text(midpoints, c(3,4,6,2,7,23), labels=labelsMobaExperience)

labelsGameExperience <- table(Descriptives$Video.Gaming.Experience)
midpoints<-barplot(table(Descriptives$Video.Gaming.Experience), col=heat.colors(1,alpha=1), border="black", main="Participants by Tier", xlab="Tier", ylab="Frequency")
text(midpoints, c(3,6,14,23), labels=labelsGameExperience)

hist(Descriptives$Age,freq=T, col=heat.colors(1,alpha=1), border="black", main="Age frequency of participants", xlab = "Age", ylab="Frequency", labels=T)

labelsGameTier <- table(Descriptives$Tier)
midpoints<-barplot(table(Descriptives$Tier), col=heat.colors(1,alpha=1), border="black", main="Participants by Tier", xlab="Tier", ylab="Frequency")
text(midpoints, c(3,6,14,23), labels=labelsGameTier)

tierCount<-table(Descriptives$Tier)
tierPerc<-round(100*tierCount/sum(tierCount),2)
pie(tierPerc, edges=500, radius=1, main="Participants by tier",labels=paste(tierPerc,"%"),  col=heat.colors(length(tierCount)))
legend("topright", c("Diamond","gold", "silver", "unranked"), cex=1, fill=heat.colors(length(tierCount)))

labelsGameTiergroup <- table(Descriptives$GroupByTier)
midpoints<-barplot(table(Descriptives$GroupByTier), col=heat.colors(1,alpha=1), border="black", main="Participants group by tier", xlab="Group", ylab="Frequency")
text(midpoints, c(3,6,14,23), labels=labelsGameTiergroup)
#legend("topright", c("novices","experts"), cex=1, fill=heat.colors(length(expertiseCount)))

tierGroupCount<-table(Descriptives$GroupByTier)
tierGroupPerc<-round(100*tierGroupCount/sum(tierGroupCount),2)
pie(tierGroupPerc, edges=500, radius=1, main="Participants by tier group", labels=paste(tierGroupPerc,"%"), col=heat.colors(length(tierGroupCount)))
legend("topright", c("A","B", "C","D","E","F","G"), cex=1, fill=heat.colors(length(tierGroupCount)))

labelsGameElogroup <- table(Descriptives$GroupByElo)
midpoints<-barplot(table(Descriptives$GroupByElo), col=heat.colors(1,alpha=1), border="black", main="Participants group by Elo", xlab="Tier", ylab="Frequency")
text(midpoints, c(3,6,14,23), labels=labelsGameElogroup)
#legend("topright", c("novices","experts"), cex=1, fill=heat.colors(length(expertiseCount)))

eloGroupCount<-table(Descriptives$GroupByElo)
eloGroupPerc<-round(100*eloGroupCount/sum(eloGroupCount),2)
pie(eloGroupPerc, edges=500, radius=1, main="Participants by gender", labels=paste(eloGroupPerc,"%"), col=heat.colors(length(eloGroupCount)))
legend("topright", c("female","male"), cex=1, fill=heat.colors(length(genderCount)))

##### Exploratory plots #####

# Game Time

gameTimeTierDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Outcome, fullDatabase$SurfaceVisGame.secs.,fullDatabase$SurfaceVisMM.secs.,fullDatabase$TotalSurfaceVisTotal.secs.,fullDatabase$VisibilityLost..secs.)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByTier", "Condition", "Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")
plot(gameTimeTierDB, col=fullDatabase$GroupByTier)

gameTimeTierDB<-cbind(lolExpExpDB$GroupByTier, lolExpExpDB$Outcome, lolExpExpDB$SurfaceVisGame.secs.,lolExpExpDB$SurfaceVisMM.secs.,lolExpExpDB$TotalSurfaceVisTotal.secs.,lolExpExpDB$VisibilityLost..secs.)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByTier", "Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")
plot(gameTimeTierDB, col=lolExpExpDB$GroupByTier)

gameTimeTierDB<-cbind(lolExpNovDB$GroupByTier, lolExpNovDB$Outcome, lolExpNovDB$SurfaceVisGame.secs.,lolExpNovDB$SurfaceVisMM.secs.,lolExpNovDB$TotalSurfaceVisTotal.secs.,lolExpNovDB$VisibilityLost..secs.)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByTier", "Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")
plot(gameTimeTierDB, col=lolExpNovDB$GroupByTier)

gameTimeTierDB<-cbind(lolNovicesDB$Outcome, lolNovicesDB$SurfaceVisGame.secs.,lolNovicesDB$SurfaceVisMM.secs.,lolNovicesDB$TotalSurfaceVisTotal.secs.,lolNovicesDB$VisibilityLost..secs.)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")
plot(gameTimeTierDB)

gameTimeTierDB<-cbind(dota2DB$GroupByElo, dota2DB$Outcome, dota2DB$SurfaceVisGame.secs.,dota2DB$SurfaceVisMM.secs.,dota2DB$TotalSurfaceVisTotal.secs.,dota2DB$VisibilityLost..secs.)
gameTimeTierDB<-as.data.frame(gameTimeTierDB)
colnames(gameTimeTierDB)<-c("GroupByElo", "Outcome", "SurfaceVisGameSecs","SurfaceVisMMSecs","SurfaceVisTotalSecs","SurfaceVislostSecs")
plot(gameTimeTierDB, col=dota2DB$GroupByElo)

# Percentage of Gaze by Tier

percGazeDB<-cbind(fullDatabase$GroupByTier,fullDatabase$PercGazeGame,fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$PercGazeMM,fullDatabase$PercGazeOut)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByTier", "Outcome", "PercGazeGame","PercGazeMM","PercGazeout")
plot(percGazeDB, col=fullDatabase$GroupByTier)

percGazeDB<-cbind(lolExpExpDB$GroupByTier,lolExpExpDB$PercGazeGame,lolExpExpDB$Condition, lolExpExpDB$Outcome, lolExpExpDB$PercGazeMM,lolExpExpDB$PercGazeOut)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByTier", "Outcome", "PercGazeGame","PercGazeMM","PercGazeout")
plot(percGazeDB, col=lolExpExpDB$GroupByTier)

percGazeDB<-cbind(lolExpNovDB$GroupByTier,lolExpNovDB$PercGazeGame,lolExpNovDB$Condition, lolExpNovDB$Outcome, lolExpNovDB$PercGazeMM,lolExpNovDB$PercGazeOut)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByTier","Outcome", "PercGazeGame","PercGazeMM","PercGazeout")
plot(percGazeDB, col=lolExpNovDB$GroupByTier)

percGazeDB<-cbind(lolNovicesDB$PercGazeGame,lolNovicesDB$Condition, lolNovicesDB$Outcome, lolNovicesDB$PercGazeMM,lolNovicesDB$PercGazeOut)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("Outcome", "PercGazeGame","PercGazeMM","PercGazeout")
plot(percGazeDB)

percGazeDB<-cbind(dota2DB$GroupByElo,dota2DB$PercGazeGame,dota2DB$Condition, dota2DB$Outcome, dota2DB$PercGazeMM,dota2DB$PercGazeOut)
percGazeDB<-as.data.frame(percGazeDB)
colnames(percGazeDB)<-c("GroupByElo", "Outcome", "PercGazeGame","PercGazeMM","PercGazeout")
plot(percGazeDB, col=dota2DB$GroupByElo)

# Number of fixations by Tier

numberFixationsDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Outcome, fullDatabase$NrFixNOMM,fullDatabase$NrFixationsMM)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByTier", "Outcome", "NrFixationsGame","NrFixationsMM")
plot(numberFixationsDB, col=fullDatabase$GroupByTier)

numberFixationsDB<-cbind(lolExpExpDB$GroupByTier,lolExpExpDB$Outcome, lolExpExpDB$NrFixNOMM,lolExpExpDB$NrFixationsMM)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByTier", "Outcome", "NrFixationsGame","NrFixationsMM")
plot(numberFixationsDB, col=lolExpExpDB$GroupByTier)

numberFixationsDB<-cbind(lolExpNovDB$GroupByTier, lolExpNovDB$Outcome, lolExpNovDB$NrFixNOMM,lolExpNovDB$NrFixationsMM)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByTier", "Outcome", "TotalFixationsGame", "NrFixationsMM")
plot(numberFixationsDB, col=lolExpNovDB$GroupByTier)

numberFixationsDB<-cbind(lolNovicesDB$Outcome, lolNovicesDB$NrFixNOMM,lolNovicesDB$NrFixationsMM)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c( "Outcome", "TotalFixationsGame", "NrFixationsMM")
plot(numberFixationsDB)

numberFixationsDB<-cbind(dota2DB$GroupByElo, dota2DB$Outcome, dota2DB$NrFixNOMM,dota2DB$NrFixationsMM)
numberFixationsDB<-as.data.frame(numberFixationsDB)
colnames(numberFixationsDB)<-c("GroupByElo", "Outcome", "TotalFixationsGame", "NrFixationsMM")
plot(numberFixationsDB, col=dota2DB$GroupByElo)

# Number of Saccades by Tier

numberSaccadesDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$SaccadesMap,fullDatabase$SaccadesMM)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByTier", "Condition", "Outcome", "SaccadesGame","SaccadesMM")
plot(numberSaccadesDB, col=fullDatabase$GroupByTier)

numberSaccadesDB<-cbind(lolExpExpDB$GroupByTier, lolExpExpDB$Outcome, lolExpExpDB$SaccadesMap,lolExpExpDB$SaccadesMM)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByTier", "Outcome", "SaccadesGame","SaccadesMM")
plot(numberSaccadesDB, col=lolExpExpDB$GroupByTier)

numberSaccadesDB<-cbind(lolExpNovDB$GroupByTier,lolExpNovDB$Outcome, lolExpNovDB$SaccadesMap,lolExpNovDB$SaccadesMM)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByTier", "Outcome", "SaccadesGame","SaccadesMM")
plot(numberSaccadesDB, col=lolExpNovDB$GroupByTier)

numberSaccadesDB<-cbind(lolNovicesDB$Outcome, lolNovicesDB$SaccadesMap,lolNovicesDB$SaccadesMM)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("Outcome", "SaccadesGame","SaccadesMM")
plot(numberSaccadesDB)

numberSaccadesDB<-cbind(dota2DB$GroupByElo, dota2DB$Outcome, dota2DB$SaccadesMap,dota2DB$SaccadesMM)
numberSaccadesDB<-as.data.frame(numberSaccadesDB)
colnames(numberSaccadesDB)<-c("GroupByElo","Outcome", "SaccadesGame","SaccadesMM")
plot(numberSaccadesDB, col=dota2DB$GroupByElo)

# Relation saccade-fixation by Tier

SacFixRatioDB<-cbind(fullDatabase$GroupByTier, fullDatabase$Outcome, fullDatabase$SacFixRatioMap, fullDatabase$SacFixRatioMM)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByTier", "Outcome", "sac-fix ratio game","sac-fix ratio mimimap")
plot(SacFixRatioDB, col=fullDatabase$GroupByTier)

SacFixRatioDB<-cbind(lolExpExpDB$GroupByTier, lolExpExpDB$Outcome, lolExpExpDB$SacFixRatioMap, lolExpExpDB$SacFixRatioMM)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByTier", "Outcome", "sac-fix ratio game","sac-fix ratio mimimap")
plot(SacFixRatioDB, col=lolExpExpDB$GroupByTier)

SacFixRatioDB<-cbind(lolExpNovDB$GroupByTier, lolExpNovDB$Outcome, lolExpNovDB$SacFixRatioMap, lolExpNovDB$SacFixRatioMM)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByTier", "Outcome", "sac-fix ratio game","sac-fix ratio mimimap")
plot(SacFixRatioDB, col=lolExpNovDB$GroupByTier)

SacFixRatioDB<-cbind(lolNovicesDB$Outcome, lolNovicesDB$SacFixRatioMap, lolNovicesDB$SacFixRatioMM)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("Outcome", "sac-fixratioGame","sac-fixratioMM")
plot(SacFixRatioDB)

SacFixRatioDB<-cbind(dota2DB$GroupByElo,dota2DB$Condition, dota2DB$Outcome, dota2DB$SacFixRatioMap, dota2DB$SacFixRatioMM)
SacFixRatioDB<-as.data.frame(SacFixRatioDB)
colnames(SacFixRatioDB)<-c("GroupByElo", "Outcome", "sac-fixratioGame","sac-fixratioMM")
plot(SacFixRatioDB, col=dota2DB$GroupByElo)

# Relation fixation-time by Tier

ratioSacTimeFixTime<-cbind(fullDatabase$GroupByTier, fullDatabase$Outcome, fullDatabase$ratioFixTimeGame, fullDatabase$ratioFixTimeMM, fullDatabase$ratioSacTimeGame, fullDatabase$ratioSacTimeMM)
ratioSacTimeFixTime<-as.data.frame(ratioSacTimeFixTime)
colnames(ratioSacTimeFixTime)<-c("GroupByTier", "Outcome","Fix-timeRatioGame","Fix-timeRatioMM", "Sac-timeRatioGame","Sac-timeRatioMM")
plot(ratioSacTimeFixTime, col=fullDatabase$GroupByTier)

ratioSacTimeFixTime<-cbind(lolExpExpDB$GroupByTier, lolExpExpDB$Outcome, lolExpExpDB$ratioFixTimeGame, lolExpExpDB$ratioFixTimeMM, lolExpExpDB$ratioSacTimeGame, lolExpExpDB$ratioSacTimeMM)
ratioSacTimeFixTime<-as.data.frame(ratioSacTimeFixTime)
colnames(ratioSacTimeFixTime)<-c("GroupByTier", "Outcome","Fix-timeRatioGame","Fix-timeRatioMM", "Sac-timeRatioGame","Sac-timeRatioMM")
plot(ratioSacTimeFixTime, col=lolExpExpDB$GroupByTier)

ratioSacTimeFixTime<-cbind(lolExpNovDB$GroupByTier, lolExpNovDB$Outcome, lolExpNovDB$ratioFixTimeGame, lolExpNovDB$ratioFixTimeMM, lolExpNovDB$ratioSacTimeGame, lolExpNovDB$ratioSacTimeMM)
ratioSacTimeFixTime<-as.data.frame(ratioSacTimeFixTime)
colnames(ratioSacTimeFixTime)<-c("GroupByTier", "Outcome","Fix-timeRatioGame","Fix-timeRatioMM", "Sac-timeRatioGame","Sac-timeRatioMM")
plot(ratioSacTimeFixTime, col=lolExpNovDB$GroupByTier)

ratioSacTimeFixTime<-cbind(lolNovicesDB$Outcome, lolNovicesDB$ratioFixTimeGame, lolNovicesDB$ratioFixTimeMM, lolNovicesDB$ratioSacTimeGame, lolNovicesDB$ratioSacTimeMM)
ratioSacTimeFixTime<-as.data.frame(ratioSacTimeFixTime)
colnames(ratioSacTimeFixTime)<-c("Outcome","Fix-timeRatioGame","Fix-timeRatioMM", "Sac-timeRatioGame","Sac-timeRatioMM")
plot(ratioSacTimeFixTime)

ratioSacTimeFixTime<-cbind(dota2DB$GroupByElo, dota2DB$Outcome, dota2DB$ratioFixTimeGame, dota2DB$ratioFixTimeMM, dota2DB$ratioSacTimeGame, dota2DB$ratioSacTimeMM)
ratioSacTimeFixTime<-as.data.frame(ratioSacTimeFixTime)
colnames(ratioSacTimeFixTime)<-c("GroupByElo", "Outcome","Fix-timeRatioGame","Fix-timeRatioMM", "Sac-timeRatioGame","Sac-timeRatioMM")
plot(ratioSacTimeFixTime, col=dota2DB$GroupByElo)

##### Full DB plots and distributions #####

# Depiction of the outcomes in a pie chart
outcomeCount<-table(fullDatabase$Outcome)
outcomePerc<-round(100*outcomeCount/sum(outcomeCount),2)
pie(outcomePerc, edges=500, radius=1, main="Outcome of the games", labels=paste(outcomePerc,"%"), col=heat.colors(length(outcomeCount)))
legend("topright", c("lost","win"), cex=1, fill=heat.colors(length(outcomeCount)))

# Depiction of the outcomes by game in a pie chart
outcomeCount<-table(lolExpExpDB$Outcome)
outcomePerc<-round(100*outcomeCount/sum(outcomeCount),2)
pie(outcomePerc, edges=500, radius=1, main="Outcome of experts-experts", labels=paste(outcomePerc,"%"), col=heat.colors(length(outcomeCount)))
legend("topright", c("lost","win"), cex=1, fill=heat.colors(length(outcomeCount)))

# Depiction of the outcomes by game in a pie chart
outcomeCount<-table(lolExpNovDB$Outcome)
outcomePerc<-round(100*outcomeCount/sum(outcomeCount),2)
pie(outcomePerc, edges=500, radius=1, main="Outcome of experts-novices", labels=paste(outcomePerc,"%"), col=heat.colors(length(outcomeCount)))
legend("topright", c("lost","win"), cex=1, fill=heat.colors(length(outcomeCount)))

# Depiction of the outcomes by game in a pie chart
outcomeCount<-table(lolNovicesDB$Outcome)
outcomePerc<-round(100*outcomeCount/sum(outcomeCount),2)
pie(outcomePerc, edges=500, radius=1, main="Outcome of Novices", labels=paste(outcomePerc,"%"), col=heat.colors(length(outcomeCount)))
legend("topright", c("lost","win"), cex=1, fill=heat.colors(length(outcomeCount)))

# Depiction of the outcomes by game in a pie chart
outcomeCount<-table(dota2DB$Outcome)
outcomePerc<-round(100*outcomeCount/sum(outcomeCount),2)
pie(outcomePerc, edges=500, radius=1, main="Outcome of DOTA2", labels=paste(outcomePerc,"%"), col=heat.colors(length(outcomeCount)))
legend("topright", c("lost","win"), cex=1, fill=heat.colors(length(outcomeCount)))

# Distribution of of sac/fix ratio in the game for LoL experts under Expert condition
distribution_hist(lolExpExpDB$SacFixRatioMap,xlab=" Expert-Expert Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of of sac/fix ratio in the mini-map for LoL experts under Expert condition
distribution_hist(lolExpExpDB$SacFixRatioMM,xlab="Expert-Expert Saccade-fixation ratio in minimap",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the game for LoL experts under novice condition
distribution_hist(lolExpNovDB$SacFixRatioMap,xlab="Expert-novice Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the mini-map for LoL experts under novice condition
distribution_hist(lolExpNovDB$SacFixRatioMM,xlab="Expert-novice Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the game for LoL novices
distribution_hist(lolNovicesDB$SacFixRatioMap,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the mini-map for LoL novices
distribution_hist(lolNovicesDB$SacFixRatioMM,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the game for DOTA2 experts
distribution_hist(dota2DB$SacFixRatioMap,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/fix ratio in the mini-map for DOTA2 experts
distribution_hist(dota2DB$SacFixRatioMM,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the game for LoL experts under the expert condition
distribution_hist(lolExpExpDB$ratioFixTimeGame,xlab="Expert-expert Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the mini-map for LoL experts under the expert condition
distribution_hist(lolExpExpDB$ratioFixTimeMM,xlab="Expert-expert Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the game for LoL experts under the novice condition
distribution_hist(lolExpNovDB$ratioFixTimeGame,xlab="Expert-novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the mini-map for LoL experts under the novice condition
distribution_hist(lolExpNovDB$ratioFixTimeMM,xlab="Expert-novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the game for LoL novices
distribution_hist(lolNovicesDB$ratioFixTimeGame,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the mini-map for LoL novices 
distribution_hist(lolNovicesDB$ratioFixTimeMM,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the game for DOTA2 experts
distribution_hist(dota2DB$ratioFixTimeGame,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of fix/time ratio in the mini-map for DOTA2 experts
distribution_hist(dota2DB$ratioFixTimeMM,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the game for LoL experts under the expert condition
distribution_hist(lolExpExpDB$ratioSacTimeGame,xlab="Expert-expert Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the mini-map for LoL experts under the expert condition
distribution_hist(lolExpExpDB$ratioSacTimeMM,xlab="Expert-expert Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the game for LoL experts under the novice condition
distribution_hist(lolExpNovDB$ratioSacTimeGame,xlab="Expert-novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the mini-map for LoL experts under the novice condition
distribution_hist(lolExpNovDB$ratioSacTimeMM,xlab="Expert-novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the game for LoL novices
distribution_hist(lolNovicesDB$ratioSacTimeGame,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the mini-map for LoL novices 
distribution_hist(lolNovicesDB$ratioSacTimeMM,xlab="Novices Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the game for DOTA2 experts
distribution_hist(dota2DB$ratioSacTimeGame,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

# Distribution of sac/time ratio in the mini-map for DOTA2 experts
distribution_hist(dota2DB$ratioSacTimeMM,xlab="DOTA2 Saccade-fixation ratio in game",ylab="Frequency of occurence")

##### Relational plots #####

#pie(summary(fullDatabase$Expertise)~summary(fullDatabase$Condition),edges=500, radius=1, main="Participants expertise", col=heat.colors(6,alpha = 1))

#plot(fullDatabase$Game~fullDatabase$Expertise,type = "", main="game vs expertise", xlab="game", ylab="expertise")

#plot(fullDatabase$Expertise~fullDatabase$Outcome,type = "", main="expertise vs outcome", xlab="expertise", ylab="outcome")

##### Spliting databases for LoL Expertise groups #####

group_A<-subset(lolExpertsDB, GroupByTier=="A")
group_B<-subset(lolExpertsDB, GroupByTier=="B")
group_C<-subset(lolExpertsDB, GroupByTier=="C")
group_D<-subset(lolExpertsDB, GroupByTier=="D")
group_E<-subset(lolExpertsDB, GroupByTier=="E")
group_F<-subset(lolExpertsDB, GroupByTier=="F")
group_G<-subset(lolExpertsDB, GroupByTier=="G")

group_silver<-subset(lolExpertsDB, Tier=="silver")
group_gold<-subset(lolExpertsDB, Tier=="gold")
group_diamond<-subset(lolExpertsDB, Tier=="diamond")

##### Spliting databases for DOTA Expertise groups #####

group_A<-subset(dota2DB, GroupByElo=="A")
group_B<-subset(dota2DB, GroupByElo=="B")
group_C<-subset(dota2DB, GroupByElo=="C")
group_D<-subset(dota2DB, GroupByElo=="D")
group_E<-subset(dota2DB, GroupByElo=="E")

##### Spliting databases for LoL Expertise groups #####

expert_tutorialDB<-subset(lolNovicesDB, Condition=="tutorial")
expert_noviceDB<-subset(lolNovicesDB, Condition=="novice")
expert_ev1DB<-subset(lolNovicesDB, Condition=="evaluation1")
expert_ev2DB<-subset(lolNovicesDB, Condition=="evaluation2")

##### Average (statistical mean) of fixations and saccades per game #####

meanSaccadesMap<-numberSaccadesDB$SaccadesGame/gameTimeTierDB$SurfaceVisGameSecs
meanSaccadesMM<-numberSaccadesDB$SaccadesMM/gameTimeTierDB$SurfaceVisMMSecs

meanSacsFixTier<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$MeanFixationsGame, meanSaccadesMap, fullDatabase$MeanFixationsMM, meanSaccadesMM)
meanSacsFixTier<-as.data.frame(meanSacsFixTier)

colnames(meanSacsFixTier)<-c("GroupByTier","Condition", "Outcome", "MeanFixMap","MeanSacMap", "MeanFixMM", "MeanSacMM")

plot(meanSacsFixTier, col=fullDatabase$GroupByTier)

with(meanSacsFixTier, plot(MeanFixMM,MeanSacMM, col = GroupByTier)) #Makes a plot with colour based on the Tier
abline(h = 12, lwd = 2, lty = 2)

#### Mean of fixation duration#####

meanFixTime<-cbind(fullDatabase$GroupByTier, fullDatabase$Condition, fullDatabase$Outcome, fullDatabase$MeanDurationGame, fullDatabase$MeanDurationMM)
meanFixTime<-as.data.frame(meanFixTime)

colnames(meanFixTime)<-c("GroupByTier","Condition", "Outcome", "MeanFixDurationMap","MeanFixDurationMM")

plot(meanFixTime, col=fullDatabase$GroupByTier)



##### Print plots##### 

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



########
