# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("car")
library("lsr")


# Process

lolexpnov<-read.table("160223lolExpNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
lolnov<-read.table("160223lolNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolnov<-subset(lolnov,Condition=="novice")

lolexpnov$Tier<-factor(lolexpnov[,"Tier"], order=T, levels=c("novice","bronce", "silver", "gold", "platinum", "diamond"))

lolnov$Tier<-"novice"
lolnov$Tier<-factor(lolnov[,"Tier"], order=T, levels=c("novice","bronce", "silver", "gold", "platinum", "diamond"))

lolexpnov$GroupByTier<-factor(lolexpnov[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"))

lolnov$GroupByTier<-"I"
lolnov$GroupByTier<-factor(lolnov[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H","I"))




# Analysis for LoL Experts between expertise groups

# Analysis for MeanDurationFix Game

lolexpnov1<-subset(lolexpnov, select=c("Tier","MeanDurationFixGame"))
lolexpnov2<-subset(lolnov, select=c("Tier","MeanDurationFixGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$MeanDurationFixGame~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$MeanDurationFixGame,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$MeanDurationFixGame~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","MeanDurationFixGame"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","MeanDurationFixGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$MeanDurationFixGame~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$MeanDurationFixGame,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$MeanDurationFixGame~lolexpVSnov$GroupByTier)



# Analysis MeanDurationFixMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","MeanDurationFixMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","MeanDurationFixMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$MeanDurationFixMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$MeanDurationFixMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$MeanDurationFixMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","MeanDurationFixMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","MeanDurationFixMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$MeanDurationFixMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$MeanDurationFixMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$MeanDurationFixMM~lolexpVSnov$GroupByTier)



# Analysis SacFixRatioMap

lolexpnov1<-subset(lolexpnov, select=c("Tier","SacFixRatioMap"))
lolexpnov2<-subset(lolnov, select=c("Tier","SacFixRatioMap"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacFixRatioMap~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacFixRatioMap,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacFixRatioMap~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","SacFixRatioMap"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","SacFixRatioMap"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacFixRatioMap~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacFixRatioMap,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacFixRatioMap~lolexpVSnov$GroupByTier)




# Analysis SacFixRatioMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","SacFixRatioMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","SacFixRatioMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacFixRatioMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacFixRatioMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacFixRatioMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","SacFixRatioMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","SacFixRatioMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacFixRatioMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacFixRatioMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacFixRatioMM~lolexpVSnov$GroupByTier)




# Analysis FixSecGame

lolexpnov1<-subset(lolexpnov, select=c("Tier","FixSecGame"))
lolexpnov2<-subset(lolnov, select=c("Tier","FixSecGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$FixSecGame~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$FixSecGame,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$FixSecGame~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","FixSecGame"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","FixSecGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$FixSecGame~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$FixSecGame,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$FixSecGame~lolexpVSnov$GroupByTier)




# Analysis FixSecMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","FixSecMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","FixSecMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$FixSecMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$FixSecMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$FixSecMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","FixSecMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","FixSecMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$FixSecMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$FixSecMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$FixSecMM~lolexpVSnov$GroupByTier)




# Analysis SacSecGame

lolexpnov1<-subset(lolexpnov, select=c("Tier","SacSecGame"))
lolexpnov2<-subset(lolnov, select=c("Tier","SacSecGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacSecGame~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacSecGame,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacSecGame~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","SacSecGame"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","SacSecGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacSecGame~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacSecGame,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacSecGame~lolexpVSnov$GroupByTier)




# Analysis SacSecMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","SacSecMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","SacSecMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacSecMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacSecMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacSecMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","SacSecMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","SacSecMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$SacSecMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$SacSecMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$SacSecMM~lolexpVSnov$GroupByTier)




# Analysis NrFixRatio

lolexpnov1<-subset(lolexpnov, select=c("Tier","NrFixRatio"))
lolexpnov2<-subset(lolnov, select=c("Tier","NrFixRatio"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$NrFixRatio~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$NrFixRatio,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$NrFixRatio~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","NrFixRatio"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","NrFixRatio"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$NrFixRatio~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$NrFixRatio,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$NrFixRatio~lolexpVSnov$GroupByTier)



# Analysis NrSacRatio

lolexpnov1<-subset(lolexpnov, select=c("Tier","NrSacRatio"))
lolexpnov2<-subset(lolnov, select=c("Tier","NrSacRatio"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$NrSacRatio~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$NrSacRatio,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$NrSacRatio~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","NrSacRatio"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","NrSacRatio"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$NrSacRatio~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$NrSacRatio,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$NrSacRatio~lolexpVSnov$GroupByTier)




# Analysis PercFixGame

lolexpnov1<-subset(lolexpnov, select=c("Tier","PercFixGame"))
lolexpnov2<-subset(lolnov, select=c("Tier","PercFixGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercFixGame~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercFixGame,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercFixGame~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","PercFixGame"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","PercFixGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercFixGame~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercFixGame,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercFixGame~lolexpVSnov$GroupByTier)




# Analysis PercFixMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","PercFixMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","PercFixMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercFixMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercFixMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercFixMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","PercFixMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","PercFixMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercFixMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercFixMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercFixMM~lolexpVSnov$GroupByTier)




# Analysis PercSacGame

lolexpnov1<-subset(lolexpnov, select=c("Tier","PercSacGame"))
lolexpnov2<-subset(lolnov, select=c("Tier","PercSacGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercSacGame~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercSacGame,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercSacGame~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","PercSacGame"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","PercSacGame"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercSacGame~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercSacGame,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercSacGame~lolexpVSnov$GroupByTier)




# Analysis PercSacMM

lolexpnov1<-subset(lolexpnov, select=c("Tier","PercSacMM"))
lolexpnov2<-subset(lolnov, select=c("Tier","PercSacMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercSacMM~lolexpVSnov$Tier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercSacMM,lolexpVSnov$Tier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercSacMM~lolexpVSnov$Tier)




lolexpnov1<-subset(lolexpnov, select=c("GroupByTier","PercSacMM"))
lolexpnov2<-subset(lolnov, select=c("GroupByTier","PercSacMM"))
lolexpVSnov<-rbind(lolexpnov1,lolexpnov2)

lolexpnovaov<-aov(lolexpVSnov$PercSacMM~lolexpVSnov$GroupByTier)
summary(lolexpnovaov)

lolexexeta<-etaSquared(lolexpnovaov)

leveneTest(lolexpVSnov$PercSacMM,lolexpVSnov$GroupByTier,center=mean)

TukeyHSD(lolexpnovaov)

boxplot(lolexpVSnov$PercSacMM~lolexpVSnov$GroupByTier)
