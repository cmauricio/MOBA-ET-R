# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("car")
library("lsr")


# Process

lolexpexp<-read.table("160223lolExpExpCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolexpexp$Tier<-factor(lolexpexp[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))

lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))

# Analysis for LoL Experts between expertise groups

# Analysis for MeanDurationFix Game

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","MeanDurationFixGame"))

lolexexaov<-aov(lolexexTierMDFG$MeanDurationFixGame~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$MeanDurationFixGame,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$MeanDurationFixGame~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixGame"))

lolexex2aov<-aov(lolexexGroupMDFG$MeanDurationFixGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$MeanDurationFixGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$MeanDurationFixGame~lolexexGroupMDFG$GroupByTier)



# Analysis MeanDurationFixMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","MeanDurationFixMM"))

lolexexaov<-aov(lolexexTierMDFG$MeanDurationFixMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$MeanDurationFixMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$MeanDurationFixMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixMM"))

lolexex2aov<-aov(lolexexGroupMDFG$MeanDurationFixMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$MeanDurationFixMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$MeanDurationFixMM~lolexexGroupMDFG$GroupByTier)



# Analysis SacFixRatioMap
 
lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","SacFixRatioMap"))

lolexexaov<-aov(lolexexTierMDFG$SacFixRatioMap~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$SacFixRatioMap,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$SacFixRatioMap~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacFixRatioMap"))

lolexex2aov<-aov(lolexexGroupMDFG$SacFixRatioMap~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacFixRatioMap,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacFixRatioMap~lolexexGroupMDFG$GroupByTier)
 



# Analysis SacFixRatioMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","SacFixRatioMM"))

lolexexaov<-aov(lolexexTierMDFG$SacFixRatioMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$SacFixRatioMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$SacFixRatioMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacFixRatioMM"))

lolexex2aov<-aov(lolexexGroupMDFG$SacFixRatioMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacFixRatioMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacFixRatioMM~lolexexGroupMDFG$GroupByTier)




# Analysis FixSecGame

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","FixSecGame"))

lolexexaov<-aov(lolexexTierMDFG$FixSecGame~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$FixSecGame,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$FixSecGame~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","FixSecGame"))

lolexex2aov<-aov(lolexexGroupMDFG$FixSecGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$FixSecGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$FixSecGame~lolexexGroupMDFG$GroupByTier)




# Analysis FixSecMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","FixSecMM"))

lolexexaov<-aov(lolexexTierMDFG$FixSecMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$FixSecMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$FixSecMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","FixSecMM"))

lolexex2aov<-aov(lolexexGroupMDFG$FixSecMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$FixSecMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$FixSecMM~lolexexGroupMDFG$GroupByTier)




# Analysis SacSecGame

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","SacSecGame"))

lolexexaov<-aov(lolexexTierMDFG$SacSecGame~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$SacSecGame,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$SacSecGame~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacSecGame"))

lolexex2aov<-aov(lolexexGroupMDFG$SacSecGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacSecGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacSecGame~lolexexGroupMDFG$GroupByTier)




# Analysis SacSecMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","SacSecMM"))

lolexexaov<-aov(lolexexTierMDFG$SacSecMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$SacSecMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$SacSecMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacSecMM"))

lolexex2aov<-aov(lolexexGroupMDFG$SacSecMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacSecMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacSecMM~lolexexGroupMDFG$GroupByTier)




# Analysis NrFixRatio

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","NrFixRatio"))

lolexexaov<-aov(lolexexTierMDFG$NrFixRatio~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$NrFixRatio,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$NrFixRatio~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","NrFixRatio"))

lolexex2aov<-aov(lolexexGroupMDFG$NrFixRatio~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$NrFixRatio,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$NrFixRatio~lolexexGroupMDFG$GroupByTier)



# Analysis NrSacRatio

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","NrSacRatio"))

lolexexaov<-aov(lolexexTierMDFG$NrSacRatio~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$NrSacRatio,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$NrSacRatio~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","NrSacRatio"))

lolexex2aov<-aov(lolexexGroupMDFG$NrSacRatio~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$NrSacRatio,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$NrSacRatio~lolexexGroupMDFG$GroupByTier)




# Analysis PercFixGame

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","PercFixGame"))

lolexexaov<-aov(lolexexTierMDFG$PercFixGame~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$PercFixGame,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$PercFixGame~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","PercFixGame"))

lolexex2aov<-aov(lolexexGroupMDFG$PercFixGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$PercFixGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$PercFixGame~lolexexGroupMDFG$GroupByTier)




# Analysis PercFixMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","PercFixMM"))

lolexexaov<-aov(lolexexTierMDFG$PercFixMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$PercFixMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$PercFixMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","PercFixMM"))

lolexex2aov<-aov(lolexexGroupMDFG$PercFixMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$PercFixMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$PercFixMM~lolexexGroupMDFG$GroupByTier)




# Analysis PercSacGame

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","PercSacGame"))

lolexexaov<-aov(lolexexTierMDFG$PercSacGame~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$PercSacGame,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$PercSacGame~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","PercSacGame"))

lolexex2aov<-aov(lolexexGroupMDFG$PercSacGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$PercSacGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$PercSacGame~lolexexGroupMDFG$GroupByTier)




# Analysis PercSacMM

lolexexTierMDFG<-subset(lolexpexp, select=c("Tier","PercSacMM"))

lolexexaov<-aov(lolexexTierMDFG$PercSacMM~lolexexTierMDFG$Tier)
summary(lolexexaov)

lolexexeta<-etaSquared(lolexexaov)

leveneTest(lolexexTierMDFG$PercSacMM,lolexexTierMDFG$Tier,center=mean)

TukeyHSD(lolexexaov)

boxplot(lolexexTierMDFG$PercSacMM~lolexexTierMDFG$Tier)




lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","PercSacMM"))

lolexex2aov<-aov(lolexexGroupMDFG$PercSacMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$PercSacMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$PercSacMM~lolexexGroupMDFG$GroupByTier)
