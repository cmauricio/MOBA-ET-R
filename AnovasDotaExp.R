# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("car")
library("lsr")


# Process

dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))



# Analysis for LoL Experts between expertise groups

# Analysis for MeanDurationFix Game

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixGame"))

loldotaaov<-aov(dotaex$MeanDurationFixGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$MeanDurationFixGame,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixGame~dotaex$GroupByElo)



# Analysis MeanDurationFixMM

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixMM"))

loldotaaov<-aov(dotaex$MeanDurationFixMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$MeanDurationFixMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixMM~dotaex$GroupByElo)


# Analysis SacFixRatioMap

dotaex<-subset(dotaexp, select=c("GroupByElo","SacFixRatioMap"))

loldotaaov<-aov(dotaex$SacFixRatioMap~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$SacFixRatioMap,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacFixRatioMap~dotaex$GroupByElo)



# Analysis SacFixRatioMM

dotaex<-subset(dotaexp, select=c("GroupByElo","SacFixRatioMM"))

loldotaaov<-aov(dotaex$SacFixRatioMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$SacFixRatioMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacFixRatioMM~dotaex$GroupByElo)



# Analysis FixSecGame

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecGame"))

loldotaaov<-aov(dotaex$FixSecGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$FixSecGame,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecGame~dotaex$GroupByElo)



# Analysis FixSecMM

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecMM"))

loldotaaov<-aov(dotaex$FixSecMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$FixSecMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecMM~dotaex$GroupByElo)



# Analysis SacSecGame

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecGame"))

loldotaaov<-aov(dotaex$SacSecGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$SacSecGame,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecGame~dotaex$GroupByElo)



# Analysis SacSecMM

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecMM"))

loldotaaov<-aov(dotaex$SacSecMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$SacSecMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecMM~dotaex$GroupByElo)




# Analysis NrFixRatio

dotaex<-subset(dotaexp, select=c("GroupByElo","NrFixRatio"))

loldotaaov<-aov(dotaex$NrFixRatio~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$NrFixRatio,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$NrFixRatio~dotaex$GroupByElo)



# Analysis NrSacRatio

dotaex<-subset(dotaexp, select=c("GroupByElo","NrSacRatio"))

loldotaaov<-aov(dotaex$NrSacRatio~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$NrSacRatio,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$NrSacRatio~dotaex$GroupByElo)



# Analysis PercFixGame

dotaex<-subset(dotaexp, select=c("GroupByElo","PercFixGame"))

loldotaaov<-aov(dotaex$PercFixGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$PercFixGame,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$PercFixGame~dotaex$GroupByElo)


# Analysis PercFixMM

dotaex<-subset(dotaexp, select=c("GroupByElo","PercFixMM"))

loldotaaov<-aov(dotaex$PercFixMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$PercFixMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$PercFixMM~dotaex$GroupByElo)


# Analysis PercSacGame

dotaex<-subset(dotaexp, select=c("GroupByElo","PercSacGame"))

loldotaaov<-aov(dotaex$PercSacGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$PercSacGame,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$PercSacGame~dotaex$GroupByElo)


# Analysis PercSacMM

dotaex<-subset(dotaexp, select=c("GroupByElo","PercSacMM"))

loldotaaov<-aov(dotaex$PercSacMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(dotaex$PercSacMM,dotaex$GroupByElo,center=mean)

TukeyHSD(loldotaaov)

boxplot(dotaex$PercSacMM~dotaex$GroupByElo)
