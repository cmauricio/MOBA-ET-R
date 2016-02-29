# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("car")
library("lsr")


# Process

lolexpexp<-read.table("160223lolExpNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))

dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))



# Analysis for LoL Experts between expertise groups

# Analysis for MeanDurationFix Game


lolexp<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixGame"))
colnames(lolexp)<-c("Group","MeanDurationFixGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixGame"))
colnames(dotaex)<-c("Group","MeanDurationFixGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$MeanDurationFixGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$MeanDurationFixGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$MeanDurationFixGame~lolVSdota$Group)



# Analysis MeanDurationFixMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixMM"))
colnames(lolexp)<-c("Group","MeanDurationFixMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixMM"))
colnames(dotaex)<-c("Group","MeanDurationFixMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$MeanDurationFixMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$MeanDurationFixMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$MeanDurationFixMM~lolVSdota$Group)


# Analysis SacFixRatioMap

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacFixRatioMap"))
colnames(lolexp)<-c("Group","SacFixRatioMap")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacFixRatioMap"))
colnames(dotaex)<-c("Group","SacFixRatioMap")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacFixRatioMap~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$SacFixRatioMap,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacFixRatioMap~lolVSdota$Group)



# Analysis SacFixRatioMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacFixRatioMM"))
colnames(lolexp)<-c("Group","SacFixRatioMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacFixRatioMM"))
colnames(dotaex)<-c("Group","SacFixRatioMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacFixRatioMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$SacFixRatioMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacFixRatioMM~lolVSdota$Group)



# Analysis FixSecGame

lolexp<-subset(lolexpexp, select=c("GroupByTier","FixSecGame"))
colnames(lolexp)<-c("Group","FixSecGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecGame"))
colnames(dotaex)<-c("Group","FixSecGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$FixSecGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$FixSecGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$FixSecGame~lolVSdota$Group)


# Analysis FixSecMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","FixSecMM"))
colnames(lolexp)<-c("Group","FixSecMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecMM"))
colnames(dotaex)<-c("Group","FixSecMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$FixSecMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$FixSecMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$FixSecMM~lolVSdota$Group)

# Analysis SacSecGame

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacSecGame"))
colnames(lolexp)<-c("Group","SacSecGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecGame"))
colnames(dotaex)<-c("Group","SacSecGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacSecGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$SacSecGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacSecGame~lolVSdota$Group)



# Analysis SacSecMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacSecMM"))
colnames(lolexp)<-c("Group","SacSecMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecMM"))
colnames(dotaex)<-c("Group","SacSecMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacSecMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$SacSecMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacSecMM~lolVSdota$Group)




# Analysis NrFixRatio

lolexp<-subset(lolexpexp, select=c("GroupByTier","NrFixRatio"))
colnames(lolexp)<-c("Group","NrFixRatio")

dotaex<-subset(dotaexp, select=c("GroupByElo","NrFixRatio"))
colnames(dotaex)<-c("Group","NrFixRatio")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$NrFixRatio~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$NrFixRatio,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$NrFixRatio~lolVSdota$Group)



# Analysis NrSacRatio

lolexp<-subset(lolexpexp, select=c("GroupByTier","NrSacRatio"))
colnames(lolexp)<-c("Group","NrSacRatio")

dotaex<-subset(dotaexp, select=c("GroupByElo","NrSacRatio"))
colnames(dotaex)<-c("Group","NrSacRatio")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$NrSacRatio~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$NrSacRatio,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$NrSacRatio~lolVSdota$Group)



# Analysis PercFixGame

lolexp<-subset(lolexpexp, select=c("GroupByTier","PercFixGame"))
colnames(lolexp)<-c("Group","PercFixGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","PercFixGame"))
colnames(dotaex)<-c("Group","PercFixGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$PercFixGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$PercFixGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$PercFixGame~lolVSdota$Group)


# Analysis PercFixMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","PercFixMM"))
colnames(lolexp)<-c("Group","PercFixMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","PercFixMM"))
colnames(dotaex)<-c("Group","PercFixMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$PercFixMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$PercFixMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$PercFixMM~lolVSdota$Group)


# Analysis PercSacGame

lolexp<-subset(lolexpexp, select=c("GroupByTier","PercSacGame"))
colnames(lolexp)<-c("Group","PercSacGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","PercSacGame"))
colnames(dotaex)<-c("Group","PercSacGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$PercSacGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$PercSacGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$PercSacGame~lolVSdota$Group)


# Analysis PercSacMM

lolexp<-subset(lolexpexp, select=c("GroupByTier","PercSacMM"))
colnames(lolexp)<-c("Group","PercSacMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","PercSacMM"))
colnames(dotaex)<-c("Group","PercSacMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$PercSacMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

leveneTest(lolVSdota$PercSacMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$PercSacMM~lolVSdota$Group)
