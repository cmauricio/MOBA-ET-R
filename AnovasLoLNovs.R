# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("car")
library("lsr")


# Process

lolnov<-read.table("160223lolNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolnovnov<-subset(lolnov,Condition=="novice"|Condition=="evaluation1"|Condition=="evaluation2")

lolnovnov$Condition<-factor(lolnovnov[,"Condition"], ordered = T, levels = c("novice","evaluation1","evaluation2"))



# Analysis for LoL Experts between expertise groups

# Analysis for MeanDurationFix Game

lolnov1<-subset(lolnovnov, select=c("Condition","MeanDurationFixGame"))

lolnovaov<-aov(lolnov1$MeanDurationFixGame~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$MeanDurationFixGame~lolnov1$Condition)




# Analysis MeanDurationFixMM

lolnov1<-subset(lolnovnov, select=c("Condition","MeanDurationFixMM"))

lolnovaov<-aov(lolnov1$MeanDurationFixMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$MeanDurationFixMM~lolnov1$Condition)


# Analysis SacFixRatioMap

lolnov1<-subset(lolnovnov, select=c("Condition","SacFixRatioMap"))

lolnovaov<-aov(lolnov1$SacFixRatioMap~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$SacFixRatioMap~lolnov1$Condition)




# Analysis SacFixRatioMM

lolnov1<-subset(lolnovnov, select=c("Condition","SacFixRatioMM"))

lolnovaov<-aov(lolnov1$SacFixRatioMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$SacFixRatioMM~lolnov1$Condition)




# Analysis FixSecGame

lolnov1<-subset(lolnovnov, select=c("Condition","FixSecGame"))

lolnovaov<-aov(lolnov1$FixSecGame~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$FixSecGame~lolnov1$Condition)




# Analysis FixSecMM

lolnov1<-subset(lolnovnov, select=c("Condition","FixSecMM"))

lolnovaov<-aov(lolnov1$FixSecMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$FixSecMM~lolnov1$Condition)




# Analysis SacSecGame

lolnov1<-subset(lolnovnov, select=c("Condition","SacSecGame"))

lolnovaov<-aov(lolnov1$SacSecGame~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$SacSecGame~lolnov1$Condition)




# Analysis SacSecMM

lolnov1<-subset(lolnovnov, select=c("Condition","SacSecMM"))

lolnovaov<-aov(lolnov1$SacSecMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$SacSecMM~lolnov1$Condition)




# Analysis NrFixRatio

lolnov1<-subset(lolnovnov, select=c("Condition","NrFixRatio"))

lolnovaov<-aov(lolnov1$NrFixRatio~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$NrFixRatio~lolnov1$Condition)



# Analysis NrSacRatio

lolnov1<-subset(lolnovnov, select=c("Condition","NrSacRatio"))

lolnovaov<-aov(lolnov1$NrSacRatio~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$NrSacRatio~lolnov1$Condition)




# Analysis PercFixGame

lolnov1<-subset(lolnovnov, select=c("Condition","PercFixGame"))

lolnovaov<-aov(lolnov1$PercFixGame~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$PercFixGame~lolnov1$Condition)




# Analysis PercFixMM

lolnov1<-subset(lolnovnov, select=c("Condition","PercFixMM"))

lolnovaov<-aov(lolnov1$PercFixMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$PercFixMM~lolnov1$Condition)




# Analysis PercSacGame

lolnov1<-subset(lolnovnov, select=c("Condition","PercSacGame"))

lolnovaov<-aov(lolnov1$PercSacGame~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$PercSacGame~lolnov1$Condition)




# Analysis PercSacMM

lolnov1<-subset(lolnovnov, select=c("Condition","PercSacMM"))

lolnovaov<-aov(lolnov1$PercSacMM~lolnov1$Condition)
summary(lolnovaov)

lolnoveta<-etaSquared(lolnovaov)

TukeyHSD(lolnovaov)

boxplot(lolnov1$PercSacMM~lolnov1$Condition)

