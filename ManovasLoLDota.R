# MANOVA analyses for LoL expertise groups, including post-hoc ANOVAs
# for detecting sources of difference. The standard analysis of variance
# uses Bonferroni correction

# etaSquared function based on the F value, degrees of freedom, and number of Observations

etasq<-function (f,k,N) { # f::f value, k::degrees of freedom, N::number of observations
     
     eta<-0
     eta<-(f*(k-1))/((f*(k-1))+(N-k))
     
     eta
     
}

#####

#Setting work directory and loading the libraries 

setwd("C:/Users/ru25tas/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking")

setwd("C:/Users/Mauro/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking")

library("car")
library("lsr")
library("psych")
library("gplots")
library("ggplot2")

# Process

lolexpexp<-read.table("160530DotaExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=F)
dotaexp<-read.table("160530LolExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=F)

#lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))

#dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))

group<-c(lolexpexp$GroupByTier,dotaexp$GroupByElo)

lolvsdota<-merge(lolexpexp,dotaexp, all=T)

for (i in 1:4) {
     lolvsdota<-lolvsdota[,-length(lolvsdota)]
}

lolvsdota$Group<-factor(lolvsdota[,"Group"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H","I", "J", "K", "L", "M","N"))

# MANOVA Analysis for LoL and DOTA groups accounting for all the variables

vsManova<-manova(cbind(FixSecGame,FixSecMM,SacSecGame,SacSecMM,MeanDurationFixGame,MeanDurationFixMM) ~ Group, data=lolvsdota)

summary(vsManova, test="Wilks")
summary.aov(vsManova, test="Wilks")
#sink()

eta<-etasq(1.090,9,91)
eta



## Analysis of Variance Fixations per Second in Game 

lolexp<-subset(lolexpexp, select=c("GroupByTier","FixSecGame"))
colnames(lolexp)<-c("Group","FixSecGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecGame"))
colnames(dotaex)<-c("Group","FixSecGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$FixSecGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta



## Analysis of Variance Fixations per Second minimap

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



## Analysis of variance Sacades per second game

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



## Analysis of variance Sacades per second minimap

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


## Analysis of variance for Mean Duration of Fixations Game


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



## Analysis of variance for Mean Duration of Fixations minimap

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

