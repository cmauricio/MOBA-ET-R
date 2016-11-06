# MANOVA analyses for DOTA expertise groups, including post-hoc ANOVAs
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


# Process

dotaexp<-read.table("160530DotaExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M", "N"))


## MANOVA Analysis for DOTA groups accounting for all the variables

dotaManova<-manova(cbind(FixSecGame,FixSecMM,SacSecGame,SacSecMM,MeanDurationFixGame,MeanDurationFixMM) ~ GroupByElo, data=dotaexp)
summary(dotaManova, "Wilks")
summary.aov(dotaManova, test="Wilks")

dotaManova

eta<-etasq(1.056,4,39)
eta



## Analysis of Variance Fixations per Second in Game 

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecGame"))

loldotaaov<-aov(dotaex$FixSecGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaaov

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecGame~dotaex$GroupByElo)

plot(factor(dotaexp$Group),dotaexp$SacSecGame, col=c("light blue","light blue","light blue", "light blue","light blue", "white", "white", "white", "white","white"))
lines(lowess(factor(dotaexp$Group),dotaexp$SacSecGame, f=1/5), col="red")



## Analysis of Variance Fixations per Second minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecMM"))

loldotaaov<-aov(dotaex$FixSecMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecMM~dotaex$GroupByElo)



## Analysis of variance Sacades per second game

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecGame"))

loldotaaov<-aov(dotaex$SacSecGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecGame~dotaex$GroupByElo)



## Analysis of variance Sacades per second minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecMM"))

loldotaaov<-aov(dotaex$SacSecMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecMM~dotaex$GroupByElo)



## Analysis of variance for Mean Duration of Fixations Game

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixGame"))

loldotaaov<-aov(dotaex$MeanDurationFixGame~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixGame~dotaex$GroupByElo)



## Analysis of variance for Mean Duration of Fixations minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixMM"))

loldotaaov<-aov(dotaex$MeanDurationFixMM~dotaex$GroupByElo)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixMM~dotaex$GroupByElo)

