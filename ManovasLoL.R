# MANOVA analyses for LOL and DOTA expertise groups, including post-hoc ANOVAs
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
library("gplots")


# Process

lolexpexp<-read.table("160530LolExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"))



## MANOVA Analysis for LoL groups accounting for all the variables

lolManova<-manova(cbind(FixSecGame,FixSecMM,SacSecGame,SacSecMM,MeanDurationFixGame,MeanDurationFixMM) ~ GroupByTier, data=lolexpexp)

summary(lolManova, test = "Wilks")
summary.aov(lolManova, test=adjusted("Bonferroni"))

eta<-etasq(1.056,4,44)
eta



## Analysis of Variance Fixations per Second in Game 

lolexex2aov<-aov(lolexpexp$FixSecGame~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$FixSecGame~lolexpexp$GroupByTier)

plotmeans(lolexpexp$FixSecGame~lolexpexp$GroupByTier, digits=2, ccol="red", mean.labels=T, main="Plot")

plot(factor(lolexpexp$GroupByTier),lolexpexp$FixSecGame, col=c("light blue","light blue","light blue", "light blue","light blue"))
lines(lowess(factor(lolexpexp$GroupByTier),lolexpexp$FixSecGame, f=1/5), col="red")



## Analysis of Variance Fixations per Second minimap

lolexex2aov<-aov(lolexpexp$FixSecMM~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$FixSecMM~lolexpexp$GroupByTier)




## Analysis of variance Sacades per second game

lolexex2aov<-aov(lolexpexp$SacSecGame~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$SacSecGame~lolexpexp$GroupByTier)



## Analysis of variance Sacades per second minimap

lolexex2aov<-aov(lolexpexp$SacSecMM~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$SacSecMM~lolexpexp$GroupByTier)


## Analysis of variance for Mean Duration of Fixations Game

lolexex2aov<-aov(lolexpexp$MeanDurationFixGame~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$MeanDurationFixGame~lolexpexp$GroupByTier)



## Analysis of variance for Mean Duration of Fixations minimap


lolexex2aov<-aov(lolexpexp$MeanDurationFixMM~lolexpexp$GroupByTier)
summary(lolexex2aov)

etaSquared(lolexex2aov)

TukeyHSD(lolexex2aov)

boxplot(lolexpexp$MeanDurationFixMM~lolexpexp$GroupByTier)
