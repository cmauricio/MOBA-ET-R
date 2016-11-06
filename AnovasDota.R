# Different Anova anayses for the eye tracking data for DOTA2 data

# etaSquared function based on the F value, degrees of freedom, and number of Observations

etasq<-function (f,k,N) { # f::f value, k::number of groups, N::number of observations
     
     eta<-0
     eta<-(f*(k-1))/((f*(k-1))+(N-k))
     
     eta
     
}

library("car")
library("lsr")


# Process

#Assigning the work directory to the variable wd
wd<-"C:/Users/ru25tas/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking"

#setting the work directory
setwd(wd)

#Importing the DOTA2 data without outliers into the variable dotaexp
dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

#Transforming the variable 'groupByElo' into ordered factors
dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M", "N"))



# Analysis for DOTA2 Experts between expertise groups

## Analysis of Variance of Fixations per second on game

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecGame"))

dotaaov<-aov(dotaex$FixSecGame~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(loldotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecGame~dotaex$GroupByElo)



## Analysis of variance of Fixations per second on minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecMM"))

dotaaov<-aov(dotaex$FixSecMM~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(dotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$FixSecMM~dotaex$GroupByElo)



## Analysis of variance of Saccades per second on game

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecGame"))

dotaaov<-aov(dotaex$SacSecGame~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(dotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecGame~dotaex$GroupByElo)



## Analysis of Variance of Saccades per second on minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecMM"))

dotaaov<-aov(dotaex$SacSecMM~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(dotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$SacSecMM~dotaex$GroupByElo)


## Analysis of variance for Mean Duration of fixations on Game

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixGame"))

dotaaov<-aov(dotaex$MeanDurationFixGame~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(dotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixGame~dotaex$GroupByElo)



## Analysis of variance for Mean duration of fixations on minimap

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixMM"))

dotaaov<-aov(dotaex$MeanDurationFixMM~dotaex$GroupByElo)
summary(dotaaov)

dotaeta<-etaSquared(dotaaov)
dotaeta

TukeyHSD(loldotaaov)

boxplot(dotaex$MeanDurationFixMM~dotaex$GroupByElo)

