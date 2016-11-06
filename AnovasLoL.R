# Different Anova anayses for the eye tracking data for LOL data

# etaSquared function based on the F value, degrees of freedom, and number of Observations

etasq<-function (f,k,N) { # f::f value, k::degrees of freedom, N::number of observations
     
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

#Importing the LOL data without outliers into the variable lolexpexp
lolexpexp<-read.table("160223lolExpExpCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

#Transforming the variables 'Tier' and 'groupByTier' into ordered factors
lolexpexp$Tier<-factor(lolexpexp[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))
lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))



# Analysis for LOL Experts between expertise groups

## Analysis of Variance of Fixations per second on game

lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","FixSecGame"))

lolexex2aov<-aov(lolexexGroupMDFG$FixSecGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$FixSecGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$FixSecGame~lolexexGroupMDFG$GroupByTier)




## Analysis of variance of Fixations per secon on minimap

lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","FixSecMM"))

lolexex2aov<-aov(lolexexGroupMDFG$FixSecMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$FixSecMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$FixSecMM~lolexexGroupMDFG$GroupByTier)




## Analysis of variance of Saccades per second on game

lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacSecGame"))

lolexex2aov<-aov(lolexexGroupMDFG$SacSecGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacSecGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacSecGame~lolexexGroupMDFG$GroupByTier)



## Analysis of Variance of Saccades per second on minimap

lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","SacSecMM"))

lolexex2aov<-aov(lolexexGroupMDFG$SacSecMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$SacSecMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$SacSecMM~lolexexGroupMDFG$GroupByTier)


## Analysis of variance for Mean Duration of fixations on Game


lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixGame"))

lolexex2aov<-aov(lolexexGroupMDFG$MeanDurationFixGame~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$MeanDurationFixGame,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$MeanDurationFixGame~lolexexGroupMDFG$GroupByTier)



## Analysis of variance for Mean duration of fixations on minimap

lolexexGroupMDFG<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixMM"))

lolexex2aov<-aov(lolexexGroupMDFG$MeanDurationFixMM~lolexexGroupMDFG$GroupByTier)
summary(lolexex2aov)

lolexexeta<-etaSquared(lolexex2aov)

leveneTest(lolexexGroupMDFG$MeanDurationFixMM,lolexexGroupMDFG$GroupByTier,center=mean)

TukeyHSD(lolexex2aov)

boxplot(lolexexGroupMDFG$MeanDurationFixMM~lolexexGroupMDFG$GroupByTier)

