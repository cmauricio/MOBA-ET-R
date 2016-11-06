# Different Anova anayses for the eye tracking data for LOL and DOTA2 data

# etaSquared function based on the F value, degrees of freedom, and number of Observations

etasq<-function (f,k,N) { # f::f value, k::degrees of freedom, N::number of observations
     
     eta<-0
     eta<-(f*(k-1))/((f*(k-1))+(N-k))
     
     eta
     
}

library("car")
library("lsr")
library("ggplot2")


# Process

#Assigning the work directory to the variable wd
wd<-"C:/Users/ru25tas/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking"

#setting the work directory
setwd(wd)

#Importing the LOL and DOTA2 databases without outliers into the variables lolexpexp and dotaexp
lolexpexp<-read.table("160223lolExpNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

#Transforming the variables 'GroupByTier' and 'GroupByElo' into ordered factors
lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))
dotaexp$GroupByElo<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M","N"))



# Analysis between expertise groups for LoL and DOTA2


#Subsetting the common variables from LoL and DOTA databases
lolexp<-subset(lolexpexp, select=c("Game","GroupByTier","FixSecGame"))
colnames(lolexp)<-c("Game","Group","FixSecGame")

dotaex<-subset(dotaexp, select=c("Game","GroupByElo","FixSecGame"))
colnames(dotaex)<-c("Game","Group","FixSecGame")

#Combining LoL and Dota Data
lolVSdota<-rbind(lolexp,dotaex)

## Analysis of Variance of Fixations per second on game
loldotaaov<-aov(lolVSdota$FixSecGame~lolVSdota$Group)
summary(loldotaaov)

#getting the etasquared from the analysis of variance
loldotaeta<-etaSquared(loldotaaov)
loldotaeta

TukeyHSD(loldotaaov)

# basic box plots
boxplot(lolVSdota$FixSecGame~lolVSdota$Group, xlab="Groups", ylab="Difference")
lo<-loess(lolVSdota$FixSecGame~as.numeric(lolVSdota$Group), span=0.5)
lines(predict(lo), col="red", lwd=1)


# Advanced graphs with ggplot to see the diffences between groups of expertise in LOL and DOTA2
lolfixsec<-ggplot(lolexp, aes(Group,FixSecGame))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecGame))+
     theme_classic()+
     ylim(0,2.2)+
     xlim("A","B","C","D","E","F","G","H")



dotafixsec<-ggplot(dotaex, aes(Group,FixSecGame))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecGame))+
     theme_classic()


png("fixsecmap.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Fixations per Second (game)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecGame))+
     theme_classic()+
     ylim(1,2.2)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Fixations per Second (game)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecGame))+
     theme_classic()+
     ylim(1,2.2)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()


## Analysis of variance of Fixations per second on minimap

lolexp<-subset(lolexpexp, select=c("GroupByTier","FixSecMM"))
colnames(lolexp)<-c("Group","FixSecMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","FixSecMM"))
colnames(dotaex)<-c("Group","FixSecMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$FixSecMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

leveneTest(lolVSdota$FixSecMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$FixSecMM~lolVSdota$Group)



lolfixsec<-ggplot(lolexp, aes(Group,FixSecMM))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group)-2,FixSecMM))+
     theme_classic()

dotafixsec<-ggplot(dotaex, aes(Group,FixSecMM))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecMM))+
     theme_classic()

png("fixsecmm.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Fixations per Second (minimap)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecMM))+
     theme_classic()+
     ylim(-0.05,0.3)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Fixations per Second (minimap)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),FixSecMM))+
     theme_classic()+
     ylim(-0.05,0.3)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()

## Analysis of variance of Saccades per second on game

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacSecGame"))
colnames(lolexp)<-c("Group","SacSecGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecGame"))
colnames(dotaex)<-c("Group","SacSecGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacSecGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

leveneTest(lolVSdota$SacSecGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacSecGame~lolVSdota$Group)



lolfixsec<-ggplot(lolexp, aes(Group,SacSecGame))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group)-2,SacSecGame))+
     theme_classic()

dotafixsec<-ggplot(dotaex, aes(Group,SacSecGame))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecGame))+
     theme_classic()

png("sacsecmap.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Saccades per Second (game)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecGame))+
     theme_classic()+
     ylim(5,15)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Saccades per Second (game)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecGame))+
     theme_classic()+
     ylim(5,15)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()

## Analysis of Variance of Saccades per second on minimap

lolexp<-subset(lolexpexp, select=c("GroupByTier","SacSecMM"))
colnames(lolexp)<-c("Group","SacSecMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","SacSecMM"))
colnames(dotaex)<-c("Group","SacSecMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$SacSecMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

leveneTest(lolVSdota$SacSecMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$SacSecMM~lolVSdota$Group)




lolfixsec<-ggplot(lolexp, aes(Group,SacSecMM))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group)-1,SacSecMM))+
     theme_classic()

dotafixsec<-ggplot(dotaex, aes(Group,SacSecMM))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecMM))+
     theme_classic()

png("sacsecmm.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Saccades per Second (minimap)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecMM))+
     theme_classic()+
     ylim(-0.15,0.6)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Saccades per Second (minimap)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),SacSecMM))+
     theme_classic()+
     ylim(-0.15,0.6)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()


## Analysis of variance for Mean Duration of fixations on Game

lolexp<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixGame"))
colnames(lolexp)<-c("Group","MeanDurationFixGame")

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixGame"))
colnames(dotaex)<-c("Group","MeanDurationFixGame")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$MeanDurationFixGame~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

leveneTest(lolVSdota$MeanDurationFixGame,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$MeanDurationFixGame~lolVSdota$Group)



lolfixsec<-ggplot(lolexp, aes(Group,MeanDurationFixGame))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group)-1,MeanDurationFixGame))+
     theme_classic()

dotafixsec<-ggplot(dotaex, aes(Group,MeanDurationFixGame))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixGame))+
     theme_classic()

png("meandurfixmap.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Mean Duration of Fixation (game)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixGame))+
     theme_classic()+
     ylim(0.200,0.320)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Mean Duration of Fixation (game)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixGame))+
     theme_classic()+
     ylim(0.200,0.320)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()


## Analysis of variance for Mean duration of fixations on minimap

lolexp<-subset(lolexpexp, select=c("GroupByTier","MeanDurationFixMM"))
colnames(lolexp)<-c("Group","MeanDurationFixMM")

dotaex<-subset(dotaexp, select=c("GroupByElo","MeanDurationFixMM"))
colnames(dotaex)<-c("Group","MeanDurationFixMM")


lolVSdota<-rbind(lolexp,dotaex)


loldotaaov<-aov(lolVSdota$MeanDurationFixMM~lolVSdota$Group)
summary(loldotaaov)

loldotaeta<-etaSquared(loldotaaov)
loldotaeta

leveneTest(lolVSdota$MeanDurationFixMM,lolVSdota$Group,center=mean)

TukeyHSD(loldotaaov)

boxplot(lolVSdota$MeanDurationFixMM~lolVSdota$Group)



lolfixsec<-ggplot(lolexp, aes(Group,MeanDurationFixMM))
lolfixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group)-2,MeanDurationFixMM))+
     theme_classic()

dotafixsec<-ggplot(dotaex, aes(Group,MeanDurationFixMM))
dotafixsec+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixMM))+
     theme_classic()

png("meandurfixmm.png",
    width=21, 
    height=10, 
    units= "cm", 
    res=300)

require(gridExtra)
plot1 <- lolfixsec+
     ylab("Mean Duration of Fixation (minimap)")+
     ggtitle("LoL")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixMM))+
     theme_classic()+
     ylim(-1,9)+
     xlim("A","B","C","D","E","F","G","H")
plot2 <- dotafixsec+
     ylab("Mean Duration of Fixation (minimap)")+
     ggtitle("DotA2")+
     geom_boxplot()+
     geom_smooth(aes(as.numeric(Group),MeanDurationFixMM))+
     theme_classic()+
     ylim(-1,9)+
     xlim("I","J","K","L","M","N")
grid.arrange(plot1, plot2, ncol=2)

dev.off()





