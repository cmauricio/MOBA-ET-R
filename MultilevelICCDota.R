# Multilevel analyses for DOTA data
# These analysis were not included in the thesis or the article

# loading libraries for leveneTest and etaSquared

library("lme4")

# Process
wDir<-"C:/Users/Mauro/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking"

setwd(wDir)

DotaExp<-read.table("160524DotaExp-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

DotaExp$GroupByElo<-factor(DotaExp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))

group<-as.data.frame(cbind(Registry=DotaExp$Registry,Dyad=DotaExp$Dyad,Group=DotaExp$Group,MeanDurationFixGame=DotaExp$MeanDurationFixGame,MeanDurationFixMM=DotaExp$MeanDurationFixMM,FixSecGame=DotaExp$FixSecGame,FixSecMM=DotaExp$FixSecMM,SacSecGame=DotaExp$SacSecGame,SacSecMM=DotaExp$SacSecMM))


l0<-lmer(MeanDurationFixGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l0)
(0.0000)/(0.0004+0.0000)

l1<-lmer(MeanDurationFixMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l1)
(0.3068)/(2.7823+0.3068)

l2<-lmer(FixSecGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l2)
(0.0211)/(0.0860+0.0211)

l3<-lmer(FixSecMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l3)
(0.0018)/(0.0073+0.0018)

l4<-lmer(SacSecGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l4)
(1.4160)/(3.3670+1.4160)

l5<-lmer(SacSecMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l5)
(0.0923)/(0.3764+0.0923)



a<-C(DotaExp$Group)

DotaExp$Dummy<-as.integer(a)

#png(filename="Lolexpexp.png",width=800, height = 600, units = "px")
corrgram(DotaExp, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
#dev.off()


