# Multilevel analyses for LoL data
# These analysis were not included in the thesis or the article

library("lme4")

# Process
wDir<-"C:/Users/Mauro/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking"

setwd(wDir)

LolExp<-read.table("160524LolExpExp-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

LolExp$Group<-factor(LolExp[,"Group"], order=T, levels=c("A", "B", "C", "D", "E","F","G","H"))

group<-as.data.frame(cbind(Registry=LolExp$Registry,Dyad=LolExp$Dyad,Group=LolExp$Group,MeanDurationFixGame=LolExp$MeanDurationFixGame,MeanDurationFixMM=LolExp$MeanDurationFixMM,FixSecGame=LolExp$FixSecGame,FixSecMM=LolExp$FixSecMM,SacSecGame=LolExp$SacSecGame,SacSecMM=LolExp$SacSecMM))

l0<-lmer(MeanDurationFixGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l0)
(0.0000)/(0.0000+0.0000)

l1<-lmer(MeanDurationFixMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l1)
(0.7143)/(9.4586+0.7143)

l2<-lmer(FixSecGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l2)
(0.0610)/(0.1267+0.0610)

l3<-lmer(FixSecMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l3)
(0.0003)/(0.0023+0.0003)

l4<-lmer(SacSecGame ~1+(1|Dyad), data=group) # Anova; ICC
summary(l4)
(2.4050)/(7.0940+2.4050)

l5<-lmer(SacSecMM ~1+(1|Dyad), data=group) # Anova; ICC
summary(l5)
(0.0180)/(0.1097+0.0180)




a<-C(LolExp$Group)

LolExp$Dummy<-as.integer(a)

#png(filename="Lolexpexp.png",width=800, height = 600, units = "px")
corrgram(LolExp, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
#dev.off()
