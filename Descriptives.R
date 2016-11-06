#  Code for getting the Descriptives in LoL and Dota using the package Psych

library("psych")

wDir<-"C:/Users/Mauro/Dropbox/PhD5/HowMOBAsFosterScR/Data/Eye-Tracking"

setwd(wDir)

#Import LoL and Dota data while transforming the group variables into factors
LolExp<-read.table("160530LolExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
LolExp$Group<-factor(LolExp[,"Group"], order=T, levels=c("A", "B", "C", "D", "E","F","G","H"))

DotaExp<-read.table("160530DotaExpManova-2ndStudy.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
DotaExp$GroupByElo<-factor(DotaExp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))

#Merging the two sets of data
lolvsdota<-merge(LolExp,DotaExp, all=T)

#This is a weird piece of code, I really need to review it!
for (i in 1:4) {
     lolvsdota<-lolvsdota[,-length(lolvsdota)]
}

#Getting the descriptives and exporting them into a csv file
a<-describe(na.omit(LolExp))
write.table(a,"DescriptivesLoL.csv",sep=";", dec=",")

b<-describe(na.omit(DotaExp))
write.table(b,"DescriptivesDota.csv",sep=";", dec=",")

c<-describe(na.omit(lolvsdota))
write.table(c,"DescriptivesLolnDota.csv",sep=";", dec=",")
