


library("corrgram")

#


lolexpnov<-read.table("160223lolExpNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
lolnov<-read.table("160223lolNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolnov<-subset(lolnov,Condition=="novice")

lolexpnov$Tier<-factor(lolexpnov[,"Tier"], order=T, levels=c("novice","bronce", "silver", "gold", "platinum", "diamond"))

lolnov$Tier<-"novice"
lolnov$Tier<-factor(lolnov[,"Tier"], order=T, levels=c("novice","bronce", "silver", "gold", "platinum", "diamond"))

lolexpnov$GroupByTier<-factor(lolexpnov[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H", "I"))

lolnov$GroupByTier<-"I"
lolnov$GroupByTier<-factor(lolnov[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H","I"))

lolexpVSnov<-merge(lolexpnov,lolnov,all=T)

#

a<-C(lolexpVSnov$Tier)

lolexpVSnov$Dummy<-as.integer(a)

png(filename="Lolexpnov.png",width=800, height = 600, units = "px")
corrgram(lolexpVSnov, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
dev.off()
