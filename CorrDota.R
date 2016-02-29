library("corrgram")

#

dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

dotaexp$Group<-dotaexp$GroupByElo
dotaexp$Group<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))

#
#


a<-C(dotaexp$GroupByElo)

dotaexp$Dummy<-as.integer(a)

png(filename="dota.png",width=800, height = 600, units = "px")
corrgram(dotaexp, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
dev.off()
