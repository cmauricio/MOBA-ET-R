library("corrgram")

#

lolexpexp<-read.table("160223lolExpNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)
dotaexp<-read.table("160223dotaExpCleanOutliers2.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolexpexp$Group<-lolexpexp$GroupByTier
lolexpexp$Group<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))

dotaexp$Group<-dotaexp$GroupByElo
dotaexp$Group<-factor(dotaexp[,"GroupByElo"], order=T, levels=c("I", "J", "K", "L", "M"))

lolVSdota<-merge(lolexpexp,dotaexp,all=T)

#
#


a<-C(lolVSdota$Group)

lolVSdota$Dummy<-as.integer(a)

lolVSdota<-lolVSdota[,-33:-36]

png(filename="LolVSdota.png",width=800, height = 600, units = "px")
corrgram(lolVSdota, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
dev.off()
