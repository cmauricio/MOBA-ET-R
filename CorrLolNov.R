library("corrgram")

lolnov<-read.table("160223lolNovCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolnovnov<-subset(lolnov,Condition=="novice"|Condition=="evaluation1"|Condition=="evaluation2")

lolnovnov$Condition<-factor(lolnovnov[,"Condition"], ordered = T, levels = c("novice","evaluation1","evaluation2"))


a<-C(lolnovnov$Condition)

lolnovnov$Dummy<-as.integer(a)

png(filename="Lolnovnov.png",width=800, height = 600, units = "px")
corrgram(lolnovnov, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
dev.off()
