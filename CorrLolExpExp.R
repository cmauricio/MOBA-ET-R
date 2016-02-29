# Different Anova anayses for the eye tracking data

# loading libraries for leveneTest and etaSquared

library("corrgram")


# Process

lolexpexp<-read.table("160223lolExpExpCleanOutliers.csv",header=T,sep=";",dec=",", stringsAsFactors=T)

lolexpexp$Tier<-factor(lolexpexp[,"Tier"], order=T, levels=c("bronce", "silver", "gold", "platinum", "diamond"))

lolexpexp$GroupByTier<-factor(lolexpexp[,"GroupByTier"], order=T, levels=c("A", "B", "C", "D", "E", "F", "G", "H"))

# Analysis for LoL Experts between expertise groups


a<-C(lolexpexp$Tier)

lolexpexp$Dummy<-as.integer(a)

png(filename="Lolexpexp.png",width=800, height = 600, units = "px")
corrgram(lolexpexp, order=F, lower.panel=panel.shade, upper.panel = panel.pie, cor.method = "pearson")
dev.off()


# Analysis for MeanDurationFix Game

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))


# Analysis MeanDurationFixMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))


# Analysis SacFixRatioMap

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis SacFixRatioMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis FixSecGame

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis FixSecMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis SacSecGame

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis SacSecMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis NrFixRatio

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))



# Analysis NrSacRatio

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis PercFixGame

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis PercFixMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis PercSacGame

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))




# Analysis PercSacMM

tiercor<-cor(lolexpexp$SacFixRatioMap)

b<-lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier)
b
summary(b)

plot(lolexpexp$SacFixRatioMap, col=lolexpexp$Tier)
abline(lm(lolexpexp$SacFixRatioMap~lolexpexp$Tier))
