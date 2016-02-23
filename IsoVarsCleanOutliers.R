# Isolating variables and cleaning outliers

lolexpexp<-read.table("160223lolExpExpCleanRatios.csv",header=T,sep=";",dec=",")

lolexpexp<-subset(x=lolexpexp, select=c("Registry", "Participant", "Session",
                                      "Age", "Gender", "Group", "Expertise",
                                      "Condition", "Game", "Tier", "Division",
                                      "GroupByTier", "Outcome", "NrFixNOMM",
                                      "MeanDurationFixGame", "SDDurationFixGame", "NrFixationsMM", 
                                      "MeanDurationFixMM", "SDDurationFixMM", "SaccadesMap",
                                      "SaccadesMM", "SurfaceVisGame.secs.", "SacFixRatioMap", "SacFixRatioMM",
                                      "FixSecGame", "FixSecMM", "SacSecGame", "SacSecMM"))

lolexpexp$NrFixRatio<-lolexpexp$NrFixNOMM/lolexpexp$NrFixationsMM
lolexpexp$NrSacRatio<-lolexpexp$SaccadesMap/lolexpexp$SaccadesMM

lolexpexp$MeanDurationFixGame<-clean.outliers(lolexpexp, "MeanDurationFixGame")
lolexpexp$SDDurationFixGame<-clean.outliers(lolexpexp, "SDDurationFixGame")
lolexpexp$MeanDurationFixMM<-clean.outliers(lolexpexp, "MeanDurationFixMM")
lolexpexp$SDDurationFixMM<-clean.outliers(lolexpexp, "SDDurationFixMM")
lolexpexp$SacFixRatioMap<-clean.outliers(lolexpexp, "SacFixRatioMap")
lolexpexp$SacFixRatioMM<-clean.outliers(lolexpexp, "SacFixRatioMM")
lolexpexp$FixSecGame<-clean.outliers(lolexpexp, "FixSecGame")
lolexpexp$FixSecMM<-clean.outliers(lolexpexp, "FixSecMM")
lolexpexp$SacSecGame<-clean.outliers(lolexpexp, "SacSecGame")
lolexpexp$SacSecMM<-clean.outliers(lolexpexp, "SacSecMM")
lolexpexp$NrFixRatio<-clean.outliers(lolexpexp, "NrFixRatio")
lolexpexp$NrSacRatio<-clean.outliers(lolexpexp, "NrSacRatio")

write.table(lolexpexp, "160223lolExpExpCleanOutliers.csv", sep=";", dec=",")




lolexpnov<-read.table("160223lolExpNovCleanRatios.csv",header=T,sep=";",dec=",")

lolexpnov<-subset(lolexpnov, select=c("Registry", "Participant", "Session",
                               "Age", "Gender", "Group", "Expertise",
                               "Condition", "Game", "Tier", "Division",
                               "GroupByTier", "Outcome", "NrFixNOMM",
                               "MeanDurationFixGame", "SDDurationFixGame", "NrFixationsMM", 
                               "MeanDurationFixMM", "SDDurationFixMM", "SaccadesMap",
                               "SaccadesMM", "SacFixRatioMap", "SacFixRatioMM",
                               "FixSecGame", "FixSecMM", "SacSecGame", "SacSecMM"))

lolexpnov$NrFixRatio<-lolexpnov$NrFixNOMM/lolexpnov$NrFixationsMM
lolexpnov$NrSacRatio<-lolexpnov$SaccadesMap/lolexpnov$SaccadesMM

lolexpnov$MeanDurationFixGame<-clean.outliers(lolexpnov, "MeanDurationFixGame")
lolexpnov$SDDurationFixGame<-clean.outliers(lolexpnov, "SDDurationFixGame")
lolexpnov$MeanDurationFixMM<-clean.outliers(lolexpnov, "MeanDurationFixMM")
lolexpnov$SDDurationFixMM<-clean.outliers(lolexpnov, "SDDurationFixMM")
lolexpnov$SacFixRatioMap<-clean.outliers(lolexpnov, "SacFixRatioMap")
lolexpnov$SacFixRatioMM<-clean.outliers(lolexpnov, "SacFixRatioMM")
lolexpnov$FixSecGame<-clean.outliers(lolexpnov, "FixSecGame")
lolexpnov$FixSecMM<-clean.outliers(lolexpnov, "FixSecMM")
lolexpnov$SacSecGame<-clean.outliers(lolexpnov, "SacSecGame")
lolexpnov$SacSecMM<-clean.outliers(lolexpnov, "SacSecMM")
lolexpnov$NrFixRatio<-clean.outliers(lolexpnov, "NrFixRatio")
lolexpnov$NrSacRatio<-clean.outliers(lolexpnov, "NrSacRatio")

write.table(lolexpnov, "160223lolExpNovCleanOutliers.csv", sep=";", dec=",")




lolnov<-read.table("160223lolNovCleanRatios.csv",header=T,sep=";",dec=",")

lolnov<-subset(lolnov, select=c("Registry", "Participant", "Session",
                               "Age", "Gender", "Group", "Expertise",
                               "Condition", "Game", "Outcome", "NrFixNOMM",
                               "MeanDurationFixGame", "SDDurationFixGame", "NrFixationsMM", 
                               "MeanDurationFixMM", "SDDurationFixMM", "SaccadesMap",
                               "SaccadesMM", "SacFixRatioMap", "SacFixRatioMM",
                               "FixSecGame", "FixSecMM", "SacSecGame", "SacSecMM"))

lolnov$NrFixRatio<-lolnov$NrFixNOMM/lolnov$NrFixationsMM
lolnov$NrSacRatio<-lolnov$SaccadesMap/lolnov$SaccadesMM

lolnov$MeanDurationFixGame<-clean.outliers(lolnov, "MeanDurationFixGame")
lolnov$SDDurationFixGame<-clean.outliers(lolnov, "SDDurationFixGame")
lolnov$MeanDurationFixMM<-clean.outliers(lolnov, "MeanDurationFixMM")
lolnov$SDDurationFixMM<-clean.outliers(lolnov, "SDDurationFixMM")
lolnov$SacFixRatioMap<-clean.outliers(lolnov, "SacFixRatioMap")
lolnov$SacFixRatioMM<-clean.outliers(lolnov, "SacFixRatioMM")
lolnov$FixSecGame<-clean.outliers(lolnov, "FixSecGame")
lolnov$FixSecMM<-clean.outliers(lolnov, "FixSecMM")
lolnov$SacSecGame<-clean.outliers(lolnov, "SacSecGame")
lolnov$SacSecMM<-clean.outliers(lolnov, "SacSecMM")
lolnov$NrFixRatio<-clean.outliers(lolnov, "NrFixRatio")
lolnov$NrSacRatio<-clean.outliers(lolnov, "NrSacRatio")

write.table(lolnov, "160223lolNovCleanOutliers.csv", sep=";", dec=",")




dotaexp<-read.table("160223dotaExpNovCleanRatios.csv",header=T,sep=";",dec=",")

dotaexp<-subset(dotaexp, select=c("Registry", "Participant", "Session",
                               "Age", "Gender", "Group", "Expertise",
                               "Condition", "Game", "Elo", "GroupByElo", "Outcome", "NrFixNOMM",
                               "MeanDurationFixGame", "SDDurationFixGame", "NrFixationsMM", 
                               "MeanDurationFixMM", "SDDurationFixMM", "SaccadesMap",
                               "SaccadesMM", "SacFixRatioMap", "SacFixRatioMM",
                               "FixSecGame", "FixSecMM", "SacSecGame", "SacSecMM"))

dotaexp$NrFixRatio<-dotaexp$NrFixNOMM/dotaexp$NrFixationsMM
dotaexp$NrSacRatio<-dotaexp$SaccadesMap/dotaexp$SaccadesMM

dotaexp$MeanDurationFixGame<-clean.outliers(dotaexp, "MeanDurationFixGame")
dotaexp$SDDurationFixGame<-clean.outliers(dotaexp, "SDDurationFixGame")
dotaexp$MeanDurationFixMM<-clean.outliers(dotaexp, "MeanDurationFixMM")
dotaexp$SDDurationFixMM<-clean.outliers(dotaexp, "SDDurationFixMM")
dotaexp$SacFixRatioMap<-clean.outliers(dotaexp, "SacFixRatioMap")
dotaexp$SacFixRatioMM<-clean.outliers(dotaexp, "SacFixRatioMM")
dotaexp$FixSecGame<-clean.outliers(dotaexp, "FixSecGame")
dotaexp$FixSecMM<-clean.outliers(dotaexp, "FixSecMM")
dotaexp$SacSecGame<-clean.outliers(dotaexp, "SacSecGame")
dotaexp$SacSecMM<-clean.outliers(dotaexp, "SacSecMM")
dotaexp$NrFixRatio<-clean.outliers(dotaexp, "NrFixRatio")
dotaexp$NrSacRatio<-clean.outliers(dotaexp, "NrSacRatio")

write.table(dotaexp, "160223dotaExpCleanOutliers.csv", sep=";", dec=",")
