# agregating four new variables to the study

lolexpexp<-read.table("160222lolExpExpClean.csv",header=T,sep=";",dec=",")

lolexpexp$FixSecGame<-lolexpexp$NrFixationsGame/lolexpexp$SurfaceVisGame.secs.
lolexpexp$FixSecMM<-lolexpexp$NrFixationsMM/lolexpexp$SurfaceVisGame.secs.
lolexpexp$SacSecGame<-lolexpexp$SaccadesMap/lolexpexp$SurfaceVisGame.secs.
lolexpexp$SacSecMM<-lolexpexp$SaccadesMM/lolexpexp$SurfaceVisGame.secs.

write.table(lolexpexp, "160223lolExpExpCleanRatios.csv", sep=";", dec=",")

lolexpnov<-read.table("160222lolExpNovClean.csv",header=T,sep=";",dec=",")

lolexpnov$FixSecGame<-lolexpnov$NrFixationsGame/lolexpnov$SurfaceVisGame.secs.
lolexpnov$FixSecMM<-lolexpnov$NrFixationsMM/lolexpnov$SurfaceVisGame.secs.
lolexpnov$SacSecGame<-lolexpnov$SaccadesMap/lolexpnov$SurfaceVisGame.secs.
lolexpnov$SacSecMM<-lolexpnov$SaccadesMM/lolexpnov$SurfaceVisGame.secs.

write.table(lolexpnov, "160223lolExpNovCleanRatios.csv", sep=";", dec=",")

lolnov<-read.table("160222lolNovClean.csv",header=T,sep=";",dec=",")

lolnov$FixSecGame<-lolnov$NrFixationsGame/lolnov$SurfaceVisGame.secs.
lolnov$FixSecMM<-lolnov$NrFixationsMM/lolnov$SurfaceVisGame.secs.
lolnov$SacSecGame<-lolnov$SaccadesMap/lolnov$SurfaceVisGame.secs.
lolnov$SacSecMM<-lolnov$SaccadesMM/lolnov$SurfaceVisGame.secs.

write.table(lolnov, "160223lolNovCleanRatios.csv", sep=";", dec=",")

dotaexp<-read.table("160222dotaExpClean.csv",header=T,sep=";",dec=",")

dotaexp$FixSecGame<-dotaexp$NrFixationsGame/dotaexp$SurfaceVisGame.secs.
dotaexp$FixSecMM<-dotaexp$NrFixationsMM/dotaexp$SurfaceVisGame.secs.
dotaexp$SacSecGame<-dotaexp$SaccadesMap/dotaexp$SurfaceVisGame.secs.
dotaexp$SacSecMM<-dotaexp$SaccadesMM/dotaexp$SurfaceVisGame.secs.

write.table(dotaexp, "160223dotaExpNovCleanRatios.csv", sep=";", dec=",")
