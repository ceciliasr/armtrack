setwd("c:/TFM/Data")
dd<-read.mat("DadesVideoCecilia.mat")
load("BDistrophyD.rda")

dd1<-data.frame(dd$TimeStamp,dd$ang,dd$LElbAngle,dd$RElbAngle,dd$LSAngle,dd$RSAngle,dd$RHandPos[,1],dd$RHandPos[,2],dd$RHandPos[,3],dd$LHandPos[,1],dd$LHandPos[,2],dd$LHandPos[,3],dd$RElbPos[,1],dd$RElbPos[,2],dd$RElbPos[,3],dd$LElbPos[,1],dd$LElbPos[,2],dd$LElbPos[,3])

names(dd1)<-names(BMD)[1:18]
save(dd1,file="HealthyVideo.rda")
